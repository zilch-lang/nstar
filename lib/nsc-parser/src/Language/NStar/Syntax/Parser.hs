{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-|
  Module: Language.NStar.Syntax.Parser
  Description: NStar's parser for a x64 syntax
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Syntax.Parser
(parseFile) where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Language.NStar.Syntax.Core
import Language.NStar.Syntax.Internal
import Text.Diagnose (Diagnostic, diagnostic, (<++>))
import Data.Bifunctor (bimap, second)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Located (Located(..), unLoc, getPos)
import qualified Data.Map as Map (fromList)
import Console.NStar.Flags (ParserFlags(..))
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Foldable (foldl')
import Language.NStar.Syntax.Errors
import Internal.Error (internalError)

type Parser a = WriterT [ParseWarning] (MP.Parsec SemanticError [LToken]) a

lexeme :: (?parserFlags :: ParserFlags) => Parser a -> Parser a
lexeme = MPL.lexeme (MPL.space whiteSpace inlineComment multilineComment)
  where inlineComment = () <$ MP.satisfy isInlineComment
        isInlineComment (InlineComment _ :@ _) = True
        isInlineComment _                      = False

        multilineComment = () <$ MP.satisfy isMultilineComment
        isMultilineComment (MultilineComment _ :@ _) = True
        isMultilineComment _                         = False

        whiteSpace = () <$ MP.satisfy isWhitespace
        isWhitespace (HSpace :@ _) = True
        isWhitespace (EOL :@ _)    = True
        isWhitespace _             = False

-------------------------------------------------------------------------------------------------

-- | Turns a list of tokens into an AST, as long as tokens are well ordered. Else, it throws an error.
parseFile :: (?parserFlags :: ParserFlags) => FilePath -> [LToken] -> Either (Diagnostic [] String Char) (Program, Diagnostic [] String Char)
parseFile file tokens = bimap (megaparsecBundleToDiagnostic "Parse error on input") (second toDiagnostic) $ MP.runParser (runWriterT parseProgram) file tokens
  where toDiagnostic = foldl' (<++>) diagnostic . fmap fromParseWarning

-- | Parses a sequence of either typed labels or instruction calls.
parseProgram :: (?parserFlags :: ParserFlags) => Parser Program
parseProgram = lexeme (pure ()) *> (Program <$> MP.many section) <* parseEOF
  where section = lexeme . located $ MP.choice [ parseInclude, MP.try parseCodeSection, MP.try parseDataSection, parseExternCodeSection ]

parseCodeSection :: (?parserFlags :: ParserFlags) => Parser Section
parseCodeSection = CodeS <$> do
  lexeme (parseSymbol Section)
  lexeme (parseSymbol (Id "code"))
  lexeme $ betweenBraces (MP.many parseTypedLabel)

parseDataSection :: (?parserFlags :: ParserFlags) => Parser Section
parseDataSection = DataS <$> do
  lexeme (parseSymbol Section)
  lexeme (parseSymbol (Id "data"))
  lexeme $ betweenBraces (MP.many (located binding))
  where binding = Bind <$> (lexeme parseIdentifier <* lexeme (parseSymbol Colon))
                       <*> (lexeme parseType <* lexeme (parseSymbol Equal))
                       <*> (lexeme parseConstant)

parseInclude :: (?parserFlags :: ParserFlags) => Parser Section
parseInclude = IncludeS <$> do
  lexeme (parseSymbol Include)
  betweenBraces $ MP.many (fmap toText <$> parseString)
  where
    toText (Str s) = s
    toText t       = internalError $ "Cannot get text of non string token " <> show t

parseExternCodeSection :: (?parserFlags :: ParserFlags) => Parser Section
parseExternCodeSection = ExternCodeS <$> do
  lexeme (parseSymbol Section)
  lexeme (parseSymbol (Id "extern"))
  lexeme (parseSymbol Dot)
  lexeme (parseSymbol (Id "code"))
  lexeme $ betweenBraces (MP.many (located binding))
  where binding = ReservedBind <$> (lexeme parseIdentifier <* lexeme (parseSymbol Colon))
                               <*> lexeme (parseForallType (parseRecordType True))

-- | Parses the end of file. There is no guarantee that any parser will try to parse something after the end of file.
--   This has to be dealt with on our own. No more token should be available after consuming the end of file.
parseEOF :: (?parserFlags :: ParserFlags) => Parser ()
parseEOF = () <$ parseSymbol EOF

-- | Parses an identifier and returns its textual representation.
parseIdentifier :: (?parserFlags :: ParserFlags) => Parser (Located Text)
parseIdentifier = MP.label "an identifier" $ lexeme do
  Id i :@ p <- MP.satisfy isIdentifier
  pure (i :@ p)
 where
   isIdentifier (Id _ :@ _) = True
   isIdentifier (_ :@ _)    = False

parseString :: (?parserFlags :: ParserFlags) => Parser LToken
parseString = MP.label "a string" $ lexeme $ MP.satisfy isString
  where isString (Str _ :@ _) = True
        isString _            = False

-- | Parses a symbol and returns it.
parseSymbol :: (?parserFlags :: ParserFlags) => Token -> Parser LToken
parseSymbol t1 = MP.label (showToken t1) $ MP.satisfy \ (t2 :@ _) -> t2 == t1

parseRegister :: (?parserFlags :: ParserFlags) => Parser (Located Register)
parseRegister = MP.label "a register" $ located $ parseSymbol Percent *> reg
  where reg = MP.choice
          [ R0 <$ parseSymbol R0'
          , R1 <$ parseSymbol R1'
          , R2 <$ parseSymbol R2'
          , R3 <$ parseSymbol R3'
          , R4 <$ parseSymbol R4'
          , R5 <$ parseSymbol R5'
          , MP.lookAhead MP.anySingle >>= MP.customFailure . NoSuchRegister . unLoc ]

parseInteger :: (?parserFlags :: ParserFlags) => Parser (Located Integer)
parseInteger = MP.label "an integer" $ lexeme do
  Integer i :@ p <- MP.satisfy isInteger
  pure (read (Text.unpack i) :@ p)
 where
   isInteger (Integer _ :@ _) = True
   isInteger _                = False

parseCharacter :: (?parserFlags :: ParserFlags) => Parser (Located Char)
parseCharacter = MP.label "a character" $ lexeme do
  Char c :@ p <- MP.satisfy isCharacter
  pure (c :@ p)
 where
   isCharacter (Char _ :@ _) = True
   isCharacter _             = False

-- | Parses something between braces.
betweenBraces :: (?parserFlags :: ParserFlags) => Parser a -> Parser a
betweenBraces = MP.between (lexeme $ parseSymbol LBrace) (parseSymbol RBrace)

-- | Parses something between brackets.
betweenBrackets :: (?parserFlags :: ParserFlags) => Parser a -> Parser a
betweenBrackets = MP.between (lexeme $ parseSymbol LBracket) (parseSymbol RBracket)

-- | Parses something between parentheses.
betweenParens :: (?parserFlags :: ParserFlags) => Parser a -> Parser a
betweenParens = MP.between (lexeme $ parseSymbol LParen) (parseSymbol RParen)

-- | Parses something between angles.
betweenAngles :: (?parserFlags :: ParserFlags) => Parser a -> Parser a
betweenAngles = MP.between (lexeme $ parseSymbol LAngle) (parseSymbol RAngle)


-----------------------------------

-- | Parses a typed label.
parseTypedLabel :: (?parserFlags :: ParserFlags) => Parser (Located Statement)
parseTypedLabel = lexeme . located $
  Label <$> (lexeme parseIdentifier <* lexeme (parseSymbol Colon))
        <*> lexeme (parseForallType (parseRecordType True))
        <*> (lexeme (parseSymbol Equal) *> parseBlock)

parseBlock :: (?parserFlags :: ParserFlags) => Parser [(Located Instruction, Bool)]
parseBlock = MP.choice
  [ (:) <$> parseInstruction <*> (lexeme (parseSymbol Semi) *> lexeme parseBlock)
  , pure . (, False) <$> parseTerminalInstruction ]

parseTerminalInstruction :: (?parserFlags :: ParserFlags) => Parser (Located Instruction)
parseTerminalInstruction = lexeme $ MP.choice
  [ parseRet
  , parseJmp
  , parseCall
  ]

-- | Parses an instruction call from the N*'s instruction set.
parseInstruction :: (?parserFlags :: ParserFlags) => Parser (Located Instruction, Bool)
parseInstruction = do
  isUnsafe <- MP.option False (True <$ lexeme (parseSymbol UnSafe))

  (, isUnsafe) <$> lexeme (
    MP.choice
      [ parseMv
      , parseNop
      , parseSalloc
      , parseSfree
      , parseSld
      , parseSst
      , parseLd
      , parseSt
      ]
    )

------------------------------------------------------------------------------------------------------------

-- | Parses a forall type variable binder.
parseForallType :: (?parserFlags :: ParserFlags) => Parser (Located Type) -> Parser (Located Type)
parseForallType pty = located $
  ForAllT <$> (lexeme (parseSymbol Forall) *> (betweenParens $ lexeme binder `MP.sepBy` lexeme (parseSymbol Comma)) <* lexeme (parseSymbol Dot))
          <*> pty
 where
   binder =
     (,) <$> (lexeme parseVariableType)
         <*> (lexeme (parseSymbol Colon) *> lexeme parseKind)

-- | Parses a non-stack type. To parse a stack type, see 'parseStackType'.
parseType :: (?parserFlags :: ParserFlags) => Parser (Located Type)
parseType = MP.choice
  [ parseForallType (parseRecordType True)
  , parseSignedType
  , parseUnsignedType
  , parsePointerType
  , parseVariableType
  , betweenParens parseType
  ]

-- | Parses a record type.
parseRecordType :: (?parserFlags :: ParserFlags) => Bool -> Parser (Located Type)
parseRecordType open = located do
  (chi, sigma, epsilon) <- betweenBraces do
    (,,) <$> lexeme field `MP.sepBy` lexeme (parseSymbol Comma)
         <*> (lexeme (parseSymbol Pipe) *> lexeme parseStackType)
         <*> (lexeme (parseSymbol Arrow) *> lexeme parseContinuation)

  pure (RecordT (Map.fromList chi) sigma epsilon open)
  where
    field = (,) <$> (lexeme parseRegister <* lexeme (parseSymbol Colon)) <*> parseType

-- | Parses any sort of signed integer type.
parseSignedType :: (?parserFlags :: ParserFlags) => Parser (Located Type)
parseSignedType = located $ fmap SignedT . MP.choice $ signed <$> [ 64 ]
 where
   signed n = fromIntegral n <$ parseSymbol (Id ("s" <> Text.pack (show n)))

-- | Parses any sort of unsigned integer type.
parseUnsignedType :: (?parserFlags :: ParserFlags) => Parser (Located Type)
parseUnsignedType = located $ fmap UnsignedT . MP.choice $ unsigned <$> [ 64 ]
 where
   unsigned n = fromIntegral n <$ parseSymbol (Id ("u" <> Text.pack (show n)))

-- | Parses a pointer to a type.
parsePointerType :: (?parserFlags :: ParserFlags) => Parser (Located Type)
parsePointerType = lexeme . located $ lexeme (parseSymbol Star) *> (PtrT <$> parseType)

-- | Parses a stack type.
parseStackType :: (?parserFlags :: ParserFlags) => Parser (Located Type)
parseStackType = foldr1 cons <$> (parseType `MP.sepBy1` MP.try (lexeme (pure ()) *> lexeme (parseSymbol DoubleColon)))
  where
    cons stack@(_ :@ p) ty = ConsT stack ty :@ p

-- | Parses a type variable.
parseVariableType :: (?parserFlags :: ParserFlags) => Parser (Located Type)
parseVariableType = located $ VarT <$> parseIdentifier

-- | Parses a type kind.
parseKind :: (?parserFlags :: ParserFlags) => Parser (Located Kind)
parseKind = located $ MP.choice
  [ T8 <$ parseSymbol (Id "T8")
  , Ts <$ parseSymbol (Id "Ts")
  , Ta <$ parseSymbol (Id "Ta")
  , Tc <$ parseSymbol (Id "Tc") ]

parseContinuation :: (?parserFlags :: ParserFlags) => Parser (Located Type)
parseContinuation = located $ MP.choice
  [ RegisterContT . unLoc <$> parseRegister
  , StackContT . unLoc <$> parseInteger
  , VarT <$> parseIdentifier ]

------------------------------------------------------------------------------------------------------------

-- | Parses an immediate literal value.
parseImmediate :: (?parserFlags :: ParserFlags) => Parser (Located Immediate)
parseImmediate = located $ MP.choice
  [ I . unLoc <$> parseInteger
  , C . unLoc <$> parseCharacter ]

-- | Parses a literal label name.
parseLabel :: (?parserFlags :: ParserFlags) => Parser (Located Expr)
parseLabel = located $ NameE <$> parseIdentifier
                             <*> MP.option [] (betweenAngles (parseSpecialization `MP.sepBy` lexeme (parseSymbol Comma)))

parseSpecialization :: (?parserFlags :: ParserFlags) => Parser (Located Type)
parseSpecialization = MP.choice
  [ MP.try (lexeme parseStackType)
  , MP.try (betweenParens (lexeme parseStackType))
  , MP.try (lexeme parseContinuation)
  , MP.try (betweenParens (lexeme parseContinuation))
  , parseType
  ]

-- | Parses a base-pointer offset.
parseBasePtrOffset :: (?parserFlags :: ParserFlags) => Parser (Located Expr)
parseBasePtrOffset = located $
  BaseOffsetE <$> MP.choice [ located $ RegE <$> parseRegister, parseLabel ]
              <*> betweenBrackets (MP.choice [ located $ RegE <$> parseRegister, located $ ImmE <$> located (I <$> parseSignedInteger) ])

-- | Parses a byte-pointer offset.
parseBytePtrOffset :: (?parserFlags :: ParserFlags) => Parser (Located Expr)
parseBytePtrOffset = located $
  ByteOffsetE <$> do
                  source <- getPos <$> located (pure ())
                  MP.option (ImmE (I 0 :@ source) :@ source) (MP.choice [ located $ RegE <$> parseRegister, located $ ImmE <$> located (I <$> parseSignedInteger) ])
              <*> betweenParens (MP.choice [ located $ RegE <$> parseRegister, parseLabel ])

parseSignedInteger :: (?parserFlags :: ParserFlags) => Parser Integer
parseSignedInteger = MP.label "an integer" $ (*) <$> sign <*> (unLoc <$> parseInteger)
 where
   sign = MP.choice [ -1 <$ parseSymbol Minus, 1 <$ parseSymbol Plus, 1 <$ pure () ]

----------------------------------------------------------------------------------------------------------------

parseConstant :: (?parserFlags :: ParserFlags) => Parser (Located Constant)
parseConstant = parseIntegerConstant MP.<|> parseCharacterConstant MP.<|> parseArrayConstant

parseIntegerConstant :: (?parserFlags :: ParserFlags) => Parser (Located Constant)
parseIntegerConstant = located $ MP.label "an integer constant" $ IntegerC <$> located parseSignedInteger

parseCharacterConstant :: (?parserFlags :: ParserFlags) => Parser (Located Constant)
parseCharacterConstant = located $ MP.label "a character constant" $ CharacterC <$> parseCharacter

parseArrayConstant :: (?parserFlags :: ParserFlags) => Parser (Located Constant)
parseArrayConstant = located $ MP.label "an array constant" $ ArrayC <$> betweenBrackets (MP.many (lexeme parseConstant))

----------------------------------------------------------------------------------------------------------------

-- | Parses a @mov@ instruction.
parseMv :: (?parserFlags :: ParserFlags) => Parser (Located Instruction)
parseMv = located $
  lexeme (parseSymbol Mv) *>
    (MV <$> MP.choice [ located $ RegE <$> parseRegister, located $ ImmE <$> parseImmediate, parseLabel ]
        <*> (lexeme (parseSymbol Comma) *> parseRegister))

-- | Parses a @ret@ instruction.
parseRet :: (?parserFlags :: ParserFlags) => Parser (Located Instruction)
parseRet = located $ RET <$ lexeme (parseSymbol Ret)

-- | Parses a @jmp@ instruction.
parseJmp :: (?parserFlags :: ParserFlags) => Parser (Located Instruction)
parseJmp = located $
  lexeme (parseSymbol Jmp) *> (JMP <$> parseLabel)

parseCall :: (?parserFlags :: ParserFlags) => Parser (Located Instruction)
parseCall = located $
  lexeme (parseSymbol Call) *> (CALL <$> parseLabel)

parseNop :: (?parserFlags :: ParserFlags) => Parser (Located Instruction)
parseNop = located $ NOP <$ lexeme (parseSymbol Nop)

parseSalloc :: (?parserFlags :: ParserFlags) => Parser (Located Instruction)
parseSalloc = located $
  lexeme (parseSymbol Salloc) *> (SALLOC <$> parseType)

parseSfree :: (?parserFlags :: ParserFlags) => Parser (Located Instruction)
parseSfree = located $ SFREE <$ lexeme (parseSymbol Sfree)

parseSld :: (?parserFlags :: ParserFlags) => Parser (Located Instruction)
parseSld = located $
  lexeme (parseSymbol Sld) *> (SLD <$> parseInteger
                                   <*> (lexeme (parseSymbol Comma) *> parseRegister))

parseSst :: (?parserFlags :: ParserFlags) => Parser (Located Instruction)
parseSst = located $
  lexeme (parseSymbol Sst) *>
    (SST <$> MP.choice [ located $ RegE <$> parseRegister, located $ ImmE <$> parseImmediate, parseLabel ]
         <*> (lexeme (parseSymbol Comma) *> parseInteger))

parseLd :: (?parserFlags :: ParserFlags) => Parser (Located Instruction)
parseLd = located $
  lexeme (parseSymbol Ld) *>
    (LD <$> MP.choice [ MP.try parseBytePtrOffset, parseBasePtrOffset ]
        <*> (lexeme (parseSymbol Comma) *> (located $ RegE <$> parseRegister)))

parseSt :: (?parserFlags :: ParserFlags) => Parser (Located Instruction)
parseSt = located $
  lexeme (parseSymbol St) *>
    (ST <$> MP.choice [ located (RegE <$> parseRegister), located (ImmE <$> parseImmediate) ]
        <*> (lexeme (parseSymbol Comma) *> MP.choice [ MP.try parseBytePtrOffset, parseBasePtrOffset ]))
