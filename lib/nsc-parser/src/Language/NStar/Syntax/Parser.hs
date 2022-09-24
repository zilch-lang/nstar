{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- |
--  Module: Language.NStar.Syntax.Parser
--  Description: NStar's parser for a x64 syntax
--  Copyright: (c) Mesabloo, 2020
--  License: BSD3
--  Stability: experimental
module Language.NStar.Syntax.Parser (parseFile) where

import Console.NStar.Flags (ParserFlags (..))
import Control.Monad.Writer (MonadWriter, runWriterT)
import Data.Bifunctor (bimap, second)
import Data.Foldable (foldl')
import Data.Located (Located (..), getPos, unLoc)
import qualified Data.Map as Map (fromList)
import Data.Text (Text)
import qualified Data.Text as Text
import Error.Diagnose (Diagnostic, addReport, def)
import Error.Diagnose.Compat.Megaparsec
import Internal.Error (internalError)
import Language.NStar.Syntax.Core
import Language.NStar.Syntax.Errors
import Language.NStar.Syntax.Internal
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

-- type Parser a = WriterT [ParseWarning] (MP.Parsec SemanticError [LToken]) a

type MonadParser m = (?parserFlags :: ParserFlags, MonadFail m, MonadWriter [ParseWarning] m, MP.MonadParsec SemanticError [LToken] m)

lexeme :: MonadParser m => m a -> m a
lexeme = MPL.lexeme (MPL.space whiteSpace inlineComment multilineComment)
  where
    inlineComment = () <$ MP.satisfy isInlineComment
    isInlineComment (InlineComment _ :@ _) = True
    isInlineComment _ = False

    multilineComment = () <$ MP.satisfy isMultilineComment
    isMultilineComment (MultilineComment _ :@ _) = True
    isMultilineComment _ = False

    whiteSpace = () <$ MP.satisfy isWhitespace
    isWhitespace (HSpace :@ _) = True
    isWhitespace (EOL :@ _) = True
    isWhitespace _ = False

-------------------------------------------------------------------------------------------------

-- | Turns a list of tokens into an AST, as long as tokens are well ordered. Else, it throws an error.
parseFile :: (?parserFlags :: ParserFlags) => FilePath -> [LToken] -> Either (Diagnostic String) (Program, Diagnostic String)
parseFile file tokens = bimap (errorDiagnosticFromBundle "Parse error on input" Nothing) (second toDiagnostic) $ MP.runParser (runWriterT parseProgram) file tokens
  where
    toDiagnostic = foldl' addReport def . fmap fromParseWarning

-- | Parses a sequence of either typed labels or instruction calls.
parseProgram :: MonadParser m => m Program
parseProgram = lexeme (pure ()) *> (Program <$> MP.many section) <* parseEOF
  where
    section = lexeme . located $ MP.choice [parseInclude, MP.try parseCodeSection, MP.try parseDataSection, parseExternCodeSection]

parseCodeSection :: MonadParser m => m Section
parseCodeSection =
  CodeS <$> do
    lexeme (parseSymbol Section)
    lexeme (parseSymbol (Id "code"))
    lexeme $ betweenBraces (MP.many parseTypedLabel)

parseDataSection :: MonadParser m => m Section
parseDataSection =
  DataS <$> do
    lexeme (parseSymbol Section)
    lexeme (parseSymbol (Id "data"))
    lexeme $ betweenBraces (MP.many (located binding))
  where
    binding =
      Bind <$> (lexeme parseIdentifier <* lexeme (parseSymbol Colon))
        <*> (lexeme parseType <* lexeme (parseSymbol Equal))
        <*> (lexeme parseConstant)

parseInclude :: MonadParser m => m Section
parseInclude =
  IncludeS <$> do
    lexeme (parseSymbol Include)
    betweenBraces $ MP.many (fmap toText <$> parseString)
  where
    toText (Str s) = s
    toText t = internalError $ "Cannot get text of non string token " <> show t

parseExternCodeSection :: MonadParser m => m Section
parseExternCodeSection =
  ExternCodeS <$> do
    lexeme (parseSymbol Section)
    lexeme (parseSymbol (Id "extern"))
    lexeme (parseSymbol Dot)
    lexeme (parseSymbol (Id "code"))
    lexeme $ betweenBraces (MP.many (located binding))
  where
    binding =
      ReservedBind <$> (lexeme parseIdentifier <* lexeme (parseSymbol Colon))
        <*> lexeme (parseForallType (parseRecordType True))

-- | Parses the end of file. There is no guarantee that any parser will try to parse something after the end of file.
--   This has to be dealt with on our own. No more token should be available after consuming the end of file.
parseEOF :: MonadParser m => m ()
parseEOF = () <$ parseSymbol EOF

-- | Parses an identifier and returns its textual representation.
parseIdentifier :: MonadParser m => m (Located Text)
parseIdentifier = MP.label "an identifier" $ lexeme do
  Id i :@ p <- MP.satisfy isIdentifier
  pure (i :@ p)
  where
    isIdentifier (Id _ :@ _) = True
    isIdentifier (_ :@ _) = False

parseString :: MonadParser m => m LToken
parseString = MP.label "a string" $ lexeme $ MP.satisfy isString
  where
    isString (Str _ :@ _) = True
    isString _ = False

-- | Parses a symbol and returns it.
parseSymbol :: MonadParser m => Token -> m LToken
parseSymbol t1 = MP.label (showToken t1) $ MP.satisfy \(t2 :@ _) -> t2 == t1

parseRegister :: MonadParser m => m (Located Register)
parseRegister = MP.label "a register" $ located $ parseSymbol Percent *> reg
  where
    reg =
      MP.choice
        [ R0 <$ parseSymbol R0',
          R1 <$ parseSymbol R1',
          R2 <$ parseSymbol R2',
          R3 <$ parseSymbol R3',
          R4 <$ parseSymbol R4',
          R5 <$ parseSymbol R5',
          MP.lookAhead MP.anySingle >>= MP.customFailure . NoSuchRegister . unLoc
        ]

parseInteger :: MonadParser m => m (Located Integer)
parseInteger = MP.label "an integer" $ lexeme do
  Integer i :@ p <- MP.satisfy isInteger
  pure (read (Text.unpack i) :@ p)
  where
    isInteger (Integer _ :@ _) = True
    isInteger _ = False

parseCharacter :: MonadParser m => m (Located Char)
parseCharacter = MP.label "a character" $ lexeme do
  Char c :@ p <- MP.satisfy isCharacter
  pure (c :@ p)
  where
    isCharacter (Char _ :@ _) = True
    isCharacter _ = False

-- | Parses something between braces.
betweenBraces :: MonadParser m => m a -> m a
betweenBraces = MP.between (lexeme $ parseSymbol LBrace) (parseSymbol RBrace)

-- | Parses something between brackets.
betweenBrackets :: MonadParser m => m a -> m a
betweenBrackets = MP.between (lexeme $ parseSymbol LBracket) (parseSymbol RBracket)

-- | Parses something between parentheses.
betweenParens :: MonadParser m => m a -> m a
betweenParens = MP.between (lexeme $ parseSymbol LParen) (parseSymbol RParen)

-- | Parses something between angles.
betweenAngles :: MonadParser m => m a -> m a
betweenAngles = MP.between (lexeme $ parseSymbol LAngle) (parseSymbol RAngle)

-----------------------------------

-- | Parses a typed label.
parseTypedLabel :: MonadParser m => m (Located Statement)
parseTypedLabel =
  lexeme . located $
    Label <$> (lexeme parseIdentifier <* lexeme (parseSymbol Colon))
      <*> lexeme (parseForallType (parseRecordType True))
      <*> (lexeme (parseSymbol Equal) *> parseBlock)

parseBlock :: MonadParser m => m [(Located Instruction, Bool)]
parseBlock =
  MP.choice
    [ (:) <$> parseInstruction <*> (lexeme (parseSymbol Semi) *> lexeme parseBlock),
      pure . (,False) <$> parseTerminalInstruction
    ]

parseTerminalInstruction :: MonadParser m => m (Located Instruction)
parseTerminalInstruction = MP.label "a terminal instruction" do
  lexeme $
    MP.choice
      [ parseRet,
        parseJmp,
        parseCall
      ]

-- | Parses an instruction call from the N*'s instruction set.
parseInstruction :: MonadParser m => m (Located Instruction, Bool)
parseInstruction = MP.label "an instruction" do
  isUnsafe <- MP.option False (True <$ lexeme (parseSymbol UnSafe))

  (,isUnsafe)
    <$> lexeme
      ( MP.choice
          [ parseMv,
            parseNop,
            parseSalloc,
            parseSfree,
            parseSld,
            parseSst,
            parseLd,
            parseSt,
            parseSref,
            parseAnd,
            parseOr,
            parseXor,
            parseNot,
            parseCmvz,
            parseCmvnz,
            parseAdd,
            parseShiftl,
            parseShiftr,
            parseSub,
            parseMul,
            parseCmvl,
            parseCmvge,
            parseCmvle,
            parseCmvg,
            parseCmve,
            parseCmvne
          ]
      )

------------------------------------------------------------------------------------------------------------

-- | Parses a forall type variable binder.
parseForallType :: MonadParser m => m (Located Type) -> m (Located Type)
parseForallType pty =
  located $
    ForAllT <$> (lexeme (parseSymbol Forall) *> (betweenParens $ lexeme binder `MP.sepBy` lexeme (parseSymbol Comma)) <* lexeme (parseSymbol Dot))
      <*> pty
  where
    binder =
      (,) <$> (lexeme parseVariableType)
        <*> (lexeme (parseSymbol Colon) *> lexeme parseKind)

-- | Parses a non-stack type. To parse a stack type, see 'parseStackType'.
parseType :: MonadParser m => m (Located Type)
parseType =
  MP.choice
    [ parseForallType (parseRecordType True),
      parseSignedType,
      parseUnsignedType,
      parsePointerType,
      parseVariableType,
      parseStructType
    ]

-- | Parses a record type.
parseRecordType :: MonadParser m => Bool -> m (Located Type)
parseRecordType open = located do
  (chi, sigma, epsilon) <- betweenBraces do
    (,,) <$> lexeme field `MP.sepBy` lexeme (parseSymbol Comma)
      <*> (lexeme (parseSymbol Pipe) *> lexeme parseStackType)
      <*> (lexeme (parseSymbol Arrow) *> lexeme parseContinuation)

  pure (RecordT (Map.fromList chi) sigma epsilon open)
  where
    field = (,) <$> (lexeme parseRegister <* lexeme (parseSymbol Colon)) <*> (parseBang MP.<|> parseType)

-- | Parses any sort of signed integer type.
parseSignedType :: MonadParser m => m (Located Type)
parseSignedType = located $ fmap SignedT . MP.choice $ signed <$> [8, 16, 32, 64]
  where
    signed n = fromIntegral n <$ parseSymbol (Id ("s" <> Text.pack (show n)))

-- | Parses any sort of unsigned integer type.
parseUnsignedType :: MonadParser m => m (Located Type)
parseUnsignedType = located $ fmap UnsignedT . MP.choice $ unsigned <$> [8, 16, 32, 64]
  where
    unsigned n = fromIntegral n <$ parseSymbol (Id ("u" <> Text.pack (show n)))

-- | Parses a pointer to a type.
parsePointerType :: MonadParser m => m (Located Type)
parsePointerType = lexeme . located $ lexeme (parseSymbol Star) *> (PtrT <$> parseType)

-- | Parses a stack type.
parseStackType :: MonadParser m => m (Located Type)
parseStackType = foldr1 cons <$> (parseType `MP.sepBy1` MP.try (lexeme (pure ()) *> lexeme (parseSymbol DoubleColon)))
  where
    cons stack@(_ :@ p) ty = ConsT stack ty :@ p

-- | Parses a type variable.
parseVariableType :: MonadParser m => m (Located Type)
parseVariableType = located $ VarT <$> parseIdentifier

-- | Parses a structure type.
parseStructType :: MonadParser m => m (Located Type)
parseStructType = located $ PackedStructT <$> betweenParens ((lexeme parseType `MP.sepBy` lexeme (parseSymbol Comma)) MP.<|> pure [])

-- | Parses a type kind.
parseKind :: MonadParser m => m (Located Kind)
parseKind =
  located $
    MP.choice
      [ T 8 <$ parseSymbol (TnK 8),
        Ts <$ parseSymbol TsK,
        Ta <$ parseSymbol TaK,
        Tc <$ parseSymbol TcK
      ]

parseContinuation :: MonadParser m => m (Located Type)
parseContinuation =
  located $
    MP.choice
      [ RegisterContT . unLoc <$> parseRegister,
        StackContT . unLoc <$> parseInteger,
        VarT <$> parseIdentifier
      ]

parseBang :: MonadParser m => m (Located Type)
parseBang = located $ BangT <$ parseSymbol Bang

------------------------------------------------------------------------------------------------------------

-- | Parses an immediate literal value.
parseImmediate :: MonadParser m => m (Located Immediate)
parseImmediate =
  located $
    MP.choice
      [ I . unLoc <$> parseInteger,
        C . unLoc <$> parseCharacter
      ]

-- | Parses a literal label name.
parseLabel :: MonadParser m => m (Located Expr)
parseLabel =
  located $
    NameE <$> parseIdentifier
      <*> MP.option [] (betweenAngles (parseSpecialization `MP.sepBy` lexeme (parseSymbol Comma)))

parseSpecialization :: MonadParser m => m (Located Type)
parseSpecialization =
  MP.choice
    [ MP.try (lexeme parseStackType),
      MP.try (betweenParens (lexeme parseStackType)),
      MP.try (lexeme parseContinuation),
      MP.try (betweenParens (lexeme parseContinuation)),
      parseType
    ]

-- | Parses a base-pointer offset.
parseBasePtrOffset :: MonadParser m => m (Located Expr)
parseBasePtrOffset =
  located $
    BaseOffsetE <$> MP.choice [located $ RegE <$> parseRegister, parseLabel]
      <*> betweenBrackets (MP.choice [located $ RegE <$> parseRegister, located $ ImmE <$> located (I <$> parseSignedInteger)])

-- | Parses a byte-pointer offset.
parseBytePtrOffset :: MonadParser m => m (Located Expr)
parseBytePtrOffset =
  located $
    ByteOffsetE <$> do
      source <- getPos <$> located (pure ())
      MP.option (ImmE (I 0 :@ source) :@ source) (MP.choice [located $ RegE <$> parseRegister, located $ ImmE <$> located (I <$> parseSignedInteger)])
      <*> betweenParens (MP.choice [located $ RegE <$> parseRegister, parseLabel])

parseSignedInteger :: MonadParser m => m Integer
parseSignedInteger = MP.label "an integer" $ (*) <$> sign <*> (unLoc <$> parseInteger)
  where
    sign = MP.choice [-1 <$ parseSymbol Minus, 1 <$ parseSymbol Plus, 1 <$ pure ()]

----------------------------------------------------------------------------------------------------------------

parseConstant :: MonadParser m => m (Located Constant)
parseConstant = parseIntegerConstant MP.<|> parseCharacterConstant MP.<|> parseStringConstant MP.<|> parseArrayConstant MP.<|> parseStructConstant

parseIntegerConstant :: MonadParser m => m (Located Constant)
parseIntegerConstant = located $ MP.label "an integer constant" $ IntegerC <$> located parseSignedInteger

parseCharacterConstant :: MonadParser m => m (Located Constant)
parseCharacterConstant = located $ MP.label "a character constant" $ CharacterC <$> parseCharacter

parseArrayConstant :: MonadParser m => m (Located Constant)
parseArrayConstant = located $ MP.label "an array constant" $ ArrayC <$> betweenBrackets (MP.many (lexeme parseConstant))

parseStringConstant :: MonadParser m => m (Located Constant)
parseStringConstant = located $ MP.label "a string constant" $ toArrayConstant <$> parseString
  where
    toArrayConstant (Str chars :@ p) = ArrayC $ ((:@ p) . CharacterC . (:@ p) <$> Text.unpack chars) <> [CharacterC ('\0' :@ p) :@ p]
    toArrayConstant l = internalError $ "Invalid string token " <> show (unLoc l)

parseStructConstant :: MonadParser m => m (Located Constant)
parseStructConstant = located $ MP.label "a structure constant" $ StructC <$> betweenParens (MP.option [] (lexeme parseField `MP.sepBy` lexeme (parseSymbol Comma)))
  where
    parseField = parseIntegerConstant MP.<|> parseCharacterConstant MP.<|> parseStructConstant

----------------------------------------------------------------------------------------------------------------

-- | Parses a @mov@ instruction.
parseMv :: MonadParser m => m (Located Instruction)
parseMv =
  located $
    lexeme (parseSymbol Mv)
      *> ( MV <$> MP.choice [located $ RegE <$> parseRegister, located $ ImmE <$> parseImmediate, parseLabel]
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

-- | Parses a @ret@ instruction.
parseRet :: MonadParser m => m (Located Instruction)
parseRet = located $ RET <$ lexeme (parseSymbol Ret)

-- | Parses a @jmp@ instruction.
parseJmp :: MonadParser m => m (Located Instruction)
parseJmp =
  located $
    lexeme (parseSymbol Jmp) *> (JMP <$> parseLabel)

parseCall :: MonadParser m => m (Located Instruction)
parseCall =
  located $
    lexeme (parseSymbol Call) *> (CALL <$> parseLabel)

parseNop :: MonadParser m => m (Located Instruction)
parseNop = located $ NOP <$ lexeme (parseSymbol Nop)

parseSalloc :: MonadParser m => m (Located Instruction)
parseSalloc =
  located $
    lexeme (parseSymbol Salloc) *> (SALLOC <$> parseType)

parseSfree :: MonadParser m => m (Located Instruction)
parseSfree = located $ SFREE <$ lexeme (parseSymbol Sfree)

parseSld :: MonadParser m => m (Located Instruction)
parseSld =
  located $
    lexeme (parseSymbol Sld)
      *> ( SLD <$> parseInteger
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseSst :: MonadParser m => m (Located Instruction)
parseSst =
  located $
    lexeme (parseSymbol Sst)
      *> ( SST <$> MP.choice [located $ RegE <$> parseRegister, located $ ImmE <$> parseImmediate, parseLabel]
             <*> (lexeme (parseSymbol Comma) *> parseInteger)
         )

parseLd :: MonadParser m => m (Located Instruction)
parseLd =
  located $
    lexeme (parseSymbol Ld)
      *> ( LD <$> MP.choice [MP.try parseBytePtrOffset, parseBasePtrOffset]
             <*> (lexeme (parseSymbol Comma) *> (located $ RegE <$> parseRegister))
         )

parseSt :: MonadParser m => m (Located Instruction)
parseSt =
  located $
    lexeme (parseSymbol St)
      *> ( ST <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [MP.try parseBytePtrOffset, parseBasePtrOffset])
         )

parseSref :: MonadParser m => m (Located Instruction)
parseSref =
  located $
    lexeme (parseSymbol Sref)
      *> ( SREF <$> parseInteger
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseAnd :: MonadParser m => m (Located Instruction)
parseAnd =
  located $
    lexeme (parseSymbol And)
      *> ( AND <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)])
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseOr :: MonadParser m => m (Located Instruction)
parseOr =
  located $
    lexeme (parseSymbol Or)
      *> ( OR <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)])
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseXor :: MonadParser m => m (Located Instruction)
parseXor =
  located $
    lexeme (parseSymbol Xor)
      *> ( XOR <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)])
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseNot :: MonadParser m => m (Located Instruction)
parseNot =
  located $
    lexeme (parseSymbol Not)
      *> ( NOT <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseCmvz :: MonadParser m => m (Located Instruction)
parseCmvz =
  located $
    lexeme (parseSymbol Cmvz)
      *> ( CMVZ <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseCmvnz :: MonadParser m => m (Located Instruction)
parseCmvnz =
  located $
    lexeme (parseSymbol Cmvnz)
      *> ( CMVNZ <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseAdd :: MonadParser m => m (Located Instruction)
parseAdd =
  located $
    lexeme (parseSymbol Add)
      *> ( ADD <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)])
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseSub :: MonadParser m => m (Located Instruction)
parseSub =
  located $
    lexeme (parseSymbol Sub)
      *> ( SUB <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)])
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseShiftl :: MonadParser m => m (Located Instruction)
parseShiftl =
  located $
    lexeme (parseSymbol Shiftl)
      *> ( SHIFTL <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> parseInteger)
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseShiftr :: MonadParser m => m (Located Instruction)
parseShiftr =
  located $
    lexeme (parseSymbol Shiftr)
      *> ( SHIFTR <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> parseInteger)
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseMul :: MonadParser m => m (Located Instruction)
parseMul =
  located $
    lexeme (parseSymbol Mul)
      *> ( MUL <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)])
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseCmvl :: MonadParser m => m (Located Instruction)
parseCmvl =
  located $
    lexeme (parseSymbol Cmvl)
      *> ( CMVL <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)])
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseCmvge :: MonadParser m => m (Located Instruction)
parseCmvge =
  located $
    lexeme (parseSymbol Cmvge)
      *> ( CMVGE <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)])
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseCmvle :: MonadParser m => m (Located Instruction)
parseCmvle =
  located $
    lexeme (parseSymbol Cmvle)
      *> ( CMVLE <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)])
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseCmvg :: MonadParser m => m (Located Instruction)
parseCmvg =
  located $
    lexeme (parseSymbol Cmvg)
      *> ( CMVG <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)])
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseCmve :: MonadParser m => m (Located Instruction)
parseCmve =
  located $
    lexeme (parseSymbol Cmve)
      *> ( CMVE <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)])
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )

parseCmvne :: MonadParser m => m (Located Instruction)
parseCmvne =
  located $
    lexeme (parseSymbol Cmvne)
      *> ( CMVNE <$> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)]
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister), located (ImmE <$> parseImmediate)])
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> MP.choice [located (RegE <$> parseRegister)])
             <*> (lexeme (parseSymbol Comma) *> parseRegister)
         )
