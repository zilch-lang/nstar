{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}

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
import Data.Located (Located(..), unLoc)
import qualified Data.Map as Map (fromList)
import Console.NStar.Flags (ParserFlags(..))
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Foldable (foldl')
import Language.NStar.Syntax.Errors

type Parser a = WriterT [ParseWarning] (MP.Parsec SemanticError [LToken]) a

lexeme :: (?parserFlags :: ParserFlags) => Parser a -> Parser a
lexeme = MPL.lexeme (MPL.space MP.empty inlineComment multilineComment)
  where inlineComment = () <$ MP.satisfy isInlineComment
        isInlineComment (InlineComment _ :@ _) = True
        isInlineComment _                      = False

        multilineComment = () <$ MP.satisfy isMultilineComment
        isMultilineComment (MultilineComment _ :@ _) = True
        isMultilineComment _                         = False

-------------------------------------------------------------------------------------------------

-- | Turns a list of tokens into an AST, as long as tokens are well ordered. Else, it throws an error.
parseFile :: (?parserFlags :: ParserFlags) => FilePath -> [LToken] -> Either (Diagnostic [] String Char) (Program, Diagnostic [] String Char)
parseFile file tokens = bimap (megaparsecBundleToDiagnostic "Parse error on input") (second toDiagnostic) $ MP.runParser (runWriterT parseProgram) file tokens
  where toDiagnostic = foldl' (<++>) diagnostic . fmap fromParseWarning

-- | Parses a sequence of either typed labels or instruction calls.
parseProgram :: (?parserFlags :: ParserFlags) => Parser Program
parseProgram = MP.optional eol *> (Program <$> MP.many instructions) <* parseEOF
  where instructions = located (parseUnsafeBlock <* eol MP.<|> parseTypedLabel MP.<|> parseInstruction <* eol)

-- | Parses the end of file. There is no guarantee that any parser will try to parse something after the end of file.
--   This has to be dealt with on our own. No more token should be available after consuming the end of file.
parseEOF :: (?parserFlags :: ParserFlags) => Parser ()
parseEOF = () <$ parseSymbol EOF

-- | Parses the end of a line.
parseEOL :: (?parserFlags :: ParserFlags) => Parser ()
parseEOL = () <$ parseSymbol EOL

-- | Parses an identifier and returns its textual representation.
parseIdentifier :: (?parserFlags :: ParserFlags) => Parser (Located Text)
parseIdentifier = MP.label "an identifier" $ lexeme do
  Id i :@ p <- MP.satisfy isIdentifier
  pure (i :@ p)
 where
   isIdentifier (Id _ :@ _) = True
   isIdentifier (_ :@ _)    = False

-- | Parses a symbol and returns it.
parseSymbol :: (?parserFlags :: ParserFlags) => Token -> Parser LToken
parseSymbol t1 = MP.label (showToken t1) . lexeme $ MP.satisfy \ (t2 :@ _) -> t2 == t1

parseRegister :: (?parserFlags :: ParserFlags) => Parser Register
parseRegister = MP.label "a register" $ parseSymbol Percent *> reg
  where reg = MP.choice
          [ R0 <$ parseSymbol R0'
          , R1 <$ parseSymbol R1'
          , R2 <$ parseSymbol R2'
          , R3 <$ parseSymbol R3'
          , R4 <$ parseSymbol R4'
          , R5 <$ parseSymbol R5'
          , SP <$ parseSymbol SP'
          , BP <$ parseSymbol BP'
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

-- | @between opening closing p@ parses @p@ enclosed between @opening@ and @closing@.
between :: (?parserFlags :: ParserFlags) => Parser a -> Parser b -> Parser c -> Parser c
between opening closing p = opening *> p <* closing

-- | Parses something between braces.
betweenBraces :: (?parserFlags :: ParserFlags) => Parser a -> Parser a
betweenBraces = lexeme . between (parseSymbol LBrace) (parseSymbol RBrace)

-- | Parses something between brackets.
betweenBrackets :: (?parserFlags :: ParserFlags) => Parser a -> Parser a
betweenBrackets = lexeme . between (parseSymbol LBracket) (parseSymbol RBracket)

-- | Parses something between parentheses.
betweenParens :: (?parserFlags :: ParserFlags) => Parser a -> Parser a
betweenParens = lexeme . between (parseSymbol LParen) (parseSymbol RParen)

-- | Parses something between angles.
betweenAngles :: (?parserFlags :: ParserFlags) => Parser a -> Parser a
betweenAngles = lexeme . between (parseSymbol LAngle) (parseSymbol RAngle)

-- | @sepBy p sep@ parses 0 or more @p@ separated by @sep@.
sepBy :: (?parserFlags :: ParserFlags) => Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep MP.<|> pure []

-- | @sepBy1 p sep@ is much like 'sepBy' but parses at least one occurrence of @p@.
sepBy1 :: (?parserFlags :: ParserFlags) => Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> MP.many (sep *> p)


-----------------------------------

eol :: (?parserFlags :: ParserFlags) => Parser ()
eol = lexeme (pure ()) *> (MP.lookAhead parseEOF MP.<|> MP.skipSome (lexeme parseEOL))

-- | Parses a typed label.
parseTypedLabel :: (?parserFlags :: ParserFlags) => Parser Statement
parseTypedLabel = lexeme $
  Label <$> (parseIdentifier <* parseSymbol Colon)
        <*> (located (parseForallType (parseRecordType True) MP.<|> parseRecordType True) <* eol)
        <*> MP.manyTill (located (parseUnsafeBlock MP.<|> parseInstruction) <* eol)
                        (MP.lookAhead (parseEOF MP.<|> MP.try (() <$ parseTypedLabel)))

-- | Parses an instruction call from the N*'s instruction set.
parseInstruction :: (?parserFlags :: ParserFlags) => Parser Statement
parseInstruction = lexeme $ Instr <$> MP.choice
  [ parseMov
  , parseRet
  , parseJmp
  , parseCall
  ]

parseUnsafeBlock :: (?parserFlags :: ParserFlags) => Parser Statement
parseUnsafeBlock = lexeme $ fmap Unsafe $ parseSymbol UnSafe *> MP.choice
  [ betweenBraces (MP.optional eol *> MP.choice
                   [ pure <$> located parseInstruction <* MP.optional eol
                   , MP.many (located (parseTypedLabel MP.<|> parseInstruction) <* eol) ])
  , pure <$> located parseInstruction
  ]


------------------------------------------------------------------------------------------------------------

-- | Parses a forall type variable binder.
parseForallType :: (?parserFlags :: ParserFlags) => Parser Type -> Parser Type
parseForallType pty =
  ForAll <$> (parseSymbol Forall *> MP.some binders <* parseSymbol Dot)
         <*> located pty
 where
   binders = betweenParens $
     (,) <$> located (Var <$> parseIdentifier)
         <*> (parseSymbol Colon *> located parseKind)

-- | Parses a non-stack type. To parse a stack type, see 'parseStackType'.
parseType :: (?parserFlags :: ParserFlags) => Parser Type
parseType = lexeme $ MP.choice
  [ parseRecordType True
  , parseSignedType
  , parseUnsignedType
  , parsePointerType
  , parseStackPointerType
  , parseVariableType
  , betweenParens parseType
  ]

-- | Parses a record type.
parseRecordType :: (?parserFlags :: ParserFlags) => Bool -> Parser Type
parseRecordType open = flip Record open . Map.fromList <$> betweenBraces (field `sepBy` parseSymbol Comma)
  where
    field = (,) <$> (located parseRegister <* parseSymbol Colon) <*> located parseType

-- | Parses any sort of signed integer type.
parseSignedType :: (?parserFlags :: ParserFlags) => Parser Type
parseSignedType = fmap Signed . MP.choice $ signed <$> [ 64 ]
 where
   signed n = fromIntegral n <$ parseSymbol (Id ("s" <> Text.pack (show n)))

-- | Parses any sort of unsigned integer type.
parseUnsignedType :: (?parserFlags :: ParserFlags) => Parser Type
parseUnsignedType = fmap Unsigned . MP.choice $ unsigned <$> [ 64 ]
 where
   unsigned n = fromIntegral n <$ parseSymbol (Id ("u" <> Text.pack (show n)))

-- | Parses a pointer to a type.
parsePointerType :: (?parserFlags :: ParserFlags) => Parser Type
parsePointerType = parseSymbol Star *> (Ptr <$> located parseType)

-- | Parses a pointer to a stack type.
parseStackPointerType :: (?parserFlags :: ParserFlags) => Parser Type
parseStackPointerType = parseSymbol Sptr *> (SPtr <$> parseStackType)

-- | Parses a stack type.
parseStackType :: (?parserFlags :: ParserFlags) => Parser (Located Type)
parseStackType = foldr1 cons <$> (located parseType `sepBy1` parseSymbol DoubleColon)
  where
    cons stack@(_ :@ p) ty = Cons stack ty :@ p

-- | Parses a type variable.
parseVariableType :: (?parserFlags :: ParserFlags) => Parser Type
parseVariableType = Var <$> parseIdentifier

-- | Parses a type kind.
parseKind :: (?parserFlags :: ParserFlags) => Parser Kind
parseKind = MP.choice
  [ T8 <$ parseSymbol (Id "T8")
  , Ts <$ parseSymbol (Id "Ts")
  , Ta <$ parseSymbol (Id "Ta") ]

------------------------------------------------------------------------------------------------------------

-- | Parses any kind of expression.
parseExpr :: (?parserFlags :: ParserFlags) => Parser Expr
parseExpr = parseValueExpr MP.<|> parseAddressExpr

-- | Parses only expressions that cannot be indexed.
parseValueExpr :: (?parserFlags :: ParserFlags) => Parser Expr
parseValueExpr = lexeme $ MP.choice
  [ Imm <$> located parseImmediate ]

-- | Parses an immediate literal value.
parseImmediate :: (?parserFlags :: ParserFlags) => Parser Immediate
parseImmediate = MP.choice
  [ I . unLoc <$> parseInteger
  , C . unLoc <$> parseCharacter ]

-- | Parses expressions that can be set and indexed.
parseAddressExpr :: (?parserFlags :: ParserFlags) => Parser Expr
parseAddressExpr = lexeme $ MP.choice
  [ parseLabel
  , Reg <$> located parseRegister
  , parseIndexedExpr ]

-- | Parses a literal label name.
parseLabel :: (?parserFlags :: ParserFlags) => Parser Expr
parseLabel = Name <$> parseIdentifier

-- | Parses an indexed addressable expression.
parseIndexedExpr :: (?parserFlags :: ParserFlags) => Parser Expr
parseIndexedExpr = MP.label "an indexed expression" $
  Indexed <$> located parseSignedInteger
          <*> betweenParens (located parseAddressExpr)

parseSignedInteger :: (?parserFlags :: ParserFlags) => Parser Integer
parseSignedInteger = (*) <$> sign <*> (unLoc <$> parseInteger)
 where
   sign = MP.choice [ -1 <$ parseSymbol Minus, 1 <$ pure () ]

----------------------------------------------------------------------------------------------------------------

-- | Parses a @mov@ instruction.
parseMov :: (?parserFlags :: ParserFlags) => Parser Instruction
parseMov =
  parseSymbol Mov *>
    (MOV <$> located parseExpr
         <*> (parseSymbol Comma *> located parseAddressExpr))

-- | Parses a @ret@ instruction.
parseRet :: (?parserFlags :: ParserFlags) => Parser Instruction
parseRet = RET <$ parseSymbol Ret

-- | Parses a @jmp@ instruction.
parseJmp :: (?parserFlags :: ParserFlags) => Parser Instruction
parseJmp =
  parseSymbol Jmp *>
    (JMP <$> located parseLabel
         <*> MP.option [] (betweenAngles (parseSpecialization `MP.sepBy` parseSymbol Comma)))

parseSpecialization :: (?parserFlags :: ParserFlags) => Parser (Located Type)
parseSpecialization = located $ MP.choice
  [ unLoc <$> MP.try parseStackType
  , unLoc <$> MP.try (betweenParens parseStackType)
  , parseType
  ]

parseCall :: (?parserFlags :: ParserFlags) => Parser Instruction
parseCall =
  parseSymbol Call *>
    (CALL <$> located parseLabel
          <*> MP.option [] (betweenAngles (parseSpecialization `MP.sepBy` parseSymbol Comma)))
