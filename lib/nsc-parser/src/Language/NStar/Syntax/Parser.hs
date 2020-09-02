{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDeriving #-}
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
import Language.NStar.Syntax.Hints (Hintable(..))
import Text.Diagnose (Diagnostic, hint)
import Data.Bifunctor (first)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Located (Located(..), unLoc)
import qualified Data.Map as Map (fromList)

type Parser a = MP.Parsec SemanticError [LToken] a

data SemanticError
  = NoSuchRegister Token
  deriving (Eq, Ord, Data, Typeable)

instance Show SemanticError where
  show (NoSuchRegister t) = "unrecognized register " <> showToken t

instance MP.ShowErrorComponent SemanticError where
  showErrorComponent = show

instance Hintable SemanticError String where
  hints (NoSuchRegister _) =
    [ hint "Registers are fixed depending on the target architecture."
    , hint "Registers available in different architectures are documented here: <https://github.com/nihil-lang/nsc/blob/develop/docs/registers.md>." ]

lexeme :: Parser a -> Parser a
lexeme = MPL.lexeme (MPL.space MP.empty inlineComment multilineComment)
  where inlineComment = () <$ MP.satisfy isInlineComment
        isInlineComment (InlineComment _ :@ _) = True
        isInlineComment _                      = False

        multilineComment = () <$ MP.satisfy isMultilineComment
        isMultilineComment (MultilineComment _ :@ _) = True
        isMultilineComment _                         = False

-------------------------------------------------------------------------------------------------

-- | Turns a list of tokens into an AST, as long as tokens are well ordered. Else, it throws an error.
parseFile :: FilePath -> [LToken] -> Either (Diagnostic [] String Char) Program
parseFile = first (megaparsecBundleToDiagnostic "Parse error on input") .: MP.runParser parseProgram
  where (.:) = (.) . (.)

-- | Parses a sequence of either typed labels or instruction calls.
parseProgram :: Parser Program
parseProgram = noise *> (Program <$> MP.many instructions) <* parseEOF
  where instructions = located (parseTypedLabel MP.<|> parseInstructionCall) <* (parseEOL MP.<|> parseEOF)
        noise = lexeme (pure ()) *> MP.many (lexeme (MP.try parseEOL))

-- | Parses the end of file. There is no guarantee that any parser will try to parse something after the end of file.
--   This has to be dealt with on our own. No more token should be available after consuming the end of file.
parseEOF :: Parser ()
parseEOF = () <$ parseSymbol EOF

-- | Parses the end of a line.
parseEOL :: Parser ()
parseEOL = () <$ parseSymbol EOL

-- | Parses an identifier and returns its textual representation.
parseIdentifier :: Parser (Located Text)
parseIdentifier = MP.label "an identifier" $ lexeme do
  Id i :@ p <- MP.satisfy isIdentifier
  pure (i :@ p)
 where
   isIdentifier (Id _ :@ _) = True
   isIdentifier (_ :@ _)    = False

-- | Parses a symbol and returns it.
parseSymbol :: Token -> Parser LToken
parseSymbol t1 = MP.label (showToken t1) . lexeme $ MP.satisfy \ (t2 :@ _) -> t2 == t1

parseRegister :: Parser Register
parseRegister = MP.label "a register" $ parseSymbol Percent *> reg
  where reg = MP.choice
          [ RAX <$ parseSymbol Rax
          , RBX <$ parseSymbol Rbx
          , RCX <$ parseSymbol Rcx
          , RDX <$ parseSymbol Rdx
          , RSI <$ parseSymbol Rsi
          , RDI <$ parseSymbol Rdi
          , RSP <$ parseSymbol Rsp
          , RBP <$ parseSymbol Rbp
          , MP.lookAhead MP.anySingle >>= MP.customFailure . NoSuchRegister . unLoc ]

parseInteger :: Parser (Located Integer)
parseInteger = MP.label "an integer" $ lexeme do
  Integer i :@ p <- MP.satisfy isInteger
  pure (i :@ p)
 where
   isInteger (Integer _ :@ _) = True
   isInteger _                = False

parseCharacter :: Parser (Located Char)
parseCharacter = MP.label "a character" $ lexeme do
  Char c :@ p <- MP.satisfy isCharacter
  pure (c :@ p)
 where
   isCharacter (Char _ :@ _) = True
   isCharacter _             = False

-- | @between opening closing p@ parses @p@ enclosed between @opening@ and @closing@.
between :: Parser a -> Parser b -> Parser c -> Parser c
between opening closing p = opening *> p <* closing

-- | Parses something between braces.
betweenBraces :: Parser a -> Parser a
betweenBraces = lexeme . between (parseSymbol LBrace) (parseSymbol RBrace)

-- | Parses something between brackets.
betweenBrackets :: Parser a -> Parser a
betweenBrackets = lexeme . between (parseSymbol LBracket) (parseSymbol RBracket)

-- | Parses something between parentheses.
betweenParens :: Parser a -> Parser a
betweenParens = lexeme . between (parseSymbol LParen) (parseSymbol RParen)

-- | Parses something between angles.
betweenAngles :: Parser a -> Parser a
betweenAngles = lexeme . between (parseSymbol LAngle) (parseSymbol RAngle)

-- | @sepBy p sep@ parses 0 or more @p@ separated by @sep@.
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep MP.<|> pure []

-- | @sepBy1 p sep@ is much like 'sepBy' but parses at least one occurrence of @p@.
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> MP.many (sep *> p)


-- | Parses a typed label.
parseTypedLabel :: Parser Statement
parseTypedLabel = lexeme $
  Label <$> (parseIdentifier <* parseSymbol Colon)
        <*> located (parseForallType parseRecordType MP.<|> parseRecordType)

-- | Parses an instruction call from the N*'s instruction set.
parseInstructionCall :: Parser Statement
parseInstructionCall = MP.choice $ fmap Instr <$>
  [ parseMov
  , parseRet ]

------------------------------------------------------------------------------------------------------------

-- | Parses a forall type variable binder.
parseForallType :: Parser Type -> Parser Type
parseForallType pty =
  ForAll <$> (parseSymbol Forall *> MP.some binders <* parseSymbol Dot)
         <*> located pty
 where
   binders = betweenParens $
     (,) <$> located (Var <$> parseIdentifier)
         <*> (parseSymbol Colon *> located parseKind)

-- | Parses a non-stack type. To parse a stack type, see 'parseStackType'.
parseType :: Parser Type
parseType = lexeme $ MP.choice
  [ parseRecordType
  , parseSignedType
  , parseUnsignedType
  , parsePointerType
  , parseStackPointerType
  , parseVariableType
  , betweenParens parseType
  ]

-- | Parses a record type.
parseRecordType :: Parser Type
parseRecordType = Record . Map.fromList <$> betweenBraces (field `sepBy` parseSymbol Comma)
  where
    field = (,) <$> (located parseRegister <* parseSymbol Colon) <*> located parseType

-- | Parses any sort of signed integer type.
parseSignedType :: Parser Type
parseSignedType = fmap Signed . MP.choice $ signed <$> [ 64 ]
 where
   signed n = fromIntegral n <$ parseSymbol (Id ("s" <> Text.pack (show n)))

-- | Parses any sort of unsigned integer type.
parseUnsignedType :: Parser Type
parseUnsignedType = fmap Unsigned . MP.choice $ unsigned <$> [ 64 ]
 where
   unsigned n = fromIntegral n <$ parseSymbol (Id ("u" <> Text.pack (show n)))

-- | Parses a pointer to a type.
parsePointerType :: Parser Type
parsePointerType = parseSymbol Star *> (Ptr <$> located parseType)

-- | Parses a pointer to a stack type.
parseStackPointerType :: Parser Type
parseStackPointerType = parseSymbol Sptr *> (SPtr <$> parseStackType)

-- | Parses a stack type.
parseStackType :: Parser (Located Type)
parseStackType = foldr1 cons <$> (located parseType `sepBy1` parseSymbol DoubleColon)
  where
    cons stack@(_ :@ p) ty = Cons stack ty :@ p

-- | Parses a type variable.
parseVariableType :: Parser Type
parseVariableType = Var <$> parseIdentifier

-- | Parses a type kind.
parseKind :: Parser Kind
parseKind = MP.choice
  [ T8 <$ parseSymbol (Id "T8")
  , Ts <$ parseSymbol (Id "Ts")
  , Ta <$ parseSymbol (Id "Ta") ]

------------------------------------------------------------------------------------------------------------

-- | Parses any kind of expression.
parseExpr :: Parser Expr
parseExpr = parseValueExpr MP.<|> parseAddressExpr

-- | Parses only expressions that cannot be indexed.
parseValueExpr :: Parser Expr
parseValueExpr = lexeme $ MP.choice
  [ Imm <$> located parseImmediate ]

-- | Parses an immediate literal value.
parseImmediate :: Parser Immediate
parseImmediate = MP.choice
  [ I . unLoc <$> parseInteger
  , C . unLoc <$> parseCharacter ]

-- | Parses expressions that can be set and indexed.
parseAddressExpr :: Parser Expr
parseAddressExpr = lexeme $ MP.choice
  [ parseLabel
  , Reg <$> located parseRegister
  , parseIndexedExpr ]

-- | Parses a literal label name.
parseLabel :: Parser Expr
parseLabel = Name <$> parseIdentifier

-- | Parses an indexed addressable expression.
parseIndexedExpr :: Parser Expr
parseIndexedExpr = MP.label "an indexed expression" $
  Indexed <$> located parseSignedInteger
          <*> betweenParens (located parseAddressExpr)

parseSignedInteger :: Parser Integer
parseSignedInteger = (*) <$> sign <*> (unLoc <$> parseInteger)
 where
   sign = MP.choice [ -1 <$ parseSymbol Minus, 1 <$ pure () ]

----------------------------------------------------------------------------------------------------------------

-- | Parses a @mov@ instruction.
parseMov :: Parser Instruction
parseMov =
  parseSymbol Mov *>
    (MOV <$> located parseAddressExpr
         <*> (parseSymbol Comma *> located parseExpr))

-- | Parses a @ret@ instruction.
parseRet :: Parser Instruction
parseRet = RET <$ parseSymbol Ret
