{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
--  Module: Language.NStar.Syntax.Lexer
--  Description: NStar's lexer
--  Copyright: (c) Mesabloo, 2020
--  License: BSD3
--  Stability: experimental
module Language.NStar.Syntax.Lexer (lexFile) where

import Console.NStar.Flags (LexerFlags (..))
import Control.Applicative (liftA2)
import Control.Monad.Writer (MonadWriter, runWriterT)
import Data.Bifunctor (bimap, second)
import Data.Char (isSpace)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text (pack, toLower)
import Error.Diagnose (Diagnostic, addReport, def)
import Error.Diagnose.Compat.Megaparsec
import Language.NStar.Syntax.Core (LToken, Token (..))
import Language.NStar.Syntax.Errors
import Language.NStar.Syntax.Internal (located)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
import Text.Read (readMaybe)

-- type Lexer a = WriterT [LexicalWarning] (MP.Parsec LexicalError Text) a

type MonadLexer m = (?lexerFlags :: LexerFlags, MonadWriter [LexicalWarning] m, MP.MonadParsec LexicalError Text m)

-- | @space@ only parses any whitespace (not accounting for newlines and vertical tabs) and discards them.
space :: MonadLexer m => m ()
space = MPL.space MP.empty MP.empty MP.empty

-- | @lexeme p@ applies @p@ and ignores any space after, if @p@ succeeds. If @p@ fails, @lexeme p@ also fails.
lexeme :: MonadLexer m => m a -> m a
lexeme = MPL.lexeme space

-- | @symbol str@ tries to parse @str@ exactly, and discards spaces after.
symbol :: MonadLexer m => Text -> m Text
symbol = MPL.symbol space

---------------------------------------------------------------------------------------------------------------------------

-- | Runs the program lexer on a given file, and returns either all the tokens identified in the source, or an error.
lexFile ::
  (?lexerFlags :: LexerFlags) =>
  -- | File name
  FilePath ->
  -- | File content
  Text ->
  Either (Diagnostic String) ([LToken], Diagnostic String)
lexFile file input =
  bimap (errorDiagnosticFromBundle "Lexical error on input" Nothing) (second toDiagnostic) $ MP.runParser (runWriterT lexProgram) file input
  where
    toDiagnostic warns = foldl' addReport def (fromLexicalWarning <$> warns)

-- | Transforms a source code into a non-empty list of tokens (accounting for 'EOF').
lexProgram :: MonadLexer m => m [LToken]
lexProgram = lexeme (pure ()) *> ((<>) <$> tokens <*> ((: []) <$> eof))
  where
    tokens =
      MP.many . lexeme $
        MP.choice
          [comment, identifierOrKeyword, literal, anySymbol, eol, whitespace]

-- | Parses an end of line and returns 'EOL'.
eol :: MonadLexer m => m LToken
eol = located (EOL <$ MPC.eol)

-- | Parses an end of file and returns 'EOF'.
--
--   Note that all files end with 'EOF'.
eof :: MonadLexer m => m LToken
eof = located (EOF <$ MP.eof)

whitespace :: MonadLexer m => m LToken
whitespace = located $ HSpace <$ MP.some (MP.satisfy (and . (<$> [isSpace, (/= '\n'), (/= '\r'), (/= '\v'), (/= '\f')]) . (&)))

-- | Parses any kind of comment, inline or multiline.
comment :: MonadLexer m => m LToken
comment = located (inline MP.<|> multiline)
  where
    inline = InlineComment . Text.pack <$> (symbol "#" *> MP.manyTill MP.anySingle (MP.lookAhead (() <$ MPC.eol MP.<|> MP.eof)))
    multiline = MultilineComment . Text.pack <$> (symbol "/*" *> MP.manyTill MP.anySingle (symbol "*/"))

-- | Parses any symbol like @[@ or @,@.
anySymbol :: MonadLexer m => m LToken
anySymbol = located . MP.choice $ uncurry sat <$> symbols
  where
    sat ret sym = ret <$ MPC.string sym

    symbols =
      [ (LParen, "("),
        (LBrace, "{"),
        (LBracket, "["),
        (LAngle, "<"),
        (RParen, ")"),
        (RBrace, "}"),
        (RBracket, "]"),
        (RAngle, ">"),
        (Star, "*"),
        (Dollar, "$"),
        (Percent, "%"),
        (Comma, ","),
        (DoubleColon, "::"),
        (Colon, ":"),
        (Dot, "."),
        (Arrow, "->"),
        (Arrow, "→"),
        (Minus, "-"),
        (Plus, "+"),
        (Equal, "="),
        (Pipe, "|"),
        (Semi, ";"),
        (Bang, "!")
      ]

-- | Tries to parse an identifier. If the result appears to be a keyword, it instead returns a keyword.
identifierOrKeyword :: MonadLexer m => m LToken
identifierOrKeyword = located do
  transform . Text.pack
    <$> ( (:) <$> (MPC.letterChar MP.<|> MPC.char '_')
            <*> MP.many (MPC.alphaNumChar MP.<|> MPC.char '_')
        )
  where
    transform :: Text -> Token
    transform w@(Text.toLower -> rw) = case rw of
      -- Instructions
      "mv" -> Mv
      "ret" -> Ret
      "jmp" -> Jmp
      "call" -> Call
      "nop" -> Nop
      "salloc" -> Salloc
      "sfree" -> Sfree
      "sld" -> Sld
      "sst" -> Sst
      "ld" -> Ld
      "st" -> St
      "sref" -> Sref
      "and" -> And
      "or" -> Or
      "xor" -> Xor
      "not" -> Not
      "cmvz" -> Cmvz
      "cmvnz" -> Cmvnz
      "add" -> Add
      "shiftl" -> Shiftl
      "shiftr" -> Shiftr
      "sub" -> Sub
      "mul" -> Mul
      "cmvl" -> Cmvl
      "cmvge" -> Cmvge
      "cmvle" -> Cmvle
      "cmvg" -> Cmvg
      "cmve" -> Cmve
      "cmvne" -> Cmvne
      "cjz" -> Cjz
      "cjnz" -> Cjnz
      "cjl" -> Cjl
      "cjge" -> Cjge
      "cjle" -> Cjle
      "cjg" -> Cjg
      -- Registers
      "r0" -> R0'
      "r1" -> R1'
      "r2" -> R2'
      "r3" -> R3'
      "r4" -> R4'
      "r5" -> R5'
      -- Keywords
      "forall" -> Forall
      "∀" -> Forall
      "unsafe" -> UnSafe
      "section" -> Section
      "include" -> Include
      "ta" -> TaK
      "tc" -> TcK
      "ts" -> TsK
      i ->
        if
            | Just ('t', ds) <- T.uncons i -> case readMaybe @Integer $ T.unpack ds of
              Nothing -> Id w
              Just n -> TnK n
            | otherwise -> Id w

-- | Parses a literal value (integer or character).
literal :: MonadLexer m => m LToken
literal =
  located $
    MP.choice
      [ Integer <$> prefixed "0b" (Text.pack <$> MP.some binary),
        Integer <$> prefixed "0x" (Text.pack <$> MP.some hexadecimal),
        Integer <$> prefixed "0o" (Text.pack <$> MP.some octal),
        Integer . Text.pack <$> MP.some decimal,
        Str . Text.pack <$> (MPC.char '"' *> MP.manyTill charLiteral (MPC.char '"')),
        Char <$> MP.between (MPC.char '\'') (MPC.char '\'') charLiteral
      ]
  where
    charLiteral = escapeChar MP.<|> MP.satisfy (liftA2 (&&) (/= '\n') (/= '\r'))
    escapeChar = MPC.char '\\' *> (codes MP.<|> MP.customFailure UnrecognizedEscapeSequence)
    codes =
      MP.choice
        [ '\a' <$ MPC.char 'a', -- Bell
          '\b' <$ MPC.char 'b', -- Backspace
          '\ESC' <$ MPC.char 'e', -- Escape character
          '\f' <$ MPC.char 'f', -- Formfeed page break
          '\n' <$ MPC.char 'n', -- Newline (Line feed)
          '\r' <$ MPC.char 'r', -- Carriage return
          '\t' <$ MPC.char 't', -- Horizontal tab
          '\v' <$ MPC.char 'v', -- Vertical tab
          '\\' <$ MPC.char '\\', -- Backslash
          '\'' <$ MPC.char '\'', -- Apostrophe
          '"' <$ MPC.char '"', -- Double quotation mark
          '\0' <$ MPC.char '0' -- Null character
          -- TODO: add support for unicode escape sequences "\uHHHH" and "\uHHHHHHHH"
        ]

    binary, octal, decimal, hexadecimal :: MonadLexer m => m Char
    binary = MP.choice $ MPC.char <$> ['0', '1']
    octal = MP.choice $ binary : (MPC.char <$> ['2', '3', '4', '5', '6', '7'])
    decimal = MP.choice $ octal : (MPC.char <$> ['8', '9'])
    hexadecimal = MP.choice $ decimal : (MPC.char' <$> ['a', 'b', 'c', 'd', 'e', 'f'])
    {-# INLINE binary #-}
    {-# INLINE octal #-}
    {-# INLINE decimal #-}
    {-# INLINE hexadecimal #-}

    prefixed :: (MonadLexer m, c ~ MP.Tokens Text) => c -> m c -> m c
    prefixed prefix p = (<>) <$> MPC.string' prefix <*> p
    {-# INLINE prefixed #-}
