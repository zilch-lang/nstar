{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.NStar.Syntax.Errors where

import qualified Text.Megaparsec as MP
import Language.NStar.Syntax.Hints
import Text.Diagnose (reportWarning, Report, hint)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Language.NStar.Syntax.Core (Token)
import Language.NStar.Syntax.Internal (showToken)

-- | The type of possible custom lexical errors, detected during lexing.
data LexicalError
  = UnrecognizedEscapeSequence  -- ^ An escape sequence as not been recognized or is not valid
  deriving (Ord, Eq, Typeable, Data, Read)

data LexicalWarning

fromLexicalWarning :: LexicalWarning -> Report String
fromLexicalWarning _ = reportWarning "" [] []

instance Show LexicalError where
  show UnrecognizedEscapeSequence = "unrecognized character escape sequence"

instance MP.ShowErrorComponent LexicalError where
  showErrorComponent = show

instance Hintable LexicalError String where
  hints UnrecognizedEscapeSequence =
    [hint "Valid escape sequences are all documented at <https://github.com/nihil-lang/nsc/blob/develop/docs/escape-sequences.md>."]

data SemanticError
  = NoSuchRegister Token
  deriving (Eq, Ord, Data, Typeable)

data ParseWarning

fromParseWarning :: ParseWarning -> Report String
fromParseWarning _ = reportWarning "" [] []

instance Show SemanticError where
  show (NoSuchRegister t) = "unrecognized register " <> showToken t

instance MP.ShowErrorComponent SemanticError where
  showErrorComponent = show

instance Hintable SemanticError String where
  hints (NoSuchRegister _) =
    [ hint "Registers are fixed depending on the target architecture."
    , hint "Registers available in different architectures are documented here: <https://github.com/nihil-lang/nsc/blob/develop/docs/registers.md>." ]
