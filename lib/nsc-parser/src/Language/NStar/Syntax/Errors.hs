{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.NStar.Syntax.Errors where

import qualified Text.Megaparsec as MP
import Error.Diagnose (warn, Report)
import Error.Diagnose.Compat.Megaparsec (HasHints(..))
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
fromLexicalWarning _ = warn "" [] []

instance Show LexicalError where
  show UnrecognizedEscapeSequence = "unrecognized character escape sequence"

instance MP.ShowErrorComponent LexicalError where
  showErrorComponent = show

instance HasHints LexicalError String where
  hints UnrecognizedEscapeSequence =
    ["Valid escape sequences are all documented at <https://github.com/nihil-lang/nsc/blob/develop/docs/escape-sequences.md>."]

data SemanticError
  = NoSuchRegister Token
  deriving (Eq, Ord, Data, Typeable)

data ParseWarning

fromParseWarning :: ParseWarning -> Report String
fromParseWarning _ = warn "" [] []

instance Show SemanticError where
  show (NoSuchRegister t) = "unrecognized register " <> showToken t

instance MP.ShowErrorComponent SemanticError where
  showErrorComponent = show

instance HasHints SemanticError String where
  hints (NoSuchRegister _) =
    [ "Registers are fixed depending on the target architecture."
    , "Registers available in different architectures are documented here: <https://github.com/nihil-lang/nsc/blob/develop/docs/registers.md>." ]
