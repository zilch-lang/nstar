module Language.NStar.CodeGen.Machine.X64.Call
( -- * Instruction encoding
  -- $encoding

  -- * Compiling
compileCall
) where

import Language.NStar.Syntax.Core (Expr(..), Type)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (TypeContext, InterOpcode(..))
import Data.Located (unLoc)
import Internal.Error (internalError)
import Data.Text (Text)

{- $encoding

-}

compileCall :: Text -> Compiler [InterOpcode]
compileCall l = pure [Byte 0xE9, Symbol32 l 0]
compileCall e = internalError $ "Unsupported instruction 'call " <> show e <> "'."
