module Language.NStar.CodeGen.Machine.X64.Ret
( -- * Instruction encoding
  -- $encoding

  -- * Compiling
compileRet
  ) where

import Language.NStar.CodeGen.Machine.Internal.Intermediate (TypeContext, InterOpcode(..))
import Language.NStar.CodeGen.Compiler (Compiler)
import Internal.Error (internalError)
import Language.NStar.Syntax.Core (Type(RegisterContT))
import Data.Located (unLoc, Located((:@)))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.Typechecker.Core (Register)

{- $encoding

-}

compileRet :: Register -> Compiler [InterOpcode]
compileRet r  = pure [Byte 0xFF, modRM 0b11 0x4 (registerNumber r)]
compileRet r  = internalError $ "Unsupported instruction 'ret " <> show r <> "'."
