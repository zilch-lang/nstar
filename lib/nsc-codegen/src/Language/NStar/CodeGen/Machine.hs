module Language.NStar.CodeGen.Machine where

import Language.NStar.Typechecker.Core (TypedProgram)
import Control.Monad.Writer (execWriter)
import Language.NStar.CodeGen.Machine.X64
import Text.Diagnose (Report)
import Language.NStar.CodeGen.Arch (SupportedArch(..))
import Data.Word (Word8)


compile :: SupportedArch -> TypedProgram -> [Word8]
compile arch prog = execWriter case arch of
  X64 -> compileX64 prog -- error "TODO: opcode compilation not yet implemented for x64"
