module Language.NStar.CodeGen.Machine where

import Language.NStar.Typechecker.Core (TypedProgram)
import Control.Monad.Writer (execWriter)
import Language.NStar.CodeGen.Machine.X64
import Language.NStar.CodeGen.Arch (SupportedArch(..))
import Language.NStar.CodeGen.Compiler (MachineInfo)
import Language.NStar.CodeGen.PreProcessor


compile :: SupportedArch -> TypedProgram -> MachineInfo
compile arch prog = execWriter case arch of
  X64 -> compileX64 (preprocessX64 prog) -- error "TODO: opcode compilation not yet implemented for x64"
