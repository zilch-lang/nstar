module Language.NStar.CodeGen.Machine where

import Language.NStar.Typechecker.Core (TypedProgram)
import Data.ByteString (ByteString)
import Control.Monad.Writer (runWriter, execWriterT, tell)
import Language.NStar.CodeGen.Errors

data SupportedArch
  = X64


compile :: SupportedArch -> TypedProgram -> ([CodegenError], ByteString)
compile arch prog = runWriter $ execWriterT case arch of
  X64 -> error "TODO: opcode compilation not yet implemented for x64"
