module Language.NStar.CodeGen.Machine where

import Language.NStar.Typechecker.Core (TypedProgram)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import Control.Monad.Writer (runWriter, execWriterT, tell)
import Language.NStar.CodeGen.Errors
import Data.Bifunctor (second)

data SupportedArch
  = X64


compile :: SupportedArch -> TypedProgram -> ([CodegenError], ByteString)
compile arch prog = second toLazyByteString $ runWriter $ execWriterT case arch of
  X64 -> error "TODO: opcode compilation not yet implemented for x64"
