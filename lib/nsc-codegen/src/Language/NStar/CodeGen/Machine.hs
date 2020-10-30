module Language.NStar.CodeGen.Machine where

import Language.NStar.Typechecker.Core (TypedProgram)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (pack)
import Control.Monad.Writer (runWriter, execWriterT)
import Language.NStar.CodeGen.Errors
import Data.Bifunctor (bimap)
import Language.NStar.CodeGen.Machine.X64
import Text.Diagnose (Report)

data SupportedArch
  = X64


compile :: SupportedArch -> TypedProgram -> ([Report String], ByteString)
compile arch prog = bimap (fmap fromCodegenWarning) BS.pack . runWriter $ execWriterT case arch of
  X64 -> compileX64 prog -- error "TODO: opcode compilation not yet implemented for x64"
