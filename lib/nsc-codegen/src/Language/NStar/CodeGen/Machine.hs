module Language.NStar.CodeGen.Machine where

import Language.NStar.Typechecker.Core (TypedProgram)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (pack)
import Control.Monad.Writer (execWriter)
import Language.NStar.CodeGen.Machine.X64
import Text.Diagnose (Report)
import Language.NStar.CodeGen.Arch (SupportedArch(..))


compile :: SupportedArch -> TypedProgram -> ByteString
compile arch prog = BS.pack $ execWriter case arch of
  X64 -> compileX64 prog -- error "TODO: opcode compilation not yet implemented for x64"
