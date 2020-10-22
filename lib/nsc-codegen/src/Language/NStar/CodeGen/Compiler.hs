module Language.NStar.CodeGen.Compiler where

import Control.Monad.Writer (Writer, WriterT)
import Data.ByteString.Builder (Builder)
import Language.NStar.CodeGen.Errors

type Compiler a = WriterT [CodegenError] (Writer Builder) a
