module Language.NStar.CodeGen.Compiler where

import Control.Monad.Writer (Writer, WriterT)
import Data.Word (Word8)
import Language.NStar.CodeGen.Errors

type Compiler a = WriterT [CodegenError] (Writer [Word8]) a
