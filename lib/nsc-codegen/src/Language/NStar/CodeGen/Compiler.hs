module Language.NStar.CodeGen.Compiler where

import Control.Monad.Writer (Writer)
import Data.Word (Word8)

type Compiler a = Writer [Word8] a
