module Data.Elf.Internal.Compile.ForArch where

import Data.Kind (Type)
import Data.Elf.Internal.BusSize (Size)

class CompileFor (n :: Size) (a :: Size -> Type) (b :: Size -> Type) where
  -- | Compiles a value of type @a@ into a value of type @b@ parameterized by the target architecture bus size @n@.
  compileFor :: a n -> b n
