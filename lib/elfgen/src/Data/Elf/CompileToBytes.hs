{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Elf.CompileToBytes
( -- * Data kinds
  Size(..), Endianness(..)
  -- * Compilation
, compile
  -- * Re-exports
, module Data.ByteString.Lazy
) where

import Data.Elf.Internal.BusSize (Size(..))
import Data.Elf.Internal.Endianness (Endianness(..))
import Data.Elf.Internal.Serialize (Serializable(..), runPut)
import Data.Elf.Object (ElfObject)
import Data.ByteString.Lazy (ByteString, writeFile)
import Data.Elf.Internal.Compile (unabstract)
import Data.Elf.Internal.Object (Object)

compile :: forall (n :: Size) (e :: Endianness).
           Serializable n e (Object S64) => ElfObject -> ByteString
compile = runPut . put @n @e . unabstract
