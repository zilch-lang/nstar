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
import Data.Elf.Internal.Compile (unabstract, CompileFor)
import Data.Elf.Internal.Object (Object)
import Data.Elf.Types (ReifySize)

-- | Completely compiles an abstract ELF file down to a 'ByteString' ready to be written to a file.
--
--   The ELF object undergoes two stages:
--
--   [An unabstracting stage] Where the abstract ELF file is broken down into a concrete representation ready to be serialized.
--   [A serialization stage] Where the concrete representation is compiled down to simple binary data following the ELF specifications.
compile :: forall (n :: Size) e.
           ( ReifySize n
           , CompileFor n ElfObject Object
           , Serializable n e (Object n)
           ) => Endianness e -> ElfObject n -> IO ByteString
compile e o = do
  obj <- unabstract o
  pure (runPut (put @n e obj))
