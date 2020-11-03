{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Elf.Internal.Object where

import Data.Elf.Internal.FileHeader
import Data.Elf.Internal.ProgramHeader
import Data.Elf.Internal.SectionHeader
import Data.Word (Word8)
import GHC.TypeNats (Nat)
import Data.Elf.Internal.BusSize (Size(..))
import Data.Elf.Internal.Serialize (Serializable(..))

data Object (n :: Size)
  = Obj
      (Elf_Ehdr n)   -- ^ The file header
      [Elf_Phdr n]   -- ^ Programs headers
      [Elf_Shdr n]   -- ^ Section headers
      [Word8]         -- ^ Raw data

instance ( Serializable n e (Elf_Ehdr n)
         , Serializable n e (Elf_Phdr n)
         , Serializable n e (Elf_Shdr n)
         ) => Serializable n e (Object n) where
  put e (Obj fileHeader programHeaders sectionHeaders binaryData) = do
    put @n e fileHeader
    put @n e programHeaders
    put @n e sectionHeaders
    put @n e binaryData
