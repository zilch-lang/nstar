{-# LANGUAGE KindSignatures #-}

module Data.Elf.Internal.Object where

import Data.Elf.Internal.FileHeader
import Data.Elf.Internal.ProgramHeader
import Data.Elf.Internal.SectionHeader
import Data.Word (Word8)
import GHC.TypeNats (Nat)

data Object (n :: Nat)
  = Obj
      (Elf_Ehdr n)   -- ^ The file header
      [Elf_Phdr n]   -- ^ Programs headers
      [Elf_Shdr n]   -- ^ Section headers
      [Word8]         -- ^ Raw data
