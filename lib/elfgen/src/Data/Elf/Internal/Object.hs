module Data.Elf.Internal.Object where

import Data.Elf.Internal.FileHeader
import Data.Elf.Internal.ProgramHeader
import Data.Elf.Internal.SectionHeader
import Data.Word (Word8)

data Object64
  = Obj64
      (Elf_Ehdr 64)   -- ^ The file header
      [Elf_Phdr 64]   -- ^ Programs headers
      [Elf_Shdr 64]   -- ^ Section headers
      [Word8]         -- ^ Raw data
