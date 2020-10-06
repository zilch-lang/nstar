{-# LANGUAGE BangPatterns #-}

module Data.Elf.Object where

import Data.Elf.Types
import Data.Elf.FileHeader (Elf64_Ehdr)
import Data.Elf.SectionHeader (Elf64_Shdr)
import Data.Elf.ProgramHeader (Elf64_Phdr)

-- | An object file layout.
data Object64
  = Object64
  { o_header    :: Elf64_Ehdr   -- ^ The ELF header
  , p_table     :: [Elf64_Phdr] -- ^ Program headers
  , s_table     :: [Elf64_Shdr] -- ^ Section headers
  , bytes       :: ![UChar]     -- ^ Data stored in the object file
  }
