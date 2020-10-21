{-# LANGUAGE BangPatterns #-}

module Data.Elf.Object where

import Data.Elf.Types
import Data.Elf.FileHeader (ElfHeader)
import Data.Elf.SectionHeader (SectionHeader)
import Data.Elf.ProgramHeader (Elf64_Phdr)

-- | An object file layout.
data Object64
  = Object64
  { o_header    :: ElfHeader           -- ^ The ELF header
  , p_table     :: [Elf64_Phdr]        -- ^ Program headers
  , s_table     :: [SectionHeader]     -- ^ Section headers
  , bytes       :: ![UChar]            -- ^ Data stored in the object file
  }
