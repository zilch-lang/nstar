{-# LANGUAGE BangPatterns #-}

module Data.Elf.Object where

import Data.Elf.Types
import Data.Elf.FileHeader (ElfHeader)
import Data.Elf.SectionHeader (SectionHeader)
import Data.Elf.ProgramHeader (ProgramHeader)
import Data.Elf.Symbol (ElfSymbol)

-- | An object file layout.
data ElfObject n
  = ElfObject
  { fileHeader    :: ElfHeader n           -- ^ The ELF header
  , segments      :: [ProgramHeader n]     -- ^ Program headers
  , sections      :: [SectionHeader n]     -- ^ Section headers
  }
