{-# LANGUAGE BangPatterns #-}

module Data.Elf.Object where

import Data.Elf.Types
import Data.Elf.FileHeader (ElfHeader)
import Data.Elf.SectionHeader (SectionHeader)
import Data.Elf.ProgramHeader (ProgramHeader)
import Data.Elf.SymbolTable (ElfSymbol)

-- | An object file layout.
data Object64
  = Object64
  { fileHeader    :: ElfHeader           -- ^ The ELF header
  , segments      :: [ProgramHeader]     -- ^ Program headers
  , sections      :: [SectionHeader]     -- ^ Section headers
  , symbols       :: [ElfSymbol]         -- ^ Symbol table
  }
