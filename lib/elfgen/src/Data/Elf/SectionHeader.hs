{-# LANGUAGE BangPatterns #-}

module Data.Elf.SectionHeader
( SectionHeader(..)
, module Data.Elf.SectionHeader.Flags
) where

import Data.Elf.Types
import Data.Word (Word8)
import Data.Elf.SectionHeader.Flags

-- | Section header
data SectionHeader n
  -- | Section header table entry unused
  = SNull
  -- | Program data
  | SProgBits
      String
      [Word8]
      (SFlags n)
  -- | Program space with no data (bss)
  | SNoBits
      String
      Integer   -- ^ Space size
      (SFlags n)
  -- | String table
  | SStrTab
      String
      [String]
deriving instance ValueSet n => Eq (SectionHeader n)
deriving instance ValueSet n => Ord (SectionHeader n)
