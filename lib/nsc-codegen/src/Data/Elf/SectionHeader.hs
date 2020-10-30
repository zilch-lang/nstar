{-# LANGUAGE BangPatterns #-}

module Data.Elf.SectionHeader
( SectionHeader(..)
, module Data.Elf.SectionHeader.Flags
) where

import Data.Elf.Types
import Data.Word (Word8)
import Data.Elf.SectionHeader.Flags

-- | Section header
data SectionHeader
  -- | Section header table entry unused
  = SNull
  -- | Program data
  | SProgBits
      [UChar]
      SFlags
  -- | Program space with no data (bss)
  | SNoBits
      [UChar]
      SFlags
