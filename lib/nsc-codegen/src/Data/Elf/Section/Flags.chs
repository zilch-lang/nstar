{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Elf.Section.Flags
( SFlags
) where

import Data.Bits (Bits)
import Data.Elf.Types (Elf64_Xword)

type SFlags = Flag
-- | Section flags
newtype Flag = Flag Elf64_Xword
  deriving (Show, Eq, Ord, Num, Bits)
