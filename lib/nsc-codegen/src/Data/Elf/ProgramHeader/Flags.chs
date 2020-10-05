{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Elf.ProgramHeader.Flags
( PFlags


) where

import Data.Bits (Bits, shiftL)
import Data.Elf.Types

type PFlags = Flag

newtype Flag = Flag Elf64_Word
  deriving (Show, Eq, Ord, Bits, Num)
