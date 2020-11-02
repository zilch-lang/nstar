{-# LANGUAGE BangPatterns #-}

module Data.Elf.ProgramHeader
( ProgramHeader(..)
, section, binaryData
, module Data.Elf.ProgramHeader.Flags
) where

import Data.Elf.Types
import Data.Elf.ProgramHeader.Flags


-- | Program segment header
data ProgramHeader
  -- | Entry for header table itself
  = PPhdr
  -- | Program header table entry unused
  | PNull
  -- | Loadable program segment
  | PLoad
      (Either String [UChar])     -- ^ Either the name of a section or concrete binary data.
      PFlags
  -- | Program interpreter
  | PInterp
      String     -- ^ Path to the dynamic interpreter
      PFlags
  deriving (Eq, Ord)

-- | Indicates that we want to fetch data from a specific section. This essentially is an alias for 'Left', to be used with data constructors from 'ProgramHeader'
--   in order to prevent duplicated binary data in the resulting ELF file (note that this duplication causes all sorts of problems like non-allocation of progbits).
section :: String -> Either String [UChar]
section = Left

-- | Indicates that we want to have direct binary data which is not present in any other section. This is an alias for 'Right'.
binaryData :: [UChar] -> Either String [UChar]
binaryData = Right
