{-# LANGUAGE BangPatterns #-}

module Data.Elf.ProgramHeader
( ProgramHeader(..)
, section, binaryData
, module Data.Elf.ProgramHeader.Flags
) where

import Data.Elf.Types
import Data.Elf.ProgramHeader.Flags
import Data.Word (Word8)


-- | Program segment header
data ProgramHeader n
  -- | Entry for header table itself
  = PPhdr
  -- | Program header table entry unused
  | PNull
  -- | Loadable program segment
  | PLoad
      (Either String [Word8])     -- ^ Either the name of a section or concrete binary data.
      (PFlags n)
  -- | Program interpreter
  | PInterp
      String     -- ^ Path to the dynamic interpreter
      (PFlags n)
deriving instance ValueSet n => Eq (ProgramHeader n)
deriving instance ValueSet n => Ord (ProgramHeader n)

-- | Indicates that we want to fetch data from a specific section. This essentially is an alias for 'Left', to be used with data constructors from 'ProgramHeader'
--   in order to prevent duplicated binary data in the resulting ELF file (note that this duplication causes all sorts of problems like non-allocation of progbits).
section :: String -> Either String [Word8]
section = Left

-- | Indicates that we want to have direct binary data which is not present in any other section. This is an alias for 'Right'.
binaryData :: [Word8] -> Either String [Word8]
binaryData = Right
