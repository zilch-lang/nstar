{-# LANGUAGE BangPatterns #-}

module Data.Elf.ProgramHeader
( ProgramHeader(..)
, module Data.Elf.ProgramHeader.Flags
) where

import Data.Elf.Types
import Data.Elf.ProgramHeader.Flags


-- | Program segment header
data ProgramHeader
  -- | Program header table entry unused
  = PNull
  -- | Loadable program segment
  | PLoad
      [UChar]
      PFlags
  -- | Program interpreter
  | PInterp
      String     -- ^ Path to the dynamic interpreter
      PFlags
