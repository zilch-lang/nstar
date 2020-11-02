{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

{-|

  For a complete documentation, please see the one in the <https://code.woboq.org/userspace/glibc/elf/elf.h.html elf.h> header on linux.
-}
module Data.Elf
( -- * Types
  module Data.Elf.Types
  -- * File header
, module Data.Elf.FileHeader
  -- * Section header
, module Data.Elf.SectionHeader
  -- * Program header
, module Data.Elf.ProgramHeader


  -- * ELF structure
, module Data.Elf.Object
) where

-- For now, let's just focus on the 64 bits format, no matter what the HOST/TARGET arch is.

import Data.Elf.Types
import Data.Elf.FileHeader
import Data.Elf.SectionHeader
import Data.Elf.ProgramHeader
import Data.Elf.Object
import Data.ByteString (ByteString)
