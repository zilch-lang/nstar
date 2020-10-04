{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

{-|

  For a complete documentation, please see the one in the <elf.h> header on linux.
-}
module Data.Elf
( -- * Types
  Elf64_Half, Elf64_Word, Elf64_Sword, Elf64_Xword, Elf64_Sxword, Elf64_Addr, Elf64_Off, Elf64_Section, Elf64_Versym, UChar

  -- * File header
, Elf64_Ehdr(..)
  -- ** @'e_ident'@ value
  -- *** Magic number
, ei_mag0, ei_mag1, ei_mag2, ei_mag3
, elfmag0, elfmag1, elfmag2, elfmag3
, elfmag

  ) where

#include <elf.h>

-- For now, let's just focus on the 64 bits format, no matter what the HOST/TARGET arch is.

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int32, Int64)
import Data.Char (ord)



-- | Unsigned 16-bits integer
type Elf64_Half = Word16

-- | Unsigned 32-bits integer
type Elf64_Word = Word32
-- | Signed 32-bits integer
type Elf64_Sword = Int32

-- | Unsigned 64-bits integer
type Elf64_Xword = Word64
-- | Signed 64-bits integer
type Elf64_Sxword = Int64

-- | Unsigned 64-bits integer
type Elf64_Addr = Word64

-- | Unsigned 64-bits integer
type Elf64_Off = Word64

-- | Unsigned 16-bits integer
type Elf64_Section = Word16

-- | Unsigned 16-bits integer
type Elf64_Versym = Elf64_Half

-- | Unsigned 8-bits integer
type UChar = Word8

-------------------------------------------------------------

-- | ELF file header. Appears at the start of every ELF file.
data Elf64_Ehdr
  = Elf64_Ehdr
  { e_ident     :: ![UChar]       -- ^ Magic number and other info (always 16 bytes long)
  , e_type      :: !Elf64_Half    -- ^ Object file type
  , e_machine   :: !Elf64_Half    -- ^ Architecture
  , e_version   :: !Elf64_Word    -- ^ Object file version
  , e_entry     :: !Elf64_Addr    -- ^ Entry point virtual address
  , e_phoff     :: !Elf64_Off     -- ^ Program header table file offset
  , e_shoff     :: !Elf64_Off     -- ^ Section header table file offset
  , e_flags     :: !Elf64_Word    -- ^ Processor-specific flags
  , e_ehsize    :: !Elf64_Half    -- ^ ELF header size in bytes
  , e_phentsize :: !Elf64_Half    -- ^ Program header table entry size
  , e_phnum     :: !Elf64_Half    -- ^ Program header table entry count
  , e_shentsize :: !Elf64_Half    -- ^ Section header table entry size
  , e_shnum     :: !Elf64_Half    -- ^ Section header table entry count
  , e_shstrndx  :: !Elf64_Half    -- ^ Section header string table index
  }

-- | File identification byte indexes
ei_mag0, ei_mag1, ei_mag2, ei_mag3 :: Int
ei_mag0 = {#const EI_MAG0#}
ei_mag1 = {#const EI_MAG1#}
ei_mag2 = {#const EI_MAG2#}
ei_mag3 = {#const EI_MAG3#}

-- | Magic number bytes
elfmag0, elfmag1, elfmag2, elfmag3 :: UChar
elfmag0 = {#const ELFMAG0#}
elfmag1 = fromIntegral $ ord {#const ELFMAG1#}
elfmag2 = fromIntegral $ ord {#const ELFMAG2#}
elfmag3 = fromIntegral $ ord {#const ELFMAG3#}

-- | Conglomeration of the identification bytes
elfmag :: [UChar]
elfmag = [elfmag0, elfmag1, elfmag2, elfmag3]

