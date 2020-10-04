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
  -- *** ELF class
, ei_class, elfclassnone, elfclass32, elfclass64, elfclassnum
  -- *** Endianness
, ei_data, elfdatanone, elfdata2lsb, elfdata2msb, elfdatanum
  -- *** Version
, ei_version
  -- *** OS ABI identification
, ei_osabi
, elfosabi_none, elfosabi_sysv, elfosabi_hpux, elfosabi_netbsd, elfosabi_gnu, elfosabi_linux, elfosabi_solaris, elfosabi_aix
, elfosabi_irix, elfosabi_freebsd, elfosabi_tru64, elfosabi_modesto, elfosabi_openbsd, elfosabi_arm_aeabi, elfosabi_arm, elfosabi_standalone

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

-- | File class byte index
ei_class :: Int
ei_class = {#const EI_CLASS#}

-- | Invalid class
elfclassnone :: UChar
elfclassnone = {#const ELFCLASSNONE#}
-- | 32-bit objects
elfclass32 :: UChar
elfclass32 = {#const ELFCLASS32#}
-- | 64-bit objects
elfclass64 :: UChar
elfclass64 = {#const ELFCLASS64#}
-- |
elfclassnum :: UChar
elfclassnum = {#const ELFCLASSNUM#}

-- | Data encoding byte index
ei_data :: Int
ei_data = {#const EI_DATA#}

-- | Invalid data encoding
elfdatanone :: UChar
elfdatanone = {#const ELFDATANONE#}
-- | 2's complement, little endian
elfdata2lsb :: UChar
elfdata2lsb = {#const ELFDATA2LSB#}
-- | 2's complement, big endian
elfdata2msb :: UChar
elfdata2msb = {#const ELFDATA2MSB#}
-- |
elfdatanum :: UChar
elfdatanum = {#const ELFDATANUM#}

-- | File version byte index (value must be @'ev_current'@)
ei_version :: Int
ei_version = {#const EI_VERSION#}

-- | OS ABI identification byte index
ei_osabi :: Int
ei_osabi = {#const EI_OSABI#}

-- | UNIX System V ABI
elfosabi_none, elfosabi_sysv :: UChar
elfosabi_none = {#const ELFOSABI_NONE#}
elfosabi_sysv = elfosabi_none
-- | HP-UX
elfosabi_hpux :: UChar
elfosabi_hpux = {#const ELFOSABI_HPUX#}
-- | NetBSD
elfosabi_netbsd :: UChar
elfosabi_netbsd = {#const ELFOSABI_NETBSD#}
-- | Object uses GNU ELF extensions
elfosabi_gnu :: UChar
elfosabi_gnu = {#const ELFOSABI_GNU#}
-- | Compatibility alias on @'elfosabi_gnu'@
elfosabi_linux :: UChar
elfosabi_linux = elfosabi_gnu
-- | Sun Solaris
elfosabi_solaris :: UChar
elfosabi_solaris = {#const ELFOSABI_SOLARIS#}
-- | IBM AIX
elfosabi_aix :: UChar
elfosabi_aix = {#const ELFOSABI_AIX#}
-- | SGI Irix
elfosabi_irix :: UChar
elfosabi_irix = {#const ELFOSABI_IRIX#}
-- | FreeBSD
elfosabi_freebsd :: UChar
elfosabi_freebsd = {#const ELFOSABI_FREEBSD#}
-- | Compaq TRU64 UNIX
elfosabi_tru64 :: UChar
elfosabi_tru64 = {#const ELFOSABI_TRU64#}
-- | Novell Modesto
elfosabi_modesto :: UChar
elfosabi_modesto = {#const ELFOSABI_MODESTO#}
-- | OpenBSD
elfosabi_openbsd :: UChar
elfosabi_openbsd = {#const ELFOSABI_OPENBSD#}
-- | ARM EABI
elfosabi_arm_aeabi :: UChar
elfosabi_arm_aeabi = {#const ELFOSABI_ARM_AEABI#}
-- | ARM
elfosabi_arm :: UChar
elfosabi_arm = {#const ELFOSABI_ARM#}
-- | Standalone (embedded) application
elfosabi_standalone :: UChar
elfosabi_standalone = {#const ELFOSABI_STANDALONE#}
