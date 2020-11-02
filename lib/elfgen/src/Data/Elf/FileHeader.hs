{-# LANGUAGE BangPatterns #-}

{-# OPTIONS -Wno-duplicate-exports #-}

module Data.Elf.FileHeader
( ElfHeader(..)
, Class(..)
, Encoding(..)
, OSABI(..)
, ObjFileType(..)
, Arch(..)
, Version(..)
, module Data.Elf.FileHeader.Flags
) where

import Data.Elf.Types
import Data.Elf.FileHeader.Flags
import Data.Word (Word8)



data ElfHeader
  = ElfHeader
      !Class         -- ^ ELF class (32-bit or 64-bit object)
      !Encoding      -- ^ Data encoding (little endian or big endian)
      !OSABI         -- ^ OS ABI identification
      !Word8         -- ^ ABI version
      !ObjFileType   -- ^ Object file type
      !Arch          -- ^ Architecture
      !Version       -- ^ Object file version (@'ev_current'@ or @'ev_none'@)
      !EFlags        -- ^ Processor-specific flags

-- | ELF class
data Class
  = C_None  -- ^ Invalid class
  | C_32    -- ^ 32-bit object
  | C_64    -- ^ 64-bit object

-- | Data encoding
data Encoding
  = D_None    -- ^ Invalid data encoding
  | D_2LSB    -- ^ 2's complement, little endian
  | D_2MSB    -- ^ 2's complement, big endian

-- | OS ABI identification
data OSABI
  = OSABI_None      -- ^ Unix System V ABI
  | OSABI_SysV      -- ^ Same as 'OSABI_None'

-- | Legal values for @'e_type'@.
data ObjFileType
  = ET_None        -- ^ No file type
  | ET_Rel         -- ^ Relocatable file
  | ET_Exec        -- ^ Executable file
  | ET_Dyn         -- ^ Shared object file
  | ET_Core        -- ^ Core file

-- | Legal values for @'e_machine'@.
data Arch
  = EM_None             -- ^ No machine
  | EM_sparc            -- ^ SUN SPARC
  | EM_x86_64           -- ^ AMD x86-64 architecture
  | EM_arm              -- ^ ARM

data Version
  = EV_None        -- ^ Invalid ELF version
  | EV_Current     -- ^ Current version
