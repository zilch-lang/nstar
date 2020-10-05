{-# LANGUAGE BangPatterns #-}

module Data.Elf.FileHeader
( Elf64_Ehdr(..)
  -- ** @'e_ident'@

  -- $e_ident
, elfosabi_none, elfosabi_sysv

  -- ** @'e_type'@
, ObjFileType(..)

  -- ** @'e_machine'@
, Arch(..)

  -- ** @'e_version'@
, ev_none, ev_current

  -- ** @'e_flags'@
, module Data.Elf.FileHeader.Flags
) where

#include <elf.h>

import Data.Elf.Types
import Data.Elf.FileHeader.Flags
import Data.Word (Word8)

-- | The ELF file header. This appears at the start of every ELF file.
data Elf64_Ehdr
  = Elf64_Ehdr
  { e_ident     :: ![UChar]      -- ^ Magic number and other info
  , e_type      :: ObjFileType   -- ^ Object file type
  , e_machine   :: Arch          -- ^ Architecture
  , e_version   :: !Elf64_Word   -- ^ Object file version
  , e_entry     :: !Elf64_Addr   -- ^ Entry point virtual address
  , e_phoff     :: !Elf64_Off    -- ^ Program header table file offset
  , e_shoff     :: !Elf64_Off    -- ^ Section header table file offset
  , e_flags     :: !EFlags       -- ^ Processor-specific flags
  , e_ehsize    :: !Elf64_Half   -- ^ ELF header size in bytes
  , e_phentsize :: !Elf64_Half   -- ^ Program header table entry size
  , e_phnum     :: !Elf64_Half   -- ^ Program header table entry count
  , e_shentsize :: !Elf64_Half   -- ^ Section header table entry size
  , e_shnum     :: !Elf64_Half   -- ^ Section header table entry count
  , e_shstrndx  :: !Elf64_Half   -- ^ Section header string table index
  }

-- | Legal values for @'e_type'@.
data ObjFileType
  = ET_None        -- ^ No file type
  | ET_Rel         -- ^ Relocatable file
  | ET_Exec        -- ^ Executable file
  | ET_Dyn         -- ^ Shared object file
  | ET_Core        -- ^ Core file
  | ET_OS Word8    -- ^ OS-specific (between @<https://code.woboq.org/userspace/glibc/elf/elf.h.html#168 ET_LOOS>@
                   --                and @<https://code.woboq.org/userspace/glibc/elf/elf.h.html#169 ET_HIOS>@)
  | ET_Proc Word8  -- ^ Processor-specific (between @<https://code.woboq.org/userspace/glibc/elf/elf.h.html#170 ET_LOPROC>@
                   --   and @<https://code.woboq.org/userspace/glibc/elf/elf.h.html#171 ET_HIPROC>@)

-- | Legal values for @'e_machine'@.
data Arch
  = EM_None             -- ^ No machine
  | EM_sparc            -- ^ SUN SPARC
  | EM_x86_64           -- ^ AMD x86-64 architecture
  | EM_arm              -- ^ ARM


-- Versions

-- | Invalid ELF version
ev_none :: Elf64_Word
ev_none = {#const EV_NONE#}

-- | Current version
ev_current :: Elf64_Word
ev_current = {#const EV_CURRENT#}


-- OS ABI identification

-- | UNIX System V ABI
elfosabi_none :: UChar
elfosabi_none = {#const ELFOSABI_NONE#}

-- | Alias for 'elfosabi_none'
elfosabi_sysv :: UChar
elfosabi_sysv = {#const ELFOSABI_SYSV#}
