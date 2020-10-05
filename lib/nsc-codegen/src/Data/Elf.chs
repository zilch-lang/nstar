{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

{-|

  For a complete documentation, please see the one in the <https://code.woboq.org/userspace/glibc/elf/elf.h.html elf.h> header on linux.
-}
module Data.Elf
( -- * Types
  module Data.Elf.Types
  -- * File header
, Elf64_Ehdr(..)
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

  -- * Section header
, Elf64_Shdr(..)
  -- ** @'sh_type'@
, SectionType(..)

  -- ** @'sh_flags'@
, module Data.Elf.Section.Flags
) where

#include <elf.h>

-- For now, let's just focus on the 64 bits format, no matter what the HOST/TARGET arch is.

import Data.Word (Word8)
import Data.Bits (Bits)

import Data.Elf.Types
import Data.Elf.FileHeader.Flags
import Data.Elf.Section.Flags



{- $e_ident

   The 'e_ident' field contains the ELF magic number and some other info laid out as:

   - Byte number 0: @0x7f@
   - Bytes number 1-3: @\'E\', \'L\', \'F\'@
   - Byte number 4: ELF class (32-bit or 64-bit object)
   - Byte number 5: Data encoding (little endian or big endian)
   - Byte number 6: File version (must always be @'ev_current'@)
   - Byte number 7: OS ABI identification (one of 'elfosabi_none', 'elfosabi_sysv')
   - Byte number 8: ABI version
   - Bytes number 9-15: @0@-ed padding bytes
-}

-------------------------------------------------------------

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

-- | Section header
data Elf64_Shdr
  = Elf64_Shdr
  { sh_name      :: String       -- ^ Section name
  , sh_type      :: SectionType  -- ^ Section type
  , sh_flags     :: SFlags       -- ^ Section flags
  , sh_addr      :: Elf64_Addr   -- ^ Section virtual address at execution
  , sh_offset    :: Elf64_Off    -- ^ Section file offset
  , sh_size      :: Elf64_Xword  -- ^ Section size in bytes
  , sh_link      :: Elf64_Word   -- ^ Link to another section
  , sh_info      :: Elf64_Word   -- ^ Additional section information
  , sh_addralign :: Elf64_Xword  -- ^ Section alignment
  , sh_entsize   :: Elf64_Xword  -- ^ Entry size if section holds table
  }

-- | Legal values for @'sh_type'@.
data SectionType
  = SHT_Null           -- ^ Section header table entry unused
  | SHT_Progbits       -- ^ Program data
  | SHT_Symtab         -- ^ Symbol table
  | SHT_Strtab         -- ^ String table
  | SHT_Rela           -- ^ Relocation entries with addends
  | SHT_Hash           -- ^ Symbol hash table
  | SHT_Dynamic        -- ^ Dynamic linking information
  | SHT_Note           -- ^ Notes
  | SHT_Nobits         -- ^ Program space with no data (bss)
  | SHT_Rel            -- ^ Relocation entries, no addends
  | SHT_Shlib          -- ^ Reserved
  | SHT_Dynsym         -- ^ Dynamic linker symbol table
  | SHT_Init_array     -- ^ Array of constructors
  | SHT_Fini_array     -- ^ Array of destructors
  | SHT_Preinit_array  -- ^ Array of pre-constructors
  | SHT_Group          -- ^ Section group
  | SHT_Symtab_shndx   -- ^ Extended section indeces

  | SHT_GNU_attributes -- ^ Object attributes
  | SHT_GNU_hash       -- ^ GNU-style hash table
  | SHT_GNU_liblist    -- ^ Prelink library list
  | SHT_Checksum       -- ^ Checksum for DSO content

  | SHT_SUNW_move
  | SHT_SUNW_comdat
  | SHT_SUNW_syminfo

  | SHT_GNU_verdef     -- ^ Version definition section
  | SHT_GNU_verneed    -- ^ Version needs section
  | SHT_GNU_versym     -- ^ Version symbol table

  | SHT_Proc !Word8    -- ^ Processor-specific
  | SHT_User !Word8    -- ^ Application-specific


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
