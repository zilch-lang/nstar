module Data.Elf.SectionHeader
( Elf64_Shdr(..)
  -- ** @'sh_type'@
, SectionType(..)

  -- ** @'sh_flags'@
, module Data.Elf.SectionHeader.Flags
) where

import Data.Elf.Types
import Data.Word (Word8)
import Data.Elf.SectionHeader.Flags

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
