{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Elf.Internal.FileHeader
( Elf_Ehdr(..)
  -- * @'e_ident'@

  -- | The 'e_ident' field contains the ELF magic number and some other info, laid out as:
  --
  --   [Byte number 0] @0x7F@
  --   [Byte number 1-3] @\'E\', \'L\', \'F\'@
  --   [Byte number 4] ELF class (32-bit or 64-bit object)
, elfclassnone, elfclass32, elfclass64
  -- |
  --   [Byte number 5] Data encoding (little endian or big endian)
, elfdatanone, elfdata2lsb, elfdata2msb
  -- |
  --   [Byte number 6] File version (must always be @'ev_current'@)
, ev_current
  -- |
  --   [Byte number 7] OS ABI identification
, elfosabi_none, elfosabi_sysv
  -- |
  --   [Byte number 8] ABI version
  --   [Byte number 9-15] @0@-ed padding bytes

  -- * @'e_type'@
, et_none, et_rel, et_exec, et_dyn, et_core

  -- * @'e_machine'@
, em_none, em_sparc, em_x86_64, em_arm

  -- * @'e_version'@
, ev_none, ev_current

  -- * @'e_flags'@
) where

#include <elf.h>

import Data.Elf.Types
import Foreign.Storable (Storable(..))
import GHC.TypeNats (Nat)
import Data.Elf.Internal.BusSize (Size(..))
import Data.Elf.Internal.Serialize (Serializable(..), SerializableValueSet)

-- | The ELF file header. This appears at the start of every ELF file.
data Elf_Ehdr n = Elf_Ehdr
  { e_ident     :: ![Elf_UChar n]   -- ^ Magic number and other info
  , e_type      :: !(Elf_Half n)    -- ^ Object file type
  , e_machine   :: !(Elf_Half n)    -- ^ Architecture
  , e_version   :: !(Elf_Word n)    -- ^ Object file version
  , e_entry     :: !(Elf_Addr n)    -- ^ Entry point virtual address
  , e_phoff     :: !(Elf_Off n)     -- ^ Program header table file offset
  , e_shoff     :: !(Elf_Off n)     -- ^ Section header table file offset
  , e_flags     :: !(Elf_Word n)    -- ^ Processor-specific flags
  , e_ehsize    :: !(Elf_Half n)    -- ^ ELF header size in bytes
  , e_phentsize :: !(Elf_Half n)    -- ^ Program header table entry size
  , e_phnum     :: !(Elf_Half n)    -- ^ Program header table entry count
  , e_shentsize :: !(Elf_Half n)    -- ^ Section header table entry size
  , e_shnum     :: !(Elf_Half n)    -- ^ Section header table entry count
  , e_shstrndx  :: !(Elf_Half n)    -- ^ Section header string table index
  }

instance Storable (Elf_Ehdr S64) where
  sizeOf _ = {#sizeof Elf64_Ehdr#}
  alignment _ = {#alignof Elf64_Ehdr#}
  peek _ = undefined      -- ↓
  poke _ _ = undefined    -- we don't need to either write or read it to/from a pointer

instance (SerializableValueSet S64 e) => Serializable S64 e (Elf_Ehdr S64) where
  put e Elf_Ehdr{..} = do
    put @S64 e e_ident
    put @S64 e e_type
    put @S64 e e_machine
    put @S64 e e_version
    put @S64 e e_entry
    put @S64 e e_phoff
    put @S64 e e_shoff
    put @S64 e e_flags
    put @S64 e e_ehsize
    put @S64 e e_phentsize
    put @S64 e e_phnum
    put @S64 e e_shentsize
    put @S64 e e_shnum
    put @S64 e e_shstrndx

-- Versions

-- | Invalid ELF version
ev_none :: ValueSet n => Elf_Word n
ev_none = {#const EV_NONE#}

-- | Current version
ev_current :: ValueSet n => Elf_Word n
ev_current = {#const EV_CURRENT#}


-- OS ABI identification

-- | UNIX System V ABI
elfosabi_none :: ValueSet n => Elf_UChar n
elfosabi_none = {#const ELFOSABI_NONE#}
-- | Alias for 'elfosabi_none'
elfosabi_sysv :: ValueSet n => Elf_UChar n
elfosabi_sysv = {#const ELFOSABI_SYSV#}


-- ELF class

-- | Invalid class
elfclassnone :: ValueSet n => Elf_UChar n
elfclassnone = {#const ELFCLASSNONE#}
-- | 32-bit object
elfclass32 :: ValueSet n => Elf_UChar n
elfclass32 = {#const ELFCLASS32#}
-- | 64-bit object
elfclass64 :: ValueSet n => Elf_UChar n
elfclass64 = {#const ELFCLASS64#}


-- Data encoding

-- | Invalid data encoding
elfdatanone :: ValueSet n => Elf_UChar n
elfdatanone = {#const ELFDATANONE#}
-- | 2's completement, little endian
elfdata2lsb :: ValueSet n => Elf_UChar n
elfdata2lsb = {#const ELFDATA2LSB#}
-- | 2's complement, big endian
elfdata2msb :: ValueSet n => Elf_UChar n
elfdata2msb = {#const ELFDATA2MSB#}


-- File type

-- | No file type
et_none :: ValueSet n => Elf_Half n
et_none = {#const ET_NONE#}
-- | Relocatable file
et_rel :: ValueSet n => Elf_Half n
et_rel = {#const ET_REL#}
-- | Executable file
et_exec :: ValueSet n => Elf_Half n
et_exec = {#const ET_EXEC#}
-- | Shared object file
et_dyn :: ValueSet n => Elf_Half n
et_dyn = {#const ET_DYN#}
-- | Core file
et_core :: ValueSet n => Elf_Half n
et_core = {#const ET_CORE#}


-- Architecture

-- | No machine
em_none :: ValueSet n => Elf_Half n
em_none = {#const EM_NONE#}
-- | SUN SPARC
em_sparc :: ValueSet n => Elf_Half n
em_sparc = {#const EM_SPARC#}
-- | ARM
em_arm :: ValueSet n => Elf_Half n
em_arm = {#const EM_ARM#}
-- | AMD x86-64 architecture
em_x86_64 :: ValueSet n => Elf_Half n
em_x86_64 = {#const EM_X86_64#}
