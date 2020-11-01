{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Elf.Internal.FileHeader
( Elf64_Ehdr(..)
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
import Data.Elf.Internal.ToBytes (ToBytes(..))
import Foreign.Storable (Storable(..))


-- | The ELF file header. This appears at the start of every ELF file.
data Elf64_Ehdr
  = Elf64_Ehdr
  { e_ident     :: ![UChar]      -- ^ Magic number and other info
  , e_type      :: !Elf64_Half   -- ^ Object file type
  , e_machine   :: !Elf64_Half   -- ^ Architecture
  , e_version   :: !Elf64_Word   -- ^ Object file version
  , e_entry     :: !Elf64_Addr   -- ^ Entry point virtual address
  , e_phoff     :: !Elf64_Off    -- ^ Program header table file offset
  , e_shoff     :: !Elf64_Off    -- ^ Section header table file offset
  , e_flags     :: !Elf64_Word   -- ^ Processor-specific flags
  , e_ehsize    :: !Elf64_Half   -- ^ ELF header size in bytes
  , e_phentsize :: !Elf64_Half   -- ^ Program header table entry size
  , e_phnum     :: !Elf64_Half   -- ^ Program header table entry count
  , e_shentsize :: !Elf64_Half   -- ^ Section header table entry size
  , e_shnum     :: !Elf64_Half   -- ^ Section header table entry count
  , e_shstrndx  :: !Elf64_Half   -- ^ Section header string table index
  }

instance Storable Elf64_Ehdr where
  sizeOf _ = 64
    --   16  bytes for @e_ident@
    -- +  2  bytes for @e_type@
    -- +  2  bytes for @e_machine@
    -- +  4  bytes for @e_version@
    -- +  8  bytes for @e_entry@
    -- +  8  bytes for @e_phoff@
    -- +  8  bytes for @e_shoff@
    -- +  4  bytes for @e_flags@
    -- +  2  bytes for @e_ehsize@
    -- +  2  bytes for @e_phentsize@
    -- +  2  bytes for @e_phnum@
    -- +  2  bytes for @e_shentsize@
    -- +  2  bytes for @e_shnum@
    -- +  2  bytes for @e_shstrndx@
    -- = 64  bytes total
  alignment _ = {#alignof Elf64_Ehdr#}
  peek _ = undefined      -- ↓
  poke _ _ = undefined    -- we don't need to either write or read it to/from a pointer

instance ToBytes Elf64_Ehdr where
  toBytes le Elf64_Ehdr{..} = mconcat
    [ toBytes le e_ident
    , toBytes le e_type
    , toBytes le e_machine
    , toBytes le e_version
    , toBytes le e_entry
    , toBytes le e_phoff
    , toBytes le e_shoff
    , toBytes le e_flags
    , toBytes le e_ehsize
    , toBytes le e_phentsize
    , toBytes le e_phnum
    , toBytes le e_shentsize
    , toBytes le e_shnum
    , toBytes le e_shstrndx
    ]

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


-- ELF class

-- | Invalid class
elfclassnone :: UChar
elfclassnone = {#const ELFCLASSNONE#}
-- | 32-bit object
elfclass32 :: UChar
elfclass32 = {#const ELFCLASS32#}
-- | 64-bit object
elfclass64 :: UChar
elfclass64 = {#const ELFCLASS64#}


-- Data encoding

-- | Invalid data encoding
elfdatanone :: UChar
elfdatanone = {#const ELFDATANONE#}
-- | 2's completement, little endian
elfdata2lsb :: UChar
elfdata2lsb = {#const ELFDATA2LSB#}
-- | 2's complement, big endian
elfdata2msb :: UChar
elfdata2msb = {#const ELFDATA2MSB#}


-- File type

-- | No file type
et_none :: Elf64_Half
et_none = {#const ET_NONE#}
-- | Relocatable file
et_rel :: Elf64_Half
et_rel = {#const ET_REL#}
-- | Executable file
et_exec :: Elf64_Half
et_exec = {#const ET_EXEC#}
-- | Shared object file
et_dyn :: Elf64_Half
et_dyn = {#const ET_DYN#}
-- | Core file
et_core :: Elf64_Half
et_core = {#const ET_CORE#}


-- Architecture

-- | No machine
em_none :: Elf64_Half
em_none = {#const EM_NONE#}
-- | SUN SPARC
em_sparc :: Elf64_Half
em_sparc = {#const EM_SPARC#}
-- | ARM
em_arm :: Elf64_Half
em_arm = {#const EM_ARM#}
-- | AMD x86-64 architecture
em_x86_64 :: Elf64_Half
em_x86_64 = {#const EM_X86_64#}
