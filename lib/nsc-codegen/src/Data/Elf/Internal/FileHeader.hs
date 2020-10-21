{-# LANGUAGE BangPatterns #-}

module Data.Elf.Internal.FileHeader where

import Data.Elf.Types

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
