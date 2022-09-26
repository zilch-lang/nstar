{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-duplicate-exports #-}

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

#ifndef _WIN32
#  include <elf.h>
#else 
#  include "elf.h"
#endif

import Data.Elf.Types
import Foreign.Storable (Storable(..))
import Data.Elf.Internal.BusSize (Size(..))
import Data.Elf.Internal.Serialize (Serializable(..))
import Foreign.Marshal.Array (peekArray, newArray)
import Foreign.Ptr (castPtr)

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

instance Storable (Elf_Ehdr 'S64) where
  sizeOf _ = {#sizeof Elf64_Ehdr#}
  alignment _ = {#alignof Elf64_Ehdr#}
  peek ptr =
    Elf_Ehdr <$> (peekArray 16 . castPtr =<< {#get struct Elf64_Ehdr->e_ident#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Ehdr->e_type#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Ehdr->e_machine#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Ehdr->e_version#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Ehdr->e_entry#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Ehdr->e_phoff#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Ehdr->e_shoff#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Ehdr->e_flags#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Ehdr->e_ehsize#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Ehdr->e_phentsize#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Ehdr->e_phnum#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Ehdr->e_shentsize#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Ehdr->e_shnum#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Ehdr->e_shstrndx#} ptr)
  poke ptr Elf_Ehdr{..} = do
    {#set struct Elf64_Ehdr->e_ident#} ptr =<< newArray (fromIntegral <$> e_ident)
    {#set struct Elf64_Ehdr->e_type#} ptr (fromIntegral e_type)
    {#set struct Elf64_Ehdr->e_machine#} ptr (fromIntegral e_machine)
    {#set struct Elf64_Ehdr->e_version#} ptr (fromIntegral e_version)
    {#set struct Elf64_Ehdr->e_entry#} ptr (fromIntegral e_entry)
    {#set struct Elf64_Ehdr->e_phoff#} ptr (fromIntegral e_phoff)
    {#set struct Elf64_Ehdr->e_shoff#} ptr (fromIntegral e_shoff)
    {#set struct Elf64_Ehdr->e_flags#} ptr (fromIntegral e_flags)
    {#set struct Elf64_Ehdr->e_ehsize#} ptr (fromIntegral e_ehsize)
    {#set struct Elf64_Ehdr->e_phentsize#} ptr (fromIntegral e_phentsize)
    {#set struct Elf64_Ehdr->e_phnum#} ptr (fromIntegral e_phnum)
    {#set struct Elf64_Ehdr->e_shentsize#} ptr (fromIntegral e_shentsize)
    {#set struct Elf64_Ehdr->e_shnum#} ptr (fromIntegral e_shnum)
    {#set struct Elf64_Ehdr->e_shstrndx#} ptr (fromIntegral e_shstrndx)

instance Serializable 'S64 e (Elf_Ehdr 'S64) where
  put e Elf_Ehdr{..} = do
    put @'S64 e e_ident
    put @'S64 e e_type
    put @'S64 e e_machine
    put @'S64 e e_version
    put @'S64 e e_entry
    put @'S64 e e_phoff
    put @'S64 e e_shoff
    put @'S64 e e_flags
    put @'S64 e e_ehsize
    put @'S64 e e_phentsize
    put @'S64 e e_phnum
    put @'S64 e e_shentsize
    put @'S64 e e_shnum
    put @'S64 e e_shstrndx

-- Versions

-- | Invalid ELF version
ev_none :: Elf_Word n
ev_none = {#const EV_NONE#}

-- | Current version
ev_current :: Elf_Word n
ev_current = {#const EV_CURRENT#}


-- OS ABI identification

-- | UNIX System V ABI
elfosabi_none :: Elf_UChar n
elfosabi_none = {#const ELFOSABI_NONE#}
-- | Alias for 'elfosabi_none'
elfosabi_sysv :: Elf_UChar n
elfosabi_sysv = {#const ELFOSABI_SYSV#}


-- ELF class

-- | Invalid class
elfclassnone :: Elf_UChar n
elfclassnone = {#const ELFCLASSNONE#}
-- | 32-bit object
elfclass32 :: Elf_UChar n
elfclass32 = {#const ELFCLASS32#}
-- | 64-bit object
elfclass64 :: Elf_UChar n
elfclass64 = {#const ELFCLASS64#}


-- Data encoding

-- | Invalid data encoding
elfdatanone :: Elf_UChar n
elfdatanone = {#const ELFDATANONE#}
-- | 2's completement, little endian
elfdata2lsb :: Elf_UChar n
elfdata2lsb = {#const ELFDATA2LSB#}
-- | 2's complement, big endian
elfdata2msb :: Elf_UChar n
elfdata2msb = {#const ELFDATA2MSB#}


-- File type

-- | No file type
et_none :: Elf_Half n
et_none = {#const ET_NONE#}
-- | Relocatable file
et_rel :: Elf_Half n
et_rel = {#const ET_REL#}
-- | Executable file
et_exec :: Elf_Half n
et_exec = {#const ET_EXEC#}
-- | Shared object file
et_dyn :: Elf_Half n
et_dyn = {#const ET_DYN#}
-- | Core file
et_core :: Elf_Half n
et_core = {#const ET_CORE#}


-- Architecture

-- | No machine
em_none :: Elf_Half n
em_none = {#const EM_NONE#}
-- | SUN SPARC
em_sparc :: Elf_Half n
em_sparc = {#const EM_SPARC#}
-- | ARM
em_arm :: Elf_Half n
em_arm = {#const EM_ARM#}
-- | AMD x86-64 architecture
em_x86_64 :: Elf_Half n
em_x86_64 = {#const EM_X86_64#}
