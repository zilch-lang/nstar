{-# LANGUAGE RecordWildCards #-}

module Data.Elf.Internal.Symbol
( Elf_Sym(..)
  -- * 'st_info'
  -- ** Symbol binding
, stb_local, stb_global, stb_weak
  -- ** Symbol type
, stt_notype, stt_object, stt_func, stt_section
  -- * 'st_other'
, stv_default, stv_internal, stv_hidden, stv_protected

, Elf_Rela(..)
  -- * 'r_info'
  -- ** Relocation type
  -- *** AMD x86-64 architecture specific
, r_x86_64_none, r_x86_64_64, r_x86_64_32, r_x86_64_32s

) where

import Data.Elf.Internal.BusSize (Size(..))
import Data.Elf.Types
import Data.Elf.Internal.Serialize (Serializable(..))
import Foreign.Storable (Storable(..))

#include <elf.h>

-- | Symbol table entry.
data Elf_Sym (n :: Size)
  = Elf_Sym
  { st_name   :: Elf_Word n      -- ^ Symbol name (string table index)
  , st_info   :: Elf_UChar n     -- ^ Symbol type and binding
  , st_other  :: Elf_UChar n     -- ^ Symbol visibility
  , st_shndx  :: Elf_Section n   -- ^ Section index
  , st_value  :: Elf_Addr n      -- ^ Symbol value
  , st_size   :: Elf_Xword n     -- ^ Symbol size
  }

instance Storable (Elf_Sym S64) where
  sizeOf _ = {#sizeof Elf64_Sym#}
  alignment _ = {#alignof Elf64_Sym#}
  peek ptr =
    Elf_Sym <$> (fromIntegral <$> {#get struct Elf64_Sym->st_name#} ptr)
            <*> (fromIntegral <$> {#get struct Elf64_Sym->st_info#} ptr)
            <*> (fromIntegral <$> {#get struct Elf64_Sym->st_other#} ptr)
            <*> (fromIntegral <$> {#get struct Elf64_Sym->st_shndx#} ptr)
            <*> (fromIntegral <$> {#get struct Elf64_Sym->st_value#} ptr)
            <*> (fromIntegral <$> {#get struct Elf64_Sym->st_size#} ptr)
  poke ptr Elf_Sym{..} = do
    {#set struct Elf64_Sym->st_name#} ptr (fromIntegral st_name)
    {#set struct Elf64_Sym->st_info#} ptr (fromIntegral st_info)
    {#set struct Elf64_Sym->st_other#} ptr (fromIntegral st_other)
    {#set struct Elf64_Sym->st_shndx#} ptr (fromIntegral st_shndx)
    {#set struct Elf64_Sym->st_value#} ptr (fromIntegral st_value)
    {#set struct Elf64_Sym->st_size#} ptr (fromIntegral st_size)

instance Serializable S64 e (Elf_Sym S64) where
  put e Elf_Sym{..} = do
    put @S64 e st_name
    put @S64 e st_info
    put @S64 e st_other
    put @S64 e st_shndx
    put @S64 e st_value
    put @S64 e st_size

-- Symbol binding type

-- | Local symbol
stb_local :: Elf_UChar n
stb_local = {#const STB_LOCAL#}
-- | Global symbol
stb_global :: Elf_UChar n
stb_global = {#const STB_GLOBAL#}
-- | Weak symbol
stb_weak :: Elf_UChar n
stb_weak = {#const STB_WEAK#}


-- Symbol type

-- | Symbol type is unspecified
stt_notype :: Elf_UChar n
stt_notype = {#const STT_NOTYPE#}
-- | Symbol is a data object
stt_object :: Elf_UChar n
stt_object = {#const STT_OBJECT#}
-- | Symbol is a code object
stt_func :: Elf_UChar n
stt_func = {#const STT_FUNC#}
-- | Symbol associated with a section
stt_section :: Elf_UChar n
stt_section = {#const STT_SECTION#}


-- Visibility

-- | Default symbol visibility rules
stv_default :: Elf_UChar n
stv_default = {#const STV_DEFAULT#}
-- | Processor specific hidden class
stv_internal :: Elf_UChar n
stv_internal = {#const STV_INTERNAL#}
-- | Sym unavailable in other modules
stv_hidden :: Elf_UChar n
stv_hidden = {#const STV_HIDDEN#}
-- | Not preemptible, not exported
stv_protected :: Elf_UChar n
stv_protected = {#const STV_PROTECTED#}


-- | Relocation table entry with addend (in section of type @SHT_RELA@).
data Elf_Rela (n :: Size)
  = Elf_Rela
  { r_offset :: Elf_Addr n          -- ^ Address
  , r_info   :: Elf_Rel_Info n      -- ^ Relocation type and symbol index
  , r_addend :: Elf_Rel_Addend n    -- ^ Addend
  }

instance Storable (Elf_Rela S64) where
  sizeOf _ = {#sizeof Elf64_Rela#}
  alignment _ = {#alignof Elf64_Rela#}
  peek ptr =
    Elf_Rela <$> (fromIntegral <$> {#get struct Elf64_Rela->r_offset#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Rela->r_info#} ptr)
             <*> (fromIntegral <$> {#get struct Elf64_Rela->r_addend#} ptr)
  poke ptr Elf_Rela{..} = do
    {#set struct Elf64_Rela->r_offset#} ptr (fromIntegral r_offset)
    {#set struct Elf64_Rela->r_info#} ptr (fromIntegral r_info)
    {#set struct Elf64_Rela->r_addend#} ptr (fromIntegral r_addend)

instance Serializable S64 e (Elf_Rela S64) where
  put e Elf_Rela{..} = do
    put @S64 e r_offset
    put @S64 e r_info
    put @S64 e r_addend

-------- AMD x86-64 relocations
-- | No reloc
r_x86_64_none :: Elf_Word n
r_x86_64_none = {#const R_X86_64_NONE#}
-- | Direct 64 bit
r_x86_64_64 :: Elf_Word n
r_x86_64_64 = {#const R_X86_64_64#}
-- | Direct 32 bit zero extended
r_x86_64_32 :: Elf_Word n
r_x86_64_32 = {#const R_X86_64_32#}
-- | Direct 32 bit sign extended
r_x86_64_32s :: Elf_Word n
r_x86_64_32s = {#const R_X86_64_32S#}
