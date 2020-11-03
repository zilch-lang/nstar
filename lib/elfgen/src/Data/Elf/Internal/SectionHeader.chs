{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Elf.Internal.SectionHeader
( Elf_Shdr(..)
  -- * @'sh_type'@
, sht_null, sht_progbits, sht_nobits, sht_strtab, sht_symtab
) where

import Data.Elf.Types
import Foreign.Storable (Storable(..))
import GHC.TypeNats (Nat)
import Data.Elf.Internal.Serialize (Serializable(..), SerializableValueSet)
import Data.Elf.Internal.BusSize (Size(..))

#include <elf.h>

-- | Section header parameterized by the architecture bus size @n@.
data Elf_Shdr n = Elf_Shdr
  { sh_name        :: !(Elf_Word n)    -- ^ Section name (string table index)
  , sh_type        :: !(Elf_Word n)    -- ^ Section type
  , sh_flags       :: !(Elf_Xword n)   -- ^ Section flags
  , sh_addr        :: !(Elf_Addr n)    -- ^ Section virtual address at execution
  , sh_offset      :: !(Elf_Off n)     -- ^ Section file offset
  , sh_size        :: !(Elf_Xword n)   -- ^ Section size in bytes
  , sh_link        :: !(Elf_Word n)    -- ^ Link to another section
  , sh_info        :: !(Elf_Word n)    -- ^ Additional section information
  , sh_addralign   :: !(Elf_Xword n)   -- ^ Section alignment
  , sh_entsize     :: !(Elf_Xword n)   -- ^ Entry size if section holds table
  }

instance Storable (Elf_Shdr S64) where
  sizeOf _ = {#sizeof Elf64_Shdr#}
  alignment _ = {#alignof Elf64_Shdr#}
  peek _ = undefined
  poke _ _ = undefined

instance (SerializableValueSet S64 e) => Serializable S64 e (Elf_Shdr S64) where
  put e Elf_Shdr{..} = do
    put @S64 e sh_name
    put @S64 e sh_type
    put @S64 e sh_flags
    put @S64 e sh_addr
    put @S64 e sh_offset
    put @S64 e sh_size
    put @S64 e sh_link
    put @S64 e sh_info
    put @S64 e sh_addralign
    put @S64 e sh_entsize

-- Section types

-- | Section header table entry unused
sht_null :: ValueSet n => Elf_Word n
sht_null = {#const SHT_NULL#}
-- | Program data
sht_progbits :: ValueSet n => Elf_Word n
sht_progbits = {#const SHT_PROGBITS#}
-- | Program space with no data (bss)
sht_nobits :: ValueSet n => Elf_Word n
sht_nobits = {#const SHT_NOBITS#}
-- | Symbol table
sht_symtab :: ValueSet n => Elf_Word n
sht_symtab = {#const SHT_SYMTAB#}
-- | String table
sht_strtab :: ValueSet n => Elf_Word n
sht_strtab = {#const SHT_STRTAB#}
