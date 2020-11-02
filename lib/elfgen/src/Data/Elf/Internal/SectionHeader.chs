{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Elf.Internal.SectionHeader
( Elf_Shdr(..)
  -- * @'sh_type'@
, sht_null, sht_progbits, sht_nobits, sht_strtab, sht_symtab
) where

import Data.Elf.Types
import Foreign.Storable (Storable(..))
import Data.Binary (Put, put)
import GHC.TypeNats (Nat)

#include <elf.h>

-- | Section header parameterized by the architecture bus size @n@.
data family Elf_Shdr (n :: Nat)
-- | A section header for a 64-bits ELF file.
data instance Elf_Shdr 64 = Elf64_Shdr
  { sh_name        :: !Elf64_Word    -- ^ Section name (string table index)
  , sh_type        :: !Elf64_Word    -- ^ Section type
  , sh_flags       :: !Elf64_Xword   -- ^ Section flags
  , sh_addr        :: !Elf64_Addr    -- ^ Section virtual address at execution
  , sh_offset      :: !Elf64_Off     -- ^ Section file offset
  , sh_size        :: !Elf64_Xword   -- ^ Section size in bytes
  , sh_link        :: !Elf64_Word    -- ^ Link to another section
  , sh_info        :: !Elf64_Word    -- ^ Additional section information
  , sh_addralign   :: !Elf64_Xword   -- ^ Section alignment
  , sh_entsize     :: !Elf64_Xword   -- ^ Entry size if section holds table
  }

instance Storable (Elf_Shdr 64) where
  sizeOf _ = {#sizeof Elf64_Shdr#}
  alignment _ = {#alignof Elf64_Shdr#}
  peek _ = undefined
  poke _ _ = undefined


-- Section types

-- | Section header table entry unused
sht_null :: Elf64_Word
sht_null = {#const SHT_NULL#}
-- | Program data
sht_progbits :: Elf64_Word
sht_progbits = {#const SHT_PROGBITS#}
-- | Program space with no data (bss)
sht_nobits :: Elf64_Word
sht_nobits = {#const SHT_NOBITS#}
-- | Symbol table
sht_symtab :: Elf64_Word
sht_symtab = {#const SHT_SYMTAB#}
-- | String table
sht_strtab :: Elf64_Word
sht_strtab = {#const SHT_STRTAB#}
