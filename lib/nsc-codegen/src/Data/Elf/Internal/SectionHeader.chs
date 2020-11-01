{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Elf.Internal.SectionHeader
( Elf64_Shdr(..)
  -- * @'sh_type'@
, sht_null, sht_progbits, sht_nobits
) where

import Data.Elf.Types
import Data.Elf.Internal.ToBytes (ToBytes(..))
import Foreign.Storable (Storable(..))

#include <elf.h>

-- | Section header
data Elf64_Shdr
  = Elf64_Shdr
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

instance Storable Elf64_Shdr where
  sizeOf _ = {#sizeof Elf64_Shdr#}
  alignment _ = {#alignof Elf64_Shdr#}
  peek _ = undefined
  poke _ _ = undefined

instance ToBytes Elf64_Shdr where
  toBytes le Elf64_Shdr{..} = mconcat
    [ toBytes le sh_name
    , toBytes le sh_type
    , toBytes le sh_flags
    , toBytes le sh_addr
    , toBytes le sh_offset
    , toBytes le sh_size
    , toBytes le sh_link
    , toBytes le sh_info
    , toBytes le sh_addralign
    , toBytes le sh_entsize
    ]

-- | Section types

-- | Section header table entry unused
sht_null :: Elf64_Word
sht_null = {#const SHT_NULL#}
-- | Program data
sht_progbits :: Elf64_Word
sht_progbits = {#const SHT_PROGBITS#}
-- | Program space with no data (bss)
sht_nobits :: Elf64_Word
sht_nobits = {#const SHT_NOBITS#}
