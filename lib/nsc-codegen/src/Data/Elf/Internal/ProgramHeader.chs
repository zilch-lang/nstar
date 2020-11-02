{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Elf.Internal.ProgramHeader
( Elf64_Phdr(..)
  -- * @'p_type'@
, pt_null, pt_phdr, pt_load, pt_interp
) where

import Data.Elf.Types
import Data.Elf.Internal.ToBytes (ToBytes(..))
import Foreign.Storable (Storable(..))

#include <elf.h>

-- | Program segment header
data Elf64_Phdr
  = Elf64_Phdr
  { p_type    :: !Elf64_Word    -- ^ Segment type
  , p_flags   :: !Elf64_Word    -- ^ Segment flags
  , p_offset  :: !Elf64_Off     -- ^ Segment file offset
  , p_vaddr   :: !Elf64_Addr    -- ^ Segment virtual address
  , p_paddr   :: !Elf64_Addr    -- ^ Segment physical address
  , p_filesz  :: !Elf64_Xword   -- ^ Segment size in file
  , p_memsz   :: !Elf64_Xword   -- ^ Segment size in memory
  , p_align   :: !Elf64_Xword   -- ^ Segment alignment
  }

instance Storable Elf64_Phdr where
  sizeOf _ = {#sizeof Elf64_Phdr#}
  alignment _ = {#alignof Elf64_Phdr#}
  peek _ = undefined
  poke _ _ = undefined

instance ToBytes Elf64_Phdr where
  toBytes le Elf64_Phdr{..} = mconcat
    [ toBytes True p_type
    , toBytes True p_flags
    , toBytes True p_offset
    , toBytes le p_vaddr
    , toBytes le p_paddr
    , toBytes le p_filesz
    , toBytes le p_memsz
    , toBytes True p_align
    ]

-- Segment types

-- | Program header table entry unused
pt_null :: Elf64_Word
pt_null = {#const PT_NULL#}
-- | Entry for header table itself
pt_phdr :: Elf64_Word
pt_phdr = {#const PT_PHDR#}
-- | Loadable program segment
pt_load :: Elf64_Word
pt_load = {#const PT_LOAD#}
-- | Program interpreter
pt_interp :: Elf64_Word
pt_interp = {#const PT_INTERP#}
