{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Elf.Internal.ProgramHeader where

import Data.Elf.Types
import Data.Elf.Internal.ToBytes (ToBytes(..))

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

instance ToBytes Elf64_Phdr where
  toBytes le Elf64_Phdr{..} = mconcat
    [ toBytes le p_type
    , toBytes le p_flags
    , toBytes le p_offset
    , toBytes le p_vaddr
    , toBytes le p_paddr
    , toBytes le p_filesz
    , toBytes le p_memsz
    , toBytes le p_align
    ]
