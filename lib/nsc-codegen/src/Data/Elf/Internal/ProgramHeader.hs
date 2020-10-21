{-# LANGUAGE BangPatterns #-}

module Data.Elf.Internal.ProgramHeader where

import Data.Elf.Types

-- | Program segment header
data ELf64_Phdr
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
