{-# LANGUAGE BangPatterns #-}

module Data.Elf.ProgramHeader
( Elf64_Phdr(..)

  -- ** @'p_type'@
, ProgType(..)

  -- ** @'p_flags'@
, module Data.Elf.ProgramHeader.Flags
) where

#include <elf.h>

import Data.Elf.Types
import Data.Elf.ProgramHeader.Flags

-- | Program segment header
data Elf64_Phdr
  = Elf64_Phdr
  { p_type      :: !ProgType      -- ^ Segment type
  , p_flags     :: !PFlags        -- ^ Segment flags
  , p_offset    :: !Elf64_Off     -- ^ Segment file offset
  , p_content   :: ![UChar]       -- ^ Segment content (from which to determine its size, address, etc)
  -- , p_vaddr     :: !Elf64_Addr    -- ^ Segment virtual address
  -- , p_paddr     :: !Elf64_Addr    -- ^ Segment physical address
  -- , p_filesz    :: !Elf64_Xword   -- ^ Segment size in file
  -- , p_memsz     :: !Elf64_Xword   -- ^ Segment size in memory
  -- , p_align     :: !Elf64_Xword   -- ^ Segment alignment
  }

-- | Legal values for @'p_type'@ (segment type).
data ProgType
  = PT_Null          -- ^ Program header table entry unused
  | PT_Load          -- ^ Loadable program segment
  | PT_Dynamic       -- ^ Dynamic linking information
  | PT_Interp        -- ^ Program interpreter
  | PT_Note          -- ^ Auxiliary information
  | PT_Shlib         -- ^ Reserved
  | PT_Phdr          -- ^ Entry for header table itself

  | PT_GNU_eh_frame  -- ^ GCC .eh_frame_hdr segment
  | PT_GNU_stack     -- ^ Indicates stack executability
  | PT_GNU_relro     -- ^ Read-only after relocation

  | PT_SUNWbss       -- ^ Sun specific segment
  | PT_SUNWstack     -- ^ Stack segment
