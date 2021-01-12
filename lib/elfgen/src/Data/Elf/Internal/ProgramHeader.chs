{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Elf.Internal.ProgramHeader
( Elf_Phdr(..)
  -- * @'p_type'@
, pt_null, pt_phdr, pt_load, pt_interp
) where

import Data.Elf.Types
import Foreign.Storable (Storable(..))
import GHC.TypeNats (Nat)
import Data.Elf.Internal.BusSize (Size(..))
import Data.Elf.Internal.Serialize (Serializable(..))

#include <elf.h>

-- | Program segment header parameterized by the CPU bus size @n@.
data Elf_Phdr n = Elf_Phdr
  { p_type    :: !(Elf_Word n)    -- ^ Segment type
  , p_flags   :: !(Elf_Word n)    -- ^ Segment flags
  , p_offset  :: !(Elf_Off n)     -- ^ Segment file offset
  , p_vaddr   :: !(Elf_Addr n)    -- ^ Segment virtual address
  , p_paddr   :: !(Elf_Addr n)    -- ^ Segment physical address
  , p_filesz  :: !(Elf_Xword n)   -- ^ Segment size in file
  , p_memsz   :: !(Elf_Xword n)   -- ^ Segment size in memory
  , p_align   :: !(Elf_Xword n)   -- ^ Segment alignment
  }

instance Storable (Elf_Phdr S64) where
  sizeOf _ = {#sizeof Elf64_Phdr#}
  alignment _ = {#alignof Elf64_Phdr#}
  peek _ = undefined
  poke _ _ = undefined

instance Serializable S64 e (Elf_Phdr S64) where
  put e Elf_Phdr{..} = do
    put @S64 e p_type
    put @S64 e p_flags
    put @S64 e p_offset
    put @S64 e p_vaddr
    put @S64 e p_paddr
    put @S64 e p_filesz
    put @S64 e p_memsz
    put @S64 e p_align

-- Segment types

-- | Program header table entry unused
pt_null :: Elf_Word n
pt_null = {#const PT_NULL#}
-- | Entry for header table itself
pt_phdr :: Elf_Word n
pt_phdr = {#const PT_PHDR#}
-- | Loadable program segment
pt_load :: Elf_Word n
pt_load = {#const PT_LOAD#}
-- | Program interpreter
pt_interp :: Elf_Word n
pt_interp = {#const PT_INTERP#}
