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
data family Elf_Phdr (n :: Size)
-- | A 64-bits ELF program header.
data instance Elf_Phdr S64 = Elf64_Phdr
  { p_type    :: !Elf64_Word    -- ^ Segment type
  , p_flags   :: !Elf64_Word    -- ^ Segment flags
  , p_offset  :: !Elf64_Off     -- ^ Segment file offset
  , p_vaddr   :: !Elf64_Addr    -- ^ Segment virtual address
  , p_paddr   :: !Elf64_Addr    -- ^ Segment physical address
  , p_filesz  :: !Elf64_Xword   -- ^ Segment size in file
  , p_memsz   :: !Elf64_Xword   -- ^ Segment size in memory
  , p_align   :: !Elf64_Xword   -- ^ Segment alignment
  }

instance Storable (Elf_Phdr S64) where
  sizeOf _ = {#sizeof Elf64_Phdr#}
  alignment _ = {#alignof Elf64_Phdr#}
  peek _ = undefined
  poke _ _ = undefined

instance ( n ~ S64
         , Serializable n e Elf64_Word
         , Serializable n e Elf64_Off
         , Serializable n e Elf64_Addr
         , Serializable n e Elf64_Xword
         ) => Serializable n e (Elf_Phdr n) where
  put Elf64_Phdr{..} = do
    put @n @e p_type
    put @n @e p_flags
    put @n @e p_offset
    put @n @e p_vaddr
    put @n @e p_paddr
    put @n @e p_filesz
    put @n @e p_memsz
    put @n @e p_align

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
