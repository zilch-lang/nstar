{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

{-|

  For a complete documentation, please see the one in the <elf.h> header on linux.
-}
module Data.Elf
( -- * Types
  Elf64_Half, Elf64_Word, Elf64_Sword, Elf64_Xword, Elf64_Sxword, Elf64_Addr, Elf64_Off, Elf64_Section, Elf64_Versym, UChar

  -- * File header
, Elf64_Ehdr(..)
  -- ** @'e_ident'@

  -- ** @'e_type'@
, ObjFileType(..)

  -- ** @'e_machine'@
, Arch(..)

  -- ** @'e_flags'@
, Flag
, Flags
  -- *** SUN SPARC specific flags
, ef_sparcv9_mm, ef_sparcv9_tso, ef_sparcv9_pso, ef_sparcv9_rmo, ef_sparc_ledata
, ef_sparc_ext_mask, ef_sparc_32plus, ef_sparc_sun_us1, ef_sparc_hal_r1, ef_sparc_sun_us3
  -- *** Alpha specific flags
, ef_alpha_32bit, ef_alpha_canrelax
  -- *** PowerPC specific flags
, ef_ppc_emb, ef_ppc_relocatable, ef_ppc_relocatable_lib, ef_ppc64_abi
) where

#include <elf.h>

-- For now, let's just focus on the 64 bits format, no matter what the HOST/TARGET arch is.

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int32, Int64)
import Data.Char (ord)
import Data.Bits (Bits)



-- | Unsigned 16-bits integer
type Elf64_Half = Word16

-- | Unsigned 32-bits integer
type Elf64_Word = Word32
-- | Signed 32-bits integer
type Elf64_Sword = Int32

-- | Unsigned 64-bits integer
type Elf64_Xword = Word64
-- | Signed 64-bits integer
type Elf64_Sxword = Int64

-- | Unsigned 64-bits integer
type Elf64_Addr = Word64

-- | Unsigned 64-bits integer
type Elf64_Off = Word64

-- | Unsigned 16-bits integer
type Elf64_Section = Word16

-- | Unsigned 16-bits integer
type Elf64_Versym = Elf64_Half

-- | Unsigned 8-bits integer
type UChar = Word8

-------------------------------------------------------------

-- | The ELF file header. This appears at the start of every ELF file.
data Elf64_Ehdr
  = Elf64_Ehdr
  { e_ident     :: ![UChar]      -- ^ Magic number and other info
  , e_type      :: ObjFileType   -- ^ Object file type
  , e_machine   :: Arch          -- ^ Architecture
  , e_version   :: !Elf64_Word   -- ^ Object file version
  , e_entry     :: !Elf64_Addr   -- ^ Entry point virtual address
  , e_phoff     :: !Elf64_Off    -- ^ Program header table file offset
  , e_shoff     :: !Elf64_Off    -- ^ Section header table file offset
  , e_flags     :: !Flags        -- ^ Processor-specific flags
  , e_ehsize    :: !Elf64_Half   -- ^ ELF header size in bytes
  , e_phentsize :: !Elf64_Half   -- ^ Program header table entry size
  , e_phnum     :: !Elf64_Half   -- ^ Program header table entry count
  , e_shentsize :: !Elf64_Half   -- ^ Section header table entry size
  , e_shnum     :: !Elf64_Half   -- ^ Section header table entry count
  , e_shstrndx  :: !Elf64_Half   -- ^ Section header string table index
  }

-- | Legal values for @'e_type'@.
data ObjFileType
  = ET_None        -- ^ No file type
  | ET_Rel         -- ^ Relocatable file
  | ET_Exec        -- ^ Executable file
  | ET_Dyn         -- ^ Shared object file
  | ET_Core        -- ^ Core file
  | ET_OS Word8    -- ^ OS-specific (between @<https://code.woboq.org/userspace/glibc/elf/elf.h.html#168 ET_LOOS>@
                   --                and @<https://code.woboq.org/userspace/glibc/elf/elf.h.html#169 ET_HIOS>@)
  | ET_Proc Word8  -- ^ Processor-specific (between @<https://code.woboq.org/userspace/glibc/elf/elf.h.html#170 ET_LOPROC>@
                   --   and @<https://code.woboq.org/userspace/glibc/elf/elf.h.html#171 ET_HIPROC>@)

-- | Legal values for @'e_machine'@.
data Arch
  = EM_None             -- ^ No machine

type Flags = Flag
-- | Processor specific flags
newtype Flag = Flag Elf64_Word
  deriving (Show, Eq, Ord, Num, Bits)

-- SPARC specific flags

ef_sparcv9_mm :: Flag
ef_sparcv9_mm = Flag {#const EF_SPARCV9_MM#}

ef_sparcv9_tso :: Flag
ef_sparcv9_tso = Flag {#const EF_SPARCV9_TSO#}

ef_sparcv9_pso :: Flag
ef_sparcv9_pso = Flag {#const EF_SPARCV9_PSO#}

ef_sparcv9_rmo :: Flag
ef_sparcv9_rmo = Flag {#const EF_SPARCV9_RMO#}
-- | little endian data
ef_sparc_ledata :: Flag
ef_sparc_ledata = Flag {#const EF_SPARC_LEDATA#}

ef_sparc_ext_mask :: Flag
ef_sparc_ext_mask = Flag {#const EF_SPARC_EXT_MASK#}
-- | generic v8+ features
ef_sparc_32plus :: Flag
ef_sparc_32plus = Flag {#const EF_SPARC_32PLUS#}
-- | Sun UltraSPARC1 extensions
ef_sparc_sun_us1 :: Flag
ef_sparc_sun_us1 = Flag {#const EF_SPARC_SUN_US1#}
-- | HAL R1 extensions
ef_sparc_hal_r1 :: Flag
ef_sparc_hal_r1 = Flag {#const EF_SPARC_HAL_R1#}
-- | Sun UltraSPARCIII extensions
ef_sparc_sun_us3 :: Flag
ef_sparc_sun_us3 = Flag {#const EF_SPARC_SUN_US3#}

-- Alpha specific flags

-- | All addresses must be < 2GB
ef_alpha_32bit :: Flag
ef_alpha_32bit = Flag {#const EF_ALPHA_32BIT#}
-- | Relocations for relaxing exist
ef_alpha_canrelax :: Flag
ef_alpha_canrelax = Flag {#const EF_ALPHA_CANRELAX#}

-- PowerPC specific flags

-- | PowerPC embedded flag
ef_ppc_emb :: Flag
ef_ppc_emb = Flag {#const EF_PPC_EMB#}
-- | PowerPC @-mrelocatable@ flag
ef_ppc_relocatable :: Flag
ef_ppc_relocatable = Flag {#const EF_PPC_RELOCATABLE#}
-- | PowerPC @-mrelocatable-lib@ flag
ef_ppc_relocatable_lib :: Flag
ef_ppc_relocatable_lib = Flag {#const EF_PPC_RELOCATABLE_LIB#}

ef_ppc64_abi :: Flag
ef_ppc64_abi = Flag {#const EF_PPC64_ABI#}
