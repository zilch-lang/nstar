{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Elf.FileHeader.Flags
( EFlags
, ef_none
  -- * SUN SPARC specific flags
, ef_sparcv9_mm, ef_sparcv9_tso, ef_sparcv9_pso, ef_sparcv9_rmo, ef_sparc_ledata
, ef_sparc_32plus, ef_sparc_sun_us1, ef_sparc_hal_r1, ef_sparc_sun_us3
  -- * Alpha specific flags
, ef_alpha_32bit, ef_alpha_canrelax
  -- * PowerPC specific flags
, ef_ppc_emb, ef_ppc_relocatable, ef_ppc_relocatable_lib, ef_ppc64_abi
  -- * ARM specific flags
, ef_arm_relexec, ef_arm_hasentry, ef_arm_interwork, ef_arm_apcs_26, ef_arm_apcs_float
, ef_arm_pic, ef_arm_align8, ef_arm_new_abi, ef_arm_old_abi, ef_arm_soft_float, ef_arm_vfp_float
, ef_arm_maverick_float, ef_arm_abi_float_soft, ef_arm_abi_float_hard
, ef_arm_symsaresorted, ef_arm_dynsymsusesegidx, ef_arm_mapsymsfirst
, ef_arm_be8, ef_arm_le8
, ef_arm_eabi_unknown, ef_arm_eabi_ver1, ef_arm_eabi_ver2, ef_arm_eabi_ver3, ef_arm_eabi_ver4, ef_arm_eabi_ver5
  -- * IA-64 specific flags
, ef_ia_64_abi64, ef_ia_64_arch
  -- * C-SKY specific flags
, ef_csky_other, ef_csky_processor, ef_csky_abiv1, ef_csky_abiv2
) where

#include <elf.h>

import Data.Elf.Types
import Data.Bits (Bits, (.&.))
import Data.Elf.Internal.BusSize

type EFlags = Flag
-- | Processor specific flags
newtype Flag (n :: Size) = Flag (Elf_Word n)
--  deriving (Show, Eq, Ord, Num, Bits, Integral, Real, Enum)
deriving instance ValueSet n => Show (Flag n)
deriving instance ValueSet n => Eq (Flag n)
deriving instance ValueSet n => Ord (Flag n)
deriving instance ValueSet n => Num (Flag n)
deriving instance ValueSet n => Bits (Flag n)
deriving instance ValueSet n => Integral (Flag n)
deriving instance ValueSet n => Real (Flag n)
deriving instance ValueSet n => Enum (Flag n)

-- | No flags
ef_none :: ValueSet n => Flag n
ef_none = Flag 0

-- SPARC specific flags

ef_sparcv9_mm :: ValueSet n => Flag n
ef_sparcv9_mm = Flag {#const EF_SPARCV9_MM#}

ef_sparcv9_tso :: ValueSet n => Flag n
ef_sparcv9_tso = Flag {#const EF_SPARCV9_TSO#}

ef_sparcv9_pso :: ValueSet n => Flag n
ef_sparcv9_pso = Flag {#const EF_SPARCV9_PSO#}

ef_sparcv9_rmo :: ValueSet n => Flag n
ef_sparcv9_rmo = Flag {#const EF_SPARCV9_RMO#}
-- | little endian data
ef_sparc_ledata :: ValueSet n => Flag n
ef_sparc_ledata = Flag {#const EF_SPARC_LEDATA#}

-- | generic v8+ features
ef_sparc_32plus :: ValueSet n => Flag n
ef_sparc_32plus = Flag {#const EF_SPARC_32PLUS#}
-- | Sun UltraSPARC1 extensions
ef_sparc_sun_us1 :: ValueSet n => Flag n
ef_sparc_sun_us1 = Flag {#const EF_SPARC_SUN_US1#}
-- | HAL R1 extensions
ef_sparc_hal_r1 :: ValueSet n => Flag n
ef_sparc_hal_r1 = Flag {#const EF_SPARC_HAL_R1#}
-- | Sun UltraSPARCIII extensions
ef_sparc_sun_us3 :: ValueSet n => Flag n
ef_sparc_sun_us3 = Flag {#const EF_SPARC_SUN_US3#}

-- Alpha specific flags

-- | All addresses must be < 2GB
ef_alpha_32bit :: ValueSet n => Flag n
ef_alpha_32bit = Flag {#const EF_ALPHA_32BIT#}
-- | Relocations for relaxing exist
ef_alpha_canrelax :: ValueSet n => Flag n
ef_alpha_canrelax = Flag {#const EF_ALPHA_CANRELAX#}

-- PowerPC specific flags

-- | PowerPC embedded flag
ef_ppc_emb :: ValueSet n => Flag n
ef_ppc_emb = Flag {#const EF_PPC_EMB#}
-- | PowerPC @-mrelocatable@ flag
ef_ppc_relocatable :: ValueSet n => Flag n
ef_ppc_relocatable = Flag {#const EF_PPC_RELOCATABLE#}
-- | PowerPC @-mrelocatable-lib@ flag
ef_ppc_relocatable_lib :: ValueSet n => Flag n
ef_ppc_relocatable_lib = Flag {#const EF_PPC_RELOCATABLE_LIB#}

ef_ppc64_abi :: ValueSet n => Flag n
ef_ppc64_abi = Flag {#const EF_PPC64_ABI#}

-- ARM specific flags

ef_arm_relexec :: ValueSet n => Flag n
ef_arm_relexec = Flag {#const EF_ARM_RELEXEC#}

ef_arm_hasentry :: ValueSet n => Flag n
ef_arm_hasentry = Flag {#const EF_ARM_HASENTRY#}

ef_arm_interwork :: ValueSet n => Flag n
ef_arm_interwork = Flag {#const EF_ARM_INTERWORK#}

ef_arm_apcs_26 :: ValueSet n => Flag n
ef_arm_apcs_26 = Flag {#const EF_ARM_APCS_26#}

ef_arm_apcs_float :: ValueSet n => Flag n
ef_arm_apcs_float = Flag {#const EF_ARM_APCS_FLOAT#}

ef_arm_pic :: ValueSet n => Flag n
ef_arm_pic = Flag {#const EF_ARM_PIC#}
-- | 8-bit structure alignment is in use
ef_arm_align8 :: ValueSet n => Flag n
ef_arm_align8 = Flag {#const EF_ARM_ALIGN8#}

ef_arm_new_abi :: ValueSet n => Flag n
ef_arm_new_abi = Flag {#const EF_ARM_NEW_ABI#}

ef_arm_old_abi :: ValueSet n => Flag n
ef_arm_old_abi = Flag {#const EF_ARM_OLD_ABI#}

ef_arm_soft_float :: ValueSet n => Flag n
ef_arm_soft_float = Flag {#const EF_ARM_SOFT_FLOAT#}

ef_arm_vfp_float :: ValueSet n => Flag n
ef_arm_vfp_float = Flag {#const EF_ARM_VFP_FLOAT#}

ef_arm_maverick_float :: ValueSet n => Flag n
ef_arm_maverick_float = Flag {#const EF_ARM_MAVERICK_FLOAT#}
-- | Conflicts with 'ef_arm_soft_float'
ef_arm_abi_float_soft :: ValueSet n => Flag n
ef_arm_abi_float_soft = Flag {#const EF_ARM_ABI_FLOAT_SOFT#}
-- | Conflicts with 'ef_arm_vfp_float'
ef_arm_abi_float_hard :: ValueSet n => Flag n
ef_arm_abi_float_hard = Flag {#const EF_ARM_ABI_FLOAT_HARD#}

-- Other constants defined in the ARM ELF spec (v B-01).
-- NOTE: these conflict with the flags defined above.

ef_arm_symsaresorted :: ValueSet n => Flag n
ef_arm_symsaresorted = Flag {#const EF_ARM_SYMSARESORTED#}

ef_arm_dynsymsusesegidx :: ValueSet n => Flag n
ef_arm_dynsymsusesegidx = Flag {#const EF_ARM_DYNSYMSUSESEGIDX#}

ef_arm_mapsymsfirst :: ValueSet n => Flag n
ef_arm_mapsymsfirst = Flag {#const EF_ARM_MAPSYMSFIRST#}

-- Constants defined in AAELF.

ef_arm_be8 :: ValueSet n => Flag n
ef_arm_be8 = Flag {#const EF_ARM_BE8#}

ef_arm_le8 :: ValueSet n => Flag n
ef_arm_le8 = Flag {#const EF_ARM_LE8#}

ef_arm_eabi_unknown :: ValueSet n => Flag n
ef_arm_eabi_unknown = Flag {#const EF_ARM_EABI_UNKNOWN#}

ef_arm_eabi_ver1 :: ValueSet n => Flag n
ef_arm_eabi_ver1 = Flag {#const EF_ARM_EABI_VER1#}

ef_arm_eabi_ver2 :: ValueSet n => Flag n
ef_arm_eabi_ver2 = Flag {#const EF_ARM_EABI_VER2#}

ef_arm_eabi_ver3 :: ValueSet n => Flag n
ef_arm_eabi_ver3 = Flag {#const EF_ARM_EABI_VER3#}

ef_arm_eabi_ver4 :: ValueSet n => Flag n
ef_arm_eabi_ver4 = Flag {#const EF_ARM_EABI_VER4#}

ef_arm_eabi_ver5 :: ValueSet n => Flag n
ef_arm_eabi_ver5 = Flag {#const EF_ARM_EABI_VER5#}

-- IA-64 specific flags

-- | 64-bit ABI
ef_ia_64_abi64 :: ValueSet n => Flag n
ef_ia_64_abi64 = Flag {#const EF_IA_64_ABI64#}

-- | architecture version mask
ef_ia_64_arch :: ValueSet n => Flag n
ef_ia_64_arch = Flag {#const EF_IA_64_ARCH#}


-- C-SKY specific flags

ef_csky_other :: ValueSet n => Flag n
ef_csky_other = Flag {#const EF_CSKY_OTHER#}

ef_csky_processor :: ValueSet n => Flag n
ef_csky_processor = Flag {#const EF_CSKY_PROCESSOR#}

ef_csky_abiv1 :: ValueSet n => Flag n
ef_csky_abiv1 = Flag {#const EF_CSKY_ABIV1#}

ef_csky_abiv2 :: ValueSet n => Flag n
ef_csky_abiv2 = Flag {#const EF_CSKY_ABIV2#}
