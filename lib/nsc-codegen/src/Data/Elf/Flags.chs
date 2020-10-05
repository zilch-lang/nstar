{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Elf.Flags
( Flag
, Flags
  -- * SUN SPARC specific flags
, ef_sparcv9_mm, ef_sparcv9_tso, ef_sparcv9_pso, ef_sparcv9_rmo, ef_sparc_ledata
, ef_sparc_ext_mask, ef_sparc_32plus, ef_sparc_sun_us1, ef_sparc_hal_r1, ef_sparc_sun_us3
  -- * Alpha specific flags
, ef_alpha_32bit, ef_alpha_canrelax
  -- * PowerPC specific flags
, ef_ppc_emb, ef_ppc_relocatable, ef_ppc_relocatable_lib, ef_ppc64_abi

) where

#include <elf.h>

import Data.Elf.Types
import Data.Bits (Bits)

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
