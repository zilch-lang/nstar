{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Elf.SectionHeader.Flags
( SFlags
  -- * Flags set
, shf_write, shf_alloc, shf_execinstr, shf_merge, shf_strings, shf_info_link
, shf_link_order, shf_os_nonconforming, shf_group, shf_tls, shf_compressed
, shf_ordered -- , shf_exclude
) where

import Data.Bits (Bits, shiftL)
import Data.Elf.Types (Elf_Xword)
import Data.Elf.Internal.BusSize
import GHC.Generics (Generic)

#ifndef _WIN32
#  include <elf.h>
#else 
#  include "elf.h"
#endif

type SFlags = Flag
-- | Section flags
newtype Flag (n :: Size) = Flag (Elf_Xword n)
  deriving (Generic)
--  deriving (Show, Eq, Ord, Num, Bits, Integral, Real, Enum)
deriving instance Show (Flag n)
deriving instance Eq (Flag n)
deriving instance Ord (Flag n)
deriving instance Num (Flag n)
deriving instance Bits (Flag n)
deriving instance Integral (Flag n)
deriving instance Real (Flag n)
deriving instance Enum (Flag n)

-- | Convenient alias of @'shiftL'@ because the imported constants use @'<<'@.
(<<) :: Bits a => a -> Int -> a
(<<) = shiftL

-- | Writable
shf_write :: Flag n
shf_write = Flag {#const SHF_WRITE#}
-- | Occupies memory during execution
shf_alloc :: Flag n
shf_alloc = Flag {#const SHF_ALLOC#}
-- | Executable
shf_execinstr :: Flag n
shf_execinstr = Flag {#const SHF_EXECINSTR#}
-- | Might me merged
shf_merge :: Flag n
shf_merge = Flag {#const SHF_MERGE#}
-- | Contains nul-terminated strings
shf_strings :: Flag n
shf_strings = Flag {#const SHF_STRINGS#}
-- | @sh_info@ contains SHT index
shf_info_link :: Flag n
shf_info_link = Flag {#const SHF_INFO_LINK#}
-- | Preserve order after combining
shf_link_order :: Flag n
shf_link_order = Flag {#const SHF_LINK_ORDER#}
-- | Non-standard OS specific handling required
shf_os_nonconforming :: Flag n
shf_os_nonconforming = Flag {#const SHF_OS_NONCONFORMING#}
-- | Section is member of a group
shf_group :: Flag n
shf_group = Flag {#const SHF_GROUP#}
-- | Section hold thread-local data
shf_tls :: Flag n
shf_tls = Flag {#const SHF_TLS#}
-- | Section with compressed data
shf_compressed :: Flag n
shf_compressed = Flag {#const SHF_COMPRESSED#}
-- | Special ordering requirement (Solaris)
shf_ordered :: Flag n
shf_ordered = Flag {#const SHF_ORDERED#}
-- -- | Section is excluded unless referenced or allocated (Solaris)
-- shf_exclude :: Flag n
-- shf_exclude = Flag {#const SHF_EXCLUDE#}
-- FIXME: figure out how to transform @1U@ into a valid Haskell integer
