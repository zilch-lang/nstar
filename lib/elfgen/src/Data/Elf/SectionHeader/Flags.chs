{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Elf.SectionHeader.Flags
( SFlags
  -- * Flags set
, shf_write, shf_alloc, shf_execinstr, shf_merge, shf_strings, shf_info_link
, shf_link_order, shf_os_nonconforming, shf_group, shf_tls, shf_compressed
, shf_ordered -- , shf_exclude
) where

import Data.Bits (Bits, shiftL)
import Data.Elf.Types (Elf_Xword, ValueSet)
import Data.Elf.Internal.BusSize

#include <elf.h>

type SFlags = Flag
-- | Section flags
newtype Flag (n :: Size) = Flag (Elf_Xword n)
--  deriving (Show, Eq, Ord, Num, Bits, Integral, Real, Enum)
deriving instance ValueSet n => Show (Flag n)
deriving instance ValueSet n => Eq (Flag n)
deriving instance ValueSet n => Ord (Flag n)
deriving instance ValueSet n => Num (Flag n)
deriving instance ValueSet n => Bits (Flag n)
deriving instance ValueSet n => Integral (Flag n)
deriving instance ValueSet n => Real (Flag n)
deriving instance ValueSet n => Enum (Flag n)

-- | Convenient alias of @'shiftL'@ because the imported constants use @'<<'@.
(<<) :: Bits a => a -> Int -> a
(<<) = shiftL

-- | Writable
shf_write :: ValueSet n => Flag n
shf_write = Flag {#const SHF_WRITE#}
-- | Occupies memory during execution
shf_alloc :: ValueSet n => Flag n
shf_alloc = Flag {#const SHF_ALLOC#}
-- | Executable
shf_execinstr :: ValueSet n => Flag n
shf_execinstr = Flag {#const SHF_EXECINSTR#}
-- | Might me merged
shf_merge :: ValueSet n => Flag n
shf_merge = Flag {#const SHF_MERGE#}
-- | Contains nul-terminated strings
shf_strings :: ValueSet n => Flag n
shf_strings = Flag {#const SHF_STRINGS#}
-- | @sh_info@ contains SHT index
shf_info_link :: ValueSet n => Flag n
shf_info_link = Flag {#const SHF_INFO_LINK#}
-- | Preserve order after combining
shf_link_order :: ValueSet n => Flag n
shf_link_order = Flag {#const SHF_LINK_ORDER#}
-- | Non-standard OS specific handling required
shf_os_nonconforming :: ValueSet n => Flag n
shf_os_nonconforming = Flag {#const SHF_OS_NONCONFORMING#}
-- | Section is member of a group
shf_group :: ValueSet n => Flag n
shf_group = Flag {#const SHF_GROUP#}
-- | Section hold thread-local data
shf_tls :: ValueSet n => Flag n
shf_tls = Flag {#const SHF_TLS#}
-- | Section with compressed data
shf_compressed :: ValueSet n => Flag n
shf_compressed = Flag {#const SHF_COMPRESSED#}
-- | Special ordering requirement (Solaris)
shf_ordered :: ValueSet n => Flag n
shf_ordered = Flag {#const SHF_ORDERED#}
-- -- | Section is excluded unless referenced or allocated (Solaris)
-- shf_exclude :: Flag n
-- shf_exclude = Flag {#const SHF_EXCLUDE#}
-- FIXME: figure out how to transform @1U@ into a valid Haskell integer
