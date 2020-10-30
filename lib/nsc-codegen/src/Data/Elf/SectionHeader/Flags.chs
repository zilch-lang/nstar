{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Elf.SectionHeader.Flags
( SFlags
  -- * Flags set
, shf_write, shf_alloc, shf_execinstr, shf_merge, shf_strings, shf_info_link
, shf_link_order, shf_os_nonconforming, shf_group, shf_tls, shf_compressed
, shf_ordered -- , shf_exclude
) where

import Data.Bits (Bits, shiftL)
import Data.Elf.Types (Elf64_Xword)

#include <elf.h>

type SFlags = Flag
-- | Section flags
newtype Flag = Flag Elf64_Xword
  deriving (Show, Eq, Ord, Num, Bits)

-- | Convenient alias of @'shiftL'@ because the imported constants use @'<<'@.
(<<) :: Bits a => a -> Int -> a
(<<) = shiftL

-- | Writable
shf_write :: Flag
shf_write = Flag {#const SHF_WRITE#}
-- | Occupies memory during execution
shf_alloc :: Flag
shf_alloc = Flag {#const SHF_ALLOC#}
-- | Executable
shf_execinstr :: Flag
shf_execinstr = Flag {#const SHF_EXECINSTR#}
-- | Might me merged
shf_merge :: Flag
shf_merge = Flag {#const SHF_MERGE#}
-- | Contains nul-terminated strings
shf_strings :: Flag
shf_strings = Flag {#const SHF_STRINGS#}
-- | @sh_info@ contains SHT index
shf_info_link :: Flag
shf_info_link = Flag {#const SHF_INFO_LINK#}
-- | Preserve order after combining
shf_link_order :: Flag
shf_link_order = Flag {#const SHF_LINK_ORDER#}
-- | Non-standard OS specific handling required
shf_os_nonconforming :: Flag
shf_os_nonconforming = Flag {#const SHF_OS_NONCONFORMING#}
-- | Section is member of a group
shf_group :: Flag
shf_group = Flag {#const SHF_GROUP#}
-- | Section hold thread-local data
shf_tls :: Flag
shf_tls = Flag {#const SHF_TLS#}
-- | Section with compressed data
shf_compressed :: Flag
shf_compressed = Flag {#const SHF_COMPRESSED#}
-- | Special ordering requirement (Solaris)
shf_ordered :: Flag
shf_ordered = Flag {#const SHF_ORDERED#}
-- -- | Section is excluded unless referenced or allocated (Solaris)
-- shf_exclude :: Flag
-- shf_exclude = Flag {#const SHF_EXCLUDE#}
-- FIXME: figure out how to transform @1U@ into a valid Haskell integer
