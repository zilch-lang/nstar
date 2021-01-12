{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Elf.ProgramHeader.Flags
( PFlags

  -- * Flags set
, pf_x, pf_w, pf_r
) where

#include <elf.h>

import Data.Bits (Bits, shiftL)
import Data.Elf.Types
import Data.Elf.Internal.BusSize

type PFlags = Flag

newtype Flag (n :: Size) = Flag (Elf_Word n)
--  deriving (Show, Eq, Ord, Bits, Num, Integral, Real, Enum)
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


-- | Segment is executable
pf_x :: Flag n
pf_x = Flag {#const PF_X#}
-- | Segment is writable
pf_w :: Flag n
pf_w = Flag {#const PF_W#}
-- | Segment is readable
pf_r :: Flag n
pf_r = Flag {#const PF_R#}
