{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Elf.ProgramHeader.Flags
( PFlags

  -- * Flags set
, pf_x, pf_w, pf_r
) where

#include <elf.h>

import Data.Bits (Bits, shiftL)
import Data.Elf.Types

type PFlags = Flag

newtype Flag = Flag Elf64_Word
  deriving (Show, Eq, Ord, Bits, Num, Integral, Real, Enum)


-- | Convenient alias of @'shiftL'@ because the imported constants use @'<<'@.
(<<) :: Bits a => a -> Int -> a
(<<) = shiftL


-- | Segment is executable
pf_x :: Flag
pf_x = Flag {#const PF_X#}
-- | Segment is writable
pf_w :: Flag
pf_w = Flag {#const PF_W#}
-- | Segment is readable
pf_r :: Flag
pf_r = Flag {#const PF_R#}
