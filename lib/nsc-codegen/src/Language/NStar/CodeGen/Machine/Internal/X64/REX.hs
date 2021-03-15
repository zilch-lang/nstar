module Language.NStar.CodeGen.Machine.Internal.X64.REX where

import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(..))

-- | REX with only the W bit set, so it indicates 64-bit operand size, but no high registers.
rexW :: InterOpcode
rexW = Byte 0x48
