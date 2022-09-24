module Language.NStar.CodeGen.Machine.X64.Shiftr
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileShiftr,
  )
where

import Data.Bits (shiftR)
import Data.Functor ((<&>))
import Data.Located (Located ((:@)), unLoc)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode (Byte))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.X64.Expression (int8)
import Language.NStar.CodeGen.Machine.X64.Mv (compileMv)
import Language.NStar.Syntax.Core (Immediate (I))
import Language.NStar.Typechecker.Core (Expr (..), Register)

-- $encoding
--
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
-- |     Opcode***    |    Instruction   | Op/En | 64-Bit Mode | Compat/Leg Mode |               Description               |
-- +==================+==================+=======+=============+=================+=========================================+
-- | REX + D0 /5      | SHR r/m8**, 1    | M1    | Valid       | N.E.            | Unsigned divide r/m8 by 2, once.        |
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
-- | D2 /5            | SHR r/m8, CL     | MC    | Valid       | Valid           | Unsigned divide r/m8 by 2, CL times.    |
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
-- | REX + D2 /5      | SHR r/m8**, CL   | MC    | Valid       | N.E.            | Unsigned divide r/m8 by 2, CL times.    |
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
-- | C0 /5 ib         | SHR r/m8, imm8   | MI    | Valid       | Valid           | Unsigned divide r/m8 by 2, imm8 times.  |
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
-- | REX + C0 /5 ib   | SHR r/m8**, imm8 | MI    | Valid       | N.E.            | Unsigned divide r/m8 by 2, imm8 times.  |
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
-- | D1 /5            | SHR r/m16, 1     | M1    | Valid       | Valid           | Unsigned divide r/m16 by 2, once.       |
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
-- | D3 /5            | SHR r/m16, CL    | MC    | Valid       | Valid           | Unsigned divide r/m16 by 2, CL times    |
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
-- | C1 /5 ib         | SHR r/m16, imm8  | MI    | Valid       | Valid           | Unsigned divide r/m16 by 2, imm8 times. |
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
-- | D1 /5            | SHR r/m32, 1     | M1    | Valid       | Valid           | Unsigned divide r/m32 by 2, once.       |
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
-- | REX.W + D1 /5    | SHR r/m64, 1     | M1    | Valid       | N.E.            | Unsigned divide r/m64 by 2, once.       |
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
-- | D3 /5            | SHR r/m32, CL    | MC    | Valid       | Valid           | Unsigned divide r/m32 by 2, CL times.   |
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
-- | REX.W + D3 /5    | SHR r/m64, CL    | MC    | Valid       | N.E.            | Unsigned divide r/m64 by 2, CL times.   |
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
-- | C1 /5 ib         | SHR r/m32, imm8  | MI    | Valid       | Valid           | Unsigned divide r/m32 by 2, imm8 times. |
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
-- | REX.W + C1 /5 ib | SHR r/m64, imm8  | MI    | Valid       | N.E.            | Unsigned divide r/m64 by 2, imm8 times. |
-- +------------------+------------------+-------+-------------+-----------------+-----------------------------------------+
--
-- - * Not the same for m of division as IDIV; rounding is toward negative infinity.
--
-- - ** In 64-bit mode, r/m8 can not be encoded to access the following byte registers if a REX prefix is used: AH, BH, CH, DH.
--
-- - *** See IA-32 Architecture Compatibility section below.
--
-- < >
--
-- +-------+------------------+-----------+-----------+-----------+
-- | Op/En | Operand 1        | Operand 2 | Operand 3 | Operand 4 |
-- +=======+==================+===========+===========+===========+
-- | M1    | ModRM:r/m (r, w) | 1         | NA        | NA        |
-- +-------+------------------+-----------+-----------+-----------+
-- | MC    | ModRM:r/m (r, w) | CL        | NA        | NA        |
-- +-------+------------------+-----------+-----------+-----------+
-- | MI    | ModRM:r/m (r, w) | imm8      | NA        | NA        |
-- +-------+------------------+-----------+-----------+-----------+

compileShiftr :: Expr -> Integer -> Register -> Compiler [InterOpcode]
compileShiftr x@(RegE r1) n r2
  | unLoc r1 == r2 = pure bytes
  | otherwise = compileMv x r2 <&> (<> bytes)
  where
    bytes = [rexW, Byte 0xC1, modRM 0b11 0x5 (registerNumber r2)] <> (Byte <$> int8 n)
compileShiftr (ImmE (I i :@ p)) n r = compileMv (ImmE $ I (i `shiftR` fromIntegral n) :@ p) r
compileShiftr _ _ _ = undefined
