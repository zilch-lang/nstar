module Language.NStar.CodeGen.Machine.X64.Shiftl
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileShiftl,
  )
where

import Data.Bits (shiftL)
import Data.Functor ((<&>))
import Data.Located (Located ((:@)), unLoc)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode (Byte))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.X64.Expression (compileExprX64, int8)
import Language.NStar.CodeGen.Machine.X64.Mv (compileMv)
import Language.NStar.Syntax.Core (Immediate (I))
import Language.NStar.Typechecker.Core (Expr (..), Register)

-- $encoding
--
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- |     Opcode***    |    Instruction   | Op/En | 64-Bit Mode | Compat/Leg Mode |            Description           |
-- +==================+==================+=======+=============+=================+==================================+
-- | D0 /4            | SHL r/m8, 1      | M1    | Valid       | Valid           | Multiply r/m8 by 2, once.        |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | REX + D0 /4      | SHL r/m8**, 1    | M1    | Valid       | N.E.            | Multiply r/m8 by 2, once.        |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | D2 /4            | SHL r/m8, CL     | MC    | Valid       | Valid           | Multiply r/m8 by 2, CL times.    |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | REX + D2 /4      | SHL r/m8**, CL   | MC    | Valid       | N.E.            | Multiply r/m8 by 2, CL times.    |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | C0 /4 ib         | SHL r/m8, imm8   | MI    | Valid       | Valid           | Multiply r/m8 by 2, imm8 times.  |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | REX + C0 /4 ib   | SHL r/m8**, imm8 | MI    | Valid       | N.E.            | Multiply r/m8 by 2, imm8 times.  |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | D1 /4            | SHL r/m16,1      | M1    | Valid       | Valid           | Multiply r/m16 by 2, once.       |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | D3 /4            | SHL r/m16, CL    | MC    | Valid       | Valid           | Multiply r/m16 by 2, CL times.   |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | C1 /4 ib         | SHL r/m16, imm8  | MI    | Valid       | Valid           | Multiply r/m16 by 2, imm8 times. |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | D1 /4            | SHL r/m32,1      | M1    | Valid       | Valid           | Multiply r/m32 by 2, once.       |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | REX.W + D1 /4    | SHL r/m64,1      | M1    | Valid       | N.E.            | Multiply r/m64 by 2, once.       |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | D3 /4            | SHL r/m32, CL    | MC    | Valid       | Valid           | Multiply r/m32 by 2, CL times.   |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | REX.W + D3 /4    | SHL r/m64, CL    | MC    | Valid       | N.E.            | Multiply r/m64 by 2, CL times.   |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | C1 /4 ib         | SHL r/m32, imm8  | MI    | Valid       | Valid           | Multiply r/m32 by 2, imm8 times. |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | REX.W + C1 /4 ib | SHL r/m64, imm8  | MI    | Valid       | N.E.            | Multiply r/m64 by 2, imm8 times. |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
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

compileShiftl :: Expr -> Integer -> Register -> Compiler [InterOpcode]
compileShiftl x@(RegE r1) n r2
  | unLoc r1 == r2 = pure bytes
  | otherwise = compileMv x r2 <&> (<> bytes)
  where
    bytes = [rexW, Byte 0xC1, modRM 0b11 0x4 (registerNumber r2)] <> (Byte <$> int8 n)
compileShiftl (ImmE (I i :@ p)) n r = compileMv (ImmE $ I (i `shiftL` fromIntegral n) :@ p) r
compileShiftl _ _ _ = undefined
