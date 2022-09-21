module Language.NStar.CodeGen.Machine.X64.And
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileAnd,
  )
where

import Data.Functor ((<&>))
import Data.Located (unLoc)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode (Byte))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.Internal.X64.SIB (sib)
import Language.NStar.CodeGen.Machine.X64.Expression (compileExprX64)
import Language.NStar.CodeGen.Machine.X64.Mv (compileMv)
import Language.NStar.Typechecker.Core (Expr (..), Register)

-- $encoding
--
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- |      Opcode      |    Instruction   | Op/En | 64-bit Mode | Compat/Leg Mode |                Description                |
-- +==================+==================+=======+=============+=================+===========================================+
-- | 24 ib            | AND AL, imm8     | I     | Valid       | Valid           | AL AND imm8.                              |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | 25 iw            | AND AX, imm16    | I     | Valid       | Valid           | AX AND imm16.                             |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | 25 id            | AND EAX, imm32   | I     | Valid       | Valid           | EAX AND imm32.                            |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | REX.W + 25 id    | AND RAX, imm32   | I     | Valid       | N.E.            | RAX AND imm32 sign-extended to 64-bits.   |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | 80 /4 ib         | AND r/m8, imm8   | MI    | Valid       | Valid           | r/m8 AND imm8.                            |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | REX + 80 /4 ib   | AND r/m8*, imm8  | MI    | Valid       | N.E.            | r/m8 AND imm8.                            |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | 81 /4 iw         | AND r/m16, imm16 | MI    | Valid       | Valid           | r/m16 AND imm16.                          |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | 81 /4 id         | AND r/m32, imm32 | MI    | Valid       | Valid           | r/m32 AND imm32.                          |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | REX.W + 81 /4 id | AND r/m64, imm32 | MI    | Valid       | N.E.            | r/m64 AND imm32 sign extended to 64-bits. |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | 83 /4 ib         | AND r/m16, imm8  | MI    | Valid       | Valid           | r/m16 AND imm8 (sign-extended).           |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | 83 /4 ib         | AND r/m32, imm8  | MI    | Valid       | Valid           | r/m32 AND imm8 (sign-extended).           |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | REX.W + 83 /4 ib | AND r/m64, imm8  | MI    | Valid       | N.E.            | r/m64 AND imm8 (sign-extended).           |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | 20 /r            | AND r/m8, r8     | MR    | Valid       | Valid           | r/m8 AND r8.                              |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | REX + 20 /r      | AND r/m8*, r8*   | MR    | Valid       | N.E.            | r/m64 AND r8 (sign-extended).             |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | 21 /r            | AND r/m16, r16   | MR    | Valid       | Valid           | r/m16 AND r16.                            |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | 21 /r            | AND r/m32, r32   | MR    | Valid       | Valid           | r/m32 AND r32.                            |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | REX.W + 21 /r    | AND r/m64, r64   | MR    | Valid       | N.E.            | r/m64 AND r32.                            |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | 22 /r            | AND r8, r/m8     | RM    | Valid       | Valid           | r8 AND r/m8.                              |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | REX + 22 /r      | AND r8*, r/m8*   | RM    | Valid       | N.E.            | r/m64 AND r8 (sign-extended).             |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | 23 /r            | AND r16, r/m16   | RM    | Valid       | Valid           | r16 AND r/m16.                            |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | 23 /r            | AND r32, r/m32   | RM    | Valid       | Valid           | r32 AND r/m32.                            |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
-- | REX.W + 23 /r    | AND r64, r/m64   | RM    | Valid       | N.E.            | r64 AND r/m64.                            |
-- +------------------+------------------+-------+-------------+-----------------+-------------------------------------------+
--
-- - * In 64-bit mode, r/m8 can not be encoded to access the following byte registers if a REX prefix is used: AH, BH, CH, DH.
--
-- < >
--
-- +-------+------------------+---------------+-----------+-----------+
-- | Op/En | Operand 1        | Operand 2     | Operand 3 | Operand 4 |
-- +=======+==================+===============+===========+===========+
-- | RM    | ModRM:reg (r, w) | ModRM:r/m (r) | NA        | NA        |
-- +-------+------------------+---------------+-----------+-----------+
-- | MR    | ModRM:r/m (r, w) | ModRM:reg (r) | NA        | NA        |
-- +-------+------------------+---------------+-----------+-----------+
-- | MI    | ModRM:r/m (r, w) | imm8/16/32    | NA        | NA        |
-- +-------+------------------+---------------+-----------+-----------+
-- | I     | AL/AX/EAX/RAX    | imm8/16/32    | NA        | NA        |
-- +-------+------------------+---------------+-----------+-----------+

compileAnd :: Expr -> Expr -> Register -> Compiler [InterOpcode]
compileAnd x@(RegE r1) (RegE s) r2
  | unLoc r1 == r2 = pure bytes
  -- when the destination and the source are the same register,
  -- we actually don't need to move anything
  | otherwise = compileMv x r2 <&> (<> bytes)
  where
    bytes = [rexW, Byte 0x21, modRM 0b11 (registerNumber r2) (registerNumber $ unLoc s)]
compileAnd x (RegE s) r = compileMv x r <&> (<> [rexW, Byte 0x21, modRM 0b11 (registerNumber r) (registerNumber $ unLoc s)])
compileAnd x@(RegE r1) y@(ImmE _) r2
  | unLoc r1 == r2 = bytes
  -- when the destination and the source are the same register,
  -- we actually don't need to move anything
  | otherwise = compileMv x r2 >>= \mv -> (mv <>) <$> bytes
  where
    bytes = ([rexW, Byte 0x21, modRM 0b00 (registerNumber r2) 0x4, sib 0b00 0x4 0x5] <>) <$> compileExprX64 32 y
-- strangely enough, GAS uses the `REX.W + 21 /r` call with a SIB byte instead of the `REX.W + 83 /4 ib`
-- I don't quite know why but the `REX.W + 81 /4 ib` does not seem to work for me...

-- we only allow 32-bits immediate values on x64
-- if you want 64-bits immediate values, you'll need to move them inside registers
compileAnd _ _ _ = undefined
