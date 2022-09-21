module Language.NStar.CodeGen.Machine.X64.Xor
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileXor,
  )
where

import Data.Functor ((<&>))
import Data.Located (unLoc)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode (Byte))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.X64.Expression (compileExprX64)
import Language.NStar.CodeGen.Machine.X64.Mv (compileMv)
import Language.NStar.Typechecker.Core (Expr (..), Register)

-- $encoding
--
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- |      Opcode      |    Instruction   | Op/En | 64-Bit Mode | Compat/Leg Mode |            Description           |
-- +==================+==================+=======+=============+=================+==================================+
-- | 34 ib            | XOR AL, imm8     | I     | Valid       | Valid           | AL XOR imm8.                     |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | 35 iw            | XOR AX, imm16    | I     | Valid       | Valid           | AX XOR imm16.                    |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | 35 id            | XOR EAX, imm32   | I     | Valid       | Valid           | EAX XOR imm32.                   |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | REX.W + 35 id    | XOR RAX, imm32   | I     | Valid       | N.E.            | RAX XOR imm32 (sign-extended).   |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | 80 /6 ib         | XOR r/m8, imm8   | MI    | Valid       | Valid           | r/m8 XOR imm8.                   |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | REX + 80 /6 ib   | XOR r/m8*, imm8  | MI    | Valid       | N.E.            | r/m8 XOR imm8.                   |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | 81 /6 iw         | XOR r/m16, imm16 | MI    | Valid       | Valid           | r/m16 XOR imm16.                 |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | 81 /6 id         | XOR r/m32, imm32 | MI    | Valid       | Valid           | r/m32 XOR imm32.                 |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | REX.W + 81 /6 id | XOR r/m64, imm32 | MI    | Valid       | N.E.            | r/m64 XOR imm32 (sign-extended). |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | 83 /6 ib         | XOR r/m16, imm8  | MI    | Valid       | Valid           | r/m16 XOR imm8 (sign-extended).  |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | 83 /6 ib         | XOR r/m32, imm8  | MI    | Valid       | Valid           | r/m32 XOR imm8 (sign-extended).  |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | REX.W + 83 /6 ib | XOR r/m64, imm8  | MI    | Valid       | N.E.            | r/m64 XOR imm8 (sign-extended).  |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | 30 /r            | XOR r/m8, r8     | MR    | Valid       | Valid           | r/m8 XOR r8.                     |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | REX + 30 /r      | XOR r/m8*, r8*   | MR    | Valid       | N.E.            | r/m8 XOR r8.                     |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | 31 /r            | XOR r/m16, r16   | MR    | Valid       | Valid           | r/m16 XOR r16.                   |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | 31 /r            | XOR r/m32, r32   | MR    | Valid       | Valid           | r/m32 XOR r32.                   |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | REX.W + 31 /r    | XOR r/m64, r64   | MR    | Valid       | N.E.            | r/m64 XOR r64.                   |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | 32 /r            | XOR r8, r/m8     | RM    | Valid       | Valid           | r8 XOR r/m8.                     |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | REX + 32 /r      | XOR r8*, r/m8*   | RM    | Valid       | N.E.            | r8 XOR r/m8.                     |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | 33 /r            | XOR r16, r/m16   | RM    | Valid       | Valid           | r16 XOR r/m16.                   |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | 33 /r            | XOR r32, r/m32   | RM    | Valid       | Valid           | r32 XOR r/m32.                   |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
-- | REX.W + 33 /r    | XOR r64, r/m64   | RM    | Valid       | N.E.            | r64 XOR r/m64.                   |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------+
--
-- - * In 64-bit mode, r/m8 can not be encoded to access the following byte registers if a REX prefix is used: AH, BH, CH, DH.
--
-- < >
--
-- +-------+------------------+---------------+-----------+-----------+
-- | Op/En | Operand 1        | Operand 2     | Operand 3 | Operand 4 |
-- +=======+==================+===============+===========+===========+
-- | I     | AL/AX/EAX/RAX    | imm8/16/32    | NA        | NA        |
-- +-------+------------------+---------------+-----------+-----------+
-- | MI    | ModRM:r/m (r, w) | imm8/16/32    | NA        | NA        |
-- +-------+------------------+---------------+-----------+-----------+
-- | MR    | ModRM:r/m (r, w) | ModRM:reg (r) | NA        | NA        |
-- +-------+------------------+---------------+-----------+-----------+
-- | RM    | ModRM:reg (r, w) | ModRM:r/m (r) | NA        | NA        |
-- +-------+------------------+---------------+-----------+-----------+

compileXor :: Expr -> Expr -> Register -> Compiler [InterOpcode]
compileXor x@(RegE r1) (RegE s) r2
  | unLoc r1 == r2 = pure bytes
  -- when the destination and the source are the same register,
  -- we actually don't need to move anything
  | otherwise = compileMv x r2 <&> (<> bytes)
  where
    bytes = [rexW, Byte 0x31, modRM 0b11 (registerNumber r2) (registerNumber $ unLoc s)]
compileXor x (RegE s) r = compileMv x r <&> (<> [rexW, Byte 0x31, modRM 0b11 (registerNumber r) (registerNumber $ unLoc s)])
compileXor x@(RegE r1) y@(ImmE _) r2
  | unLoc r1 == r2 = bytes
  -- when the destination and the source are the same register,
  -- we actually don't need to move anything
  | otherwise = compileMv x r2 >>= \mv -> (mv <>) <$> bytes
  where
    bytes = ([Byte 0x81, modRM 0b11 0x6 (registerNumber r2)] <>) <$> compileExprX64 32 y
-- we only allow 32-bits immediate values on x64
-- if you want 64-bits immediate values, you'll need to move them inside registers
-- TODO: perhaps throw a warning or error for this case
compileXor _ _ _ = undefined
