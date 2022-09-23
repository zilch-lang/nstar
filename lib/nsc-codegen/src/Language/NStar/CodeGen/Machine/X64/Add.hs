module Language.NStar.CodeGen.Machine.X64.Add
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileAdd,
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
import Language.NStar.Syntax.Core (Immediate (..))
import Language.NStar.Typechecker.Core (Expr (..), Register)

-- $encoding
--
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- |      Opcode      |    Instruction   | Op/En | 64-bit Mode | Compat/Leg Mode |                  Description                 |
-- +==================+==================+=======+=============+=================+==============================================+
-- | 04 ib            | ADD AL, imm8     | I     | Valid       | Valid           | Add imm8 to AL.                              |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | 05 iw            | ADD AX, imm16    | I     | Valid       | Valid           | Add imm16 to AX.                             |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | 05 id            | ADD EAX, imm32   | I     | Valid       | Valid           | Add imm32 to EAX.                            |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | REX.W + 05 id    | ADD RAX, imm32   | I     | Valid       | N.E.            | Add imm32 sign-extended to 64-bits to RAX.   |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | 80 /0 ib         | ADD r/m8, imm8   | MI    | Valid       | Valid           | Add imm8 to r/m8.                            |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | REX + 80 /0 ib   | ADD r/m8*, imm8  | MI    | Valid       | N.E.            | Add sign-extended imm8 to r/m8.              |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | 81 /0 iw         | ADD r/m16, imm16 | MI    | Valid       | Valid           | Add imm16 to r/m16.                          |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | 81 /0 id         | ADD r/m32, imm32 | MI    | Valid       | Valid           | Add imm32 to r/m32.                          |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | REX.W + 81 /0 id | ADD r/m64, imm32 | MI    | Valid       | N.E.            | Add imm32 sign-extended to 64-bits to r/m64. |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | 83 /0 ib         | ADD r/m16, imm8  | MI    | Valid       | Valid           | Add sign-extended imm8 to r/m16.             |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | 83 /0 ib         | ADD r/m32, imm8  | MI    | Valid       | Valid           | Add sign-extended imm8 to r/m32.             |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | REX.W + 83 /0 ib | ADD r/m64, imm8  | MI    | Valid       | N.E.            | Add sign-extended imm8 to r/m64.             |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | 00 /r            | ADD r/m8, r8     | MR    | Valid       | Valid           | Add r8 to r/m8.                              |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | REX + 00 /r      | ADD r/m8*, r8*   | MR    | Valid       | N.E.            | Add r8 to r/m8.                              |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | 01 /r            | ADD r/m16, r16   | MR    | Valid       | Valid           | Add r16 to r/m16.                            |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | 01 /r            | ADD r/m32, r32   | MR    | Valid       | Valid           | Add r32 to r/m32.                            |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | REX.W + 01 /r    | ADD r/m64, r64   | MR    | Valid       | N.E.            | Add r64 to r/m64.                            |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | 02 /r            | ADD r8, r/m8     | RM    | Valid       | Valid           | Add r/m8 to r8.                              |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | REX + 02 /r      | ADD r8*, r/m8*   | RM    | Valid       | N.E.            | Add r/m8 to r8.                              |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | 03 /r            | ADD r16, r/m16   | RM    | Valid       | Valid           | Add r/m16 to r16.                            |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | 03 /r            | ADD r32, r/m32   | RM    | Valid       | Valid           | Add r/m32 to r32.                            |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
-- | REX.W + 03 /r    | ADD r64, r/m64   | RM    | Valid       | N.E.            | Add r/m64 to r64.                            |
-- +------------------+------------------+-------+-------------+-----------------+----------------------------------------------+
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

compileAdd :: Expr -> Expr -> Register -> Compiler [InterOpcode]
compileAdd x@(RegE a) (RegE b) r
  | unLoc a == r = pure [rexW, Byte 0x03, modRM 0b11 (registerNumber $ unLoc b) (registerNumber r)]
  | unLoc b == r = undefined
  | otherwise = compileMv x r <&> (<> [rexW, Byte 0x03, modRM 0b11 (registerNumber $ unLoc b) (registerNumber r)])
compileAdd i@(ImmE n) j@(RegE b) r = case unLoc n of
  I 1 -> compileMv j r <&> (<> [rexW, Byte 0xFF, modRM 0b11 0x0 (registerNumber r)])
  I (-1) -> compileMv j r <&> (<> [rexW, Byte 0xFF, modRM 0b11 0x1 (registerNumber r)])
  --   ^^^ use INC instead of ADD if we know we are incrementing by ±1
  _ -> compileMv i r <&> (<> [rexW, Byte 0x03, modRM 0b11 (registerNumber $ unLoc b) (registerNumber r)])
compileAdd i j@(ImmE n) r =
  compileMv i r >>= \tmp ->
    (tmp <>) <$> case unLoc n of
      I 1 -> pure [rexW, Byte 0xFF, modRM 0b11 0x0 (registerNumber r)]
      I (-1) -> pure [rexW, Byte 0xFF, modRM 0b11 0x1 (registerNumber r)]
      --   ^^^ use INC instead of ADD if we know we are incrementing by ±1
      _ -> ([Byte 0x81, modRM 0b11 0x0 (registerNumber r)] <>) <$> compileExprX64 32 j
-- TODO: check if @i@ is @RegE r@ and do not move if yes
compileAdd _ _ _ = undefined
