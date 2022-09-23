module Language.NStar.CodeGen.Machine.X64.Smul
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileSmul,
  )
where

import Data.Functor ((<&>))
import Data.Located (Located ((:@)), unLoc)
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
-- +------------------+------------------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- |      Opcode      |       Instruction      | Op/En | 64-Bit Mode | Compat/Leg Mode |                          Description                         |
-- +==================+========================+=======+=============+=================+==============================================================+
-- | F6 /5            | IMUL r/m8*             | M     | Valid       | Valid           | AX:= AL ∗ r/m byte.                                          |
-- +------------------+------------------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | F7 /5            | IMUL r/m16             | M     | Valid       | Valid           | DX:AX := AX ∗ r/m word.                                      |
-- +------------------+------------------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | F7 /5            | IMUL r/m32             | M     | Valid       | Valid           | EDX:EAX := EAX ∗ r/m32.                                      |
-- +------------------+------------------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | REX.W + F7 /5    | IMUL r/m64             | M     | Valid       | N.E.            | RDX:RAX := RAX ∗ r/m64.                                      |
-- +------------------+------------------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | 0F AF /r         | IMUL r16, r/m16        | RM    | Valid       | Valid           | word register := word register ∗ r/m16.                      |
-- +------------------+------------------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | 0F AF /r         | IMUL r32, r/m32        | RM    | Valid       | Valid           | doubleword register := doubleword register ∗ r/m32.          |
-- +------------------+------------------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | REX.W + 0F AF /r | IMUL r64, r/m64        | RM    | Valid       | N.E.            | Quadword register := Quadword register ∗ r/m64.              |
-- +------------------+------------------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | 6B /r ib         | IMUL r16, r/m16, imm8  | RMI   | Valid       | Valid           | word register := r/m16 ∗ sign-extended immediate byte.       |
-- +------------------+------------------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | 6B /r ib         | IMUL r32, r/m32, imm8  | RMI   | Valid       | Valid           | doubleword register := r/m32 ∗ sign-extended immediate byte. |
-- +------------------+------------------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | REX.W + 6B /r ib | IMUL r64, r/m64, imm8  | RMI   | Valid       | N.E.            | Quadword register := r/m64 ∗ sign-extended immediate byte.   |
-- +------------------+------------------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | 69 /r iw         | IMUL r16, r/m16, imm16 | RMI   | Valid       | Valid           | word register := r/m16 ∗ immediate word.                     |
-- +------------------+------------------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | 69 /r id         | IMUL r32, r/m32, imm32 | RMI   | Valid       | Valid           | doubleword register := r/m32 ∗ immediate doubleword.         |
-- +------------------+------------------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | REX.W + 69 /r id | IMUL r64, r/m64, imm32 | RMI   | Valid       | N.E.            | Quadword register := r/m64 ∗ immediate doubleword.           |
-- +------------------+------------------------+-------+-------------+-----------------+--------------------------------------------------------------+
--
-- - * In 64-bit mode, r/m8 can not be encoded to access the following byte registers if a REX prefix is used: AH, BH, CH, DH.
--
-- < >
--
-- +-------+------------------+---------------+------------+-----------+
-- | Op/En | Operand 1        | Operand 2     | Operand 3  | Operand 4 |
-- +=======+==================+===============+============+===========+
-- | M     | ModRM:r/m (r, w) | NA            | NA         | NA        |
-- +-------+------------------+---------------+------------+-----------+
-- | RM    | ModRM:reg (r, w) | ModRM:r/m (r) | NA         | NA        |
-- +-------+------------------+---------------+------------+-----------+
-- | RMI   | ModRM:reg (r, w) | ModRM:r/m (r) | imm8/16/32 | NA        |
-- +-------+------------------+---------------+------------+-----------+

compileSmul :: Expr -> Expr -> Register -> Compiler [InterOpcode]
compileSmul i@(RegE a) (RegE b) r
  | unLoc a == r = pure [rexW, Byte 0x0F, Byte 0xAF, modRM 0b11 (registerNumber $ unLoc b) (registerNumber r)]
  | unLoc b == r = undefined
  | otherwise = compileMv i r <&> (<> [rexW, Byte 0x0F, Byte 0xAF, modRM 0b11 (registerNumber $ unLoc b) (registerNumber r)])
compileSmul i@(ImmE n) j@(RegE b) r = case unLoc n of
  I 0 -> compileMv i r
  I 1 -> compileMv j r
  _ -> compileMv i r <&> (<> [rexW, Byte 0x0F, Byte 0xAF, modRM 0b11 (registerNumber $ unLoc b) (registerNumber r)])
compileSmul i@(RegE a) j@(ImmE n) r = case unLoc n of
  I 0 -> compileMv j r
  I 1 -> compileMv i r
  _ -> undefined
compileSmul (ImmE (I m :@ p)) (ImmE (I n :@ _)) r = compileMv (ImmE $ I (m * n) :@ p) r
compileSmul _ _ _ = undefined
