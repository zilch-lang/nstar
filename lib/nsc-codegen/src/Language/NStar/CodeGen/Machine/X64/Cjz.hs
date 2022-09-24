module Language.NStar.CodeGen.Machine.X64.Cjz
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileCjz,
  )
where

import Data.Located (unLoc)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode (..))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.X64.Expression (int8)
import Language.NStar.CodeGen.Machine.X64.Jmp (compileJmp)
import Language.NStar.Syntax.Core (Expr (..), Immediate (..))

-- $encoding
--
-- +----------+-------------+-------+-------------+-----------------+-------------------------------------------------------------+
-- |  Opcode  | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode |                         Description                         |
-- +==========+=============+=======+=============+=================+=============================================================+
-- | 0F 85 cw | JNZ rel16   | D     | N.S.        | Valid           | Jump near if not zero (ZF=0). Not supported in 64-bit mode. |
-- +----------+-------------+-------+-------------+-----------------+-------------------------------------------------------------+
-- | 0F 85 cd | JNZ rel32   | D     | Valid       | Valid           | Jump near if not zero (ZF=0).                               |
-- +----------+-------------+-------+-------------+-----------------+-------------------------------------------------------------+
-- | 0F 84 cw | JZ rel16    | D     | N.S.        | Valid           | Jump near if 0 (ZF=1). Not supported in 64-bit mode.        |
-- +----------+-------------+-------+-------------+-----------------+-------------------------------------------------------------+
-- | 0F 84 cd | JZ rel32    | D     | Valid       | Valid           | Jump near if 0 (ZF=1).                                      |
-- +----------+-------------+-------+-------------+-----------------+-------------------------------------------------------------+
--
-- < >
--
-- +-------+-----------+-----------+-----------+-----------+
-- | Op/En | Operand 1 | Operand 2 | Operand 3 | Operand 4 |
-- +=======+===========+===========+===========+===========+
-- | D     | Offset    | NA        | NA        | NA        |
-- +-------+-----------+-----------+-----------+-----------+

compileCjz :: Expr -> Expr -> Expr -> Compiler [InterOpcode]
compileCjz (RegE a) (NameE m _) (NameE n _) =
  pure $
    -- cmp a 0
    [rexW, Byte 0x83, modRM 0b11 0x7 (registerNumber $ unLoc a)] <> (Byte <$> int8 0)
      <>
      -- jz m
      [Byte 0x0F, Byte 0x84, Jump (unLoc m)]
      <>
      -- jnz n
      [Byte 0x0F, Byte 0x85, Jump (unLoc n)]
compileCjz (ImmE x) m@(NameE _ _) n@(NameE _ _) = case unLoc x of
  I 0 -> compileJmp m
  _ -> compileJmp n
compileCjz _ _ _ = undefined
