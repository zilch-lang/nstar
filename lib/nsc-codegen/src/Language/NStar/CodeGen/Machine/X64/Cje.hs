module Language.NStar.CodeGen.Machine.X64.Cje
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileCje,
  )
where

import Data.Functor ((<&>))
import Data.Located (unLoc)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode (..))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.X64.Expression (compileExprX64)
import Language.NStar.CodeGen.Machine.X64.Jmp (compileJmp)
import Language.NStar.Syntax.Core (Expr (..), Immediate (..))

-- $encoding
--
-- +----------+-------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- |  Opcode  | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode |                          Description                         |
-- +==========+=============+=======+=============+=================+==============================================================+
-- | 0F 84 cw | JE rel16    | D     | N.S.        | Valid           | Jump near if equal (ZF=1). Not supported in 64-bit mode.     |
-- +----------+-------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | 0F 84 cd | JE rel32    | D     | Valid       | Valid           | Jump near if equal (ZF=1).                                   |
-- +----------+-------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | 0F 85 cw | JNE rel16   | D     | N.S.        | Valid           | Jump near if not equal (ZF=0). Not supported in 64-bit mode. |
-- +----------+-------------+-------+-------------+-----------------+--------------------------------------------------------------+
-- | 0F 85 cd | JNE rel32   | D     | Valid       | Valid           | Jump near if not equal (ZF=0).                               |
-- +----------+-------------+-------+-------------+-----------------+--------------------------------------------------------------+
--
-- < >
--
-- +-------+-----------+-----------+-----------+-----------+
-- | Op/En | Operand 1 | Operand 2 | Operand 3 | Operand 4 |
-- +=======+===========+===========+===========+===========+
-- | D     | Offset    | NA        | NA        | NA        |
-- +-------+-----------+-----------+-----------+-----------+

compileCje :: Expr -> Expr -> Expr -> Expr -> Compiler [InterOpcode]
compileCje (RegE a) (RegE b) (NameE m _) (NameE n _) =
  pure $
    -- cmp a b
    [rexW, Byte 0x39, modRM 0b11 (registerNumber $ unLoc b) (registerNumber $ unLoc a)]
      <>
      -- je m
      [Byte 0x0F, Byte 0x84, Jump (unLoc m)]
      <>
      -- jne n
      [Byte 0x0F, Byte 0x85, Jump (unLoc n)]
compileCje (RegE a) j@(ImmE _) (NameE m _) (NameE n _) =
  -- cmp a j
  (([rexW, Byte 0x81, modRM 0b11 0x7 (registerNumber $ unLoc a)] <>) <$> compileExprX64 32 j)
    <&> ( <> -- je m
            [Byte 0x0F, Byte 0x84, Jump (unLoc m)]
              <>
              -- jne n
              [Byte 0x0F, Byte 0x85, Jump (unLoc n)]
        )
compileCje i@(ImmE _) (RegE b) (NameE m _) (NameE n _) =
  -- cmp b i
  (([rexW, Byte 0x81, modRM 0b11 0x7 (registerNumber $ unLoc b)] <>) <$> compileExprX64 32 i)
    <&> ( <> -- je m
            [Byte 0x0F, Byte 0x84, Jump (unLoc m)]
              <>
              -- jne n
              [Byte 0x0F, Byte 0x85, Jump (unLoc n)]
        )
compileCje (ImmE a) (ImmE b) m@(NameE _ _) n@(NameE _ _) = case (unLoc a, unLoc b) of
  (I a, I b) | a == b -> compileJmp m
  _ -> compileJmp n
compileCje _ _ _ _ = undefined
