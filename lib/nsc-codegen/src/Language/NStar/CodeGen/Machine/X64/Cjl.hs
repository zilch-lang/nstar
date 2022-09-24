module Language.NStar.CodeGen.Machine.X64.Cjl
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileCjl,
  )
where

import Data.Functor ((<&>))
import Data.Located (unLoc)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode (..))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.X64.Expression (compileExprX64, int8)
import Language.NStar.CodeGen.Machine.X64.Jmp (compileJmp)
import Language.NStar.Syntax.Core (Expr (..), Immediate (..))

-- $encoding
--
-- +----------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------+
-- |  Opcode  | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode |                              Description                             |
-- +==========+=============+=======+=============+=================+======================================================================+
-- | 0F 8C cw | JL rel16    | D     | N.S.        | Valid           | Jump near if less (SF≠ OF). Not supported in 64-bit mode.            |
-- +----------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------+
-- | 0F 8C cd | JL rel32    | D     | Valid       | Valid           | Jump near if less (SF≠ OF).                                          |
-- +----------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------+
-- | 0F 8D cw | JGE rel16   | D     | N.S.        | Valid           | Jump near if greater or equal (SF=OF). Not supported in 64-bit mode. |
-- +----------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------+
-- | 0F 8D cd | JGE rel32   | D     | Valid       | Valid           | Jump near if greater or equal (SF=OF).                               |
-- +----------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------+
--
-- < >
--
-- +-------+-----------+-----------+-----------+-----------+
-- | Op/En | Operand 1 | Operand 2 | Operand 3 | Operand 4 |
-- +=======+===========+===========+===========+===========+
-- | D     | Offset    | NA        | NA        | NA        |
-- +-------+-----------+-----------+-----------+-----------+

compileCjl :: Expr -> Expr -> Expr -> Expr -> Compiler [InterOpcode]
compileCjl (RegE a) (RegE b) (NameE m _) (NameE n _) =
  pure $
    -- cmp a b
    [rexW, Byte 0x39, modRM 0b11 (registerNumber $ unLoc b) (registerNumber $ unLoc a)]
      <>
      -- jl m
      [Byte 0x0F, Byte 0x8C, Jump (unLoc m)]
      <>
      -- jge n
      [Byte 0x0F, Byte 0x8D, Jump (unLoc n)]
compileCjl (RegE a) j@(ImmE _) (NameE m _) (NameE n _) =
  -- cmp a j
  (([rexW, Byte 0x81, modRM 0b11 0x7 (registerNumber $ unLoc a)] <>) <$> compileExprX64 32 j)
    <&> ( <> -- jl m
            [Byte 0x0F, Byte 0x8C, Jump (unLoc m)]
              <>
              -- jge n
              [Byte 0x0F, Byte 0x8D, Jump (unLoc n)]
        )
compileCjl i@(ImmE _) (RegE b) (NameE m _) (NameE n _) =
  -- cmp b i
  (([rexW, Byte 0x81, modRM 0b11 0x7 (registerNumber $ unLoc b)] <>) <$> compileExprX64 32 i)
    <&> ( <> -- jge m
            [Byte 0x0F, Byte 0x8D, Jump (unLoc m)]
              <>
              -- jl n
              [Byte 0x0F, Byte 0x8C, Jump (unLoc n)]
        )
compileCjl (ImmE a) (ImmE b) m@(NameE _ _) n@(NameE _ _) = case (unLoc a, unLoc b) of
  (I a, I b) | a < b -> compileJmp m
  _ -> compileJmp n
compileCjl _ _ _ _ = undefined
