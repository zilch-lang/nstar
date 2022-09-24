module Language.NStar.CodeGen.Machine.X64.Cjle
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileCjle,
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
-- +----------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------+
-- |  Opcode  | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode |                                 Description                                |
-- +==========+=============+=======+=============+=================+============================================================================+
-- | 0F 8E cw | JLE rel16   | D     | N.S.        | Valid           | Jump near if less or equal (ZF=1 or SF≠ OF). Not supported in 64-bit mode. |
-- +----------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------+
-- | 0F 8E cd | JLE rel32   | D     | Valid       | Valid           | Jump near if less or equal (ZF=1 or SF≠ OF).                               |
-- +----------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------+
-- | 0F 8F cw | JG rel16    | D     | N.S.        | Valid           | Jump near if greater (ZF=0 and SF=OF). Not supported in 64-bit mode.       |
-- +----------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------+
-- | 0F 8F cd | JG rel32    | D     | Valid       | Valid           | Jump near if greater (ZF=0 and SF=OF).                                     |
-- +----------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------+
--
-- < >
--
-- +-------+-----------+-----------+-----------+-----------+
-- | Op/En | Operand 1 | Operand 2 | Operand 3 | Operand 4 |
-- +=======+===========+===========+===========+===========+
-- | D     | Offset    | NA        | NA        | NA        |
-- +-------+-----------+-----------+-----------+-----------+

compileCjle :: Expr -> Expr -> Expr -> Expr -> Compiler [InterOpcode]
compileCjle (RegE a) (RegE b) (NameE m _) (NameE n _) =
  pure $
    -- cmp a b
    [rexW, Byte 0x39, modRM 0b11 (registerNumber $ unLoc b) (registerNumber $ unLoc a)]
      <>
      -- jle m
      [Byte 0x0F, Byte 0x8E, Jump (unLoc m)]
      <>
      -- jg n
      [Byte 0x0F, Byte 0x8F, Jump (unLoc n)]
compileCjle (RegE a) j@(ImmE _) (NameE m _) (NameE n _) =
  -- cmp a j
  (([rexW, Byte 0x81, modRM 0b11 0x7 (registerNumber $ unLoc a)] <>) <$> compileExprX64 32 j)
    <&> ( <> -- jle m
            [Byte 0x0F, Byte 0x8E, Jump (unLoc m)]
              <>
              -- jg n
              [Byte 0x0F, Byte 0x8F, Jump (unLoc n)]
        )
compileCjle i@(ImmE _) (RegE b) (NameE m _) (NameE n _) =
  -- cmp b i
  (([rexW, Byte 0x81, modRM 0b11 0x7 (registerNumber $ unLoc b)] <>) <$> compileExprX64 32 i)
    <&> ( <> -- jg m
            [Byte 0x0F, Byte 0x8F, Jump (unLoc m)]
              <>
              -- jle n
              [Byte 0x0F, Byte 0x8E, Jump (unLoc n)]
        )
compileCjle (ImmE a) (ImmE b) m@(NameE _ _) n@(NameE _ _) = case (unLoc a, unLoc b) of
  (I a, I b) | a <= b -> compileJmp m
  _ -> compileJmp n
compileCjle _ _ _ _ = undefined
