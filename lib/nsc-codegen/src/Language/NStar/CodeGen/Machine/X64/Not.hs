module Language.NStar.CodeGen.Machine.X64.Not
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileNot,
  )
where

import Data.Located (Located ((:@)), getPos, unLoc)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode (Byte))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.X64.Mv (compileMv)
import Language.NStar.Typechecker.Core (Expr (..), Register)

-- $encoding
--
-- +---------------+-------------+-------+-------------+-----------------+----------------------------+
-- |     Opcode    | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode |         Description        |
-- +===============+=============+=======+=============+=================+============================+
-- | F6 /2         | NOT r/m8    | M     | Valid       | Valid           | Reverse each bit of r/m8.  |
-- +---------------+-------------+-------+-------------+-----------------+----------------------------+
-- | REX + F6 /2   | NOT r/m8*   | M     | Valid       | N.E.            | Reverse each bit of r/m8.  |
-- +---------------+-------------+-------+-------------+-----------------+----------------------------+
-- | F7 /2         | NOT r/m16   | M     | Valid       | Valid           | Reverse each bit of r/m16. |
-- +---------------+-------------+-------+-------------+-----------------+----------------------------+
-- | F7 /2         | NOT r/m32   | M     | Valid       | Valid           | Reverse each bit of r/m32. |
-- +---------------+-------------+-------+-------------+-----------------+----------------------------+
-- | REX.W + F7 /2 | NOT r/m64   | M     | Valid       | N.E.            | Reverse each bit of r/m64. |
-- +---------------+-------------+-------+-------------+-----------------+----------------------------+
--
-- - * In 64-bit mode, r/m8 can not be encoded to access the following byte registers if a REX prefix is used: AH, BH, CH, DH.
--
-- < >
--
-- +-------+------------------+-----------+-----------+-----------+
-- | Op/En | Operand 1        | Operand 2 | Operand 3 | Operand 4 |
-- +=======+==================+===========+===========+===========+
-- | M     | ModRM:r/m (r, w) | NA        | NA        | NA        |
-- +-------+------------------+-----------+-----------+-----------+

compileNot :: Expr -> Register -> Compiler [InterOpcode]
compileNot x@(ImmE i) r = (<>) <$> compileMv x r <*> compileNot (RegE (r :@ getPos i)) r
compileNot e@(RegE s) r
  | unLoc s == r = pure [rexW, Byte 0xF7, modRM 0b11 0x2 (registerNumber r)]
  -- when the destination and the source are the same register,
  -- we actually don't need to move anything
  | otherwise = ([rexW, Byte 0xF7, modRM 0b11 0x2 (registerNumber $ unLoc s)] <>) <$> compileMv e r
compileNot _ _ = undefined
