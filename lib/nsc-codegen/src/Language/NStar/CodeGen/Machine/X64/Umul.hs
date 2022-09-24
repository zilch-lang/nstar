module Language.NStar.CodeGen.Machine.X64.Umul
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileUmul,
  )
where

import Control.Monad (when)
import Data.Located (Located ((:@)), Position, unLoc)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode (Byte))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.X64.Mv (compileMv)
import Language.NStar.Typechecker.Core (Expr (..), Register (..), Type)

-- $encoding
--
-- +---------------+-------------+-------+-------------+-----------------+---------------------------------------------+
-- |     Opcode    | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode |                 Description                 |
-- +===============+=============+=======+=============+=================+=============================================+
-- | F6 /4         | MUL r/m8    | M     | Valid       | Valid           | Unsigned multiply (AX := AL ∗ r/m8).        |
-- +---------------+-------------+-------+-------------+-----------------+---------------------------------------------+
-- | REX + F6 /4   | MUL r/m8*   | M     | Valid       | N.E.            | Unsigned multiply (AX := AL ∗ r/m8).        |
-- +---------------+-------------+-------+-------------+-----------------+---------------------------------------------+
-- | F7 /4         | MUL r/m16   | M     | Valid       | Valid           | Unsigned multiply (DX:AX := AX ∗ r/m16).    |
-- +---------------+-------------+-------+-------------+-----------------+---------------------------------------------+
-- | F7 /4         | MUL r/m32   | M     | Valid       | Valid           | Unsigned multiply (EDX:EAX := EAX ∗ r/m32). |
-- +---------------+-------------+-------+-------------+-----------------+---------------------------------------------+
-- | REX.W + F7 /4 | MUL r/m64   | M     | Valid       | N.E.            | Unsigned multiply (RDX:RAX := RAX ∗ r/m64). |
-- +---------------+-------------+-------+-------------+-----------------+---------------------------------------------+
--
-- - * In 64-bit mode, r/m8 can not be encoded to access the following byte registers if a REX prefix is used: AH, BH, CH, DH.
--
-- < >
--
-- +-------+---------------+-----------+-----------+-----------+
-- | Op/En | Operand 1     | Operand 2 | Operand 3 | Operand 4 |
-- +=======+===============+===========+===========+===========+
-- | M     | ModRM:r/m (r) | NA        | NA        | NA        |
-- +-------+---------------+-----------+-----------+-----------+

compileUmul :: Map (Located Register) (Located Type) -> Expr -> Expr -> Register -> Position -> Compiler [InterOpcode]
compileUmul chi a (RegE b) r p = do
  -- save RDX (%r2) and RAX (%r0) on the stack if they are used for meaningful data
  let isR2Used = isJust (Map.lookup (R2 :@ p) chi) && r /= R2
      isR0Used = isJust (Map.lookup (R0 :@ p) chi) && r /= R0

      pushR2 = if isR2Used then [Byte 0xFF, modRM 0b11 0x6 (registerNumber R2)] else []
      popR2 = if isR2Used then [Byte 0x8F, modRM 0b11 0x0 (registerNumber R2)] else []

      pushR0 = if isR0Used then [Byte 0xFF, modRM 0b11 0x6 (registerNumber R0)] else []
      popR0 = if isR0Used then [Byte 0x8F, modRM 0b11 0x0 (registerNumber R0)] else []

  when (unLoc b == r) do
    undefined

  mv1 <- compileMv a R0
  mv2 <- compileMv (RegE (R0 <$ b)) r

  pure $
    mconcat
      [ pushR0,
        pushR2,
        mv1,
        [rexW, Byte 0xF7, modRM 0b11 0x4 (registerNumber $ unLoc b)],
        mv2,
        popR2,
        popR0
      ]
-- TODO: move immediate to temporary register if second argument
compileUmul _ _ _ _ _ = undefined
