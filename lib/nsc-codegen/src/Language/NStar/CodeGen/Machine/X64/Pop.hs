module Language.NStar.CodeGen.Machine.X64.Pop
( -- * Instruction encoding
  -- $encoding

  -- * Compiling
compilePop
) where

import Language.NStar.Syntax.Core (Expr(..), Type, Immediate(..))
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(..))
import Data.Located (Located((:@)), unLoc)
import Internal.Error (internalError)
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.SIB (sib)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)

{- $encoding

+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
| Opcode | Instruction | Op/En | 64-bit Mode | Compat/Leg Mode |                                       Description                                      |
+========+=============+=======+=============+=================+========================================================================================+
|  8F /0 |  POP r/m16  |   M   |    Valid    |      Valid      |                   Pop top of stack into m16; increment stack pointer.                  |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
|  8F /0 |  POP r/m32  |   M   |     N.E.    |      Valid      |                   Pop top of stack into m32; increment stack pointer.                  |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
|  8F /0 |  POP r/m64  |   M   |    Valid    |       N.E.      | Pop top of stack into m64; increment stack pointer. Cannot encode 32-bit operand size. |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
| 58+ rw |   POP r16   |   O   |    Valid    |      Valid      |                   Pop top of stack into r16; increment stack pointer.                  |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
| 58+ rd |   POP r32   |   O   |     N.E.    |      Valid      |                   Pop top of stack into r32; increment stack pointer.                  |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
| 58+ rd |   POP r64   |   O   |    Valid    |       N.E.      | Pop top of stack into r64; increment stack pointer. Cannot encode 32-bit operand size. |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
|   1F   |    POP DS   |   ZO  |   Invalid   |      Valid      |                   Pop top of stack into DS; increment stack pointer.                   |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
|   07   |    POP ES   |   ZO  |   Invalid   |      Valid      |                   Pop top of stack into ES; increment stack pointer.                   |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
|   17   |    POP SS   |   ZO  |   Invalid   |      Valid      |                   Pop top of stack into SS; increment stack pointer.                   |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
|  0F A1 |    POP FS   |   ZO  |    Valid    |      Valid      |              Pop top of stack into FS; increment stack pointer by 16 bits.             |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
|  0F A1 |    POP FS   |   ZO  |     N.E.    |      Valid      |              Pop top of stack into FS; increment stack pointer by 32 bits.             |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
|  0F A1 |    POP FS   |   ZO  |    Valid    |       N.E.      |              Pop top of stack into FS; increment stack pointer by 64 bits.             |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
|  0F A9 |    POP GS   |   ZO  |    Valid    |      Valid      |              Pop top of stack into GS; increment stack pointer by 16 bits.             |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
|  0F A9 |    POP GS   |   ZO  |     N.E.    |      Valid      |              Pop top of stack into GS; increment stack pointer by 32 bits.             |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+
|  0F A9 |    POP GS   |   ZO  |    Valid    |       N.E.      |              Pop top of stack into GS; increment stack pointer by 64 bits.             |
+--------+-------------+-------+-------------+-----------------+----------------------------------------------------------------------------------------+

< > {- Blank line -}

+-------+-----------------+-----------+-----------+-----------+
| Op/En |    Operand 1    | Operand 2 | Operand 3 | Operand 4 |
+=======+=================+===========+===========+===========+
|   M   |  ModRM:r/m (w)  |     NA    |     NA    |     NA    |
+-------+-----------------+-----------+-----------+-----------+
|   O   | opcode + rd (w) |     NA    |     NA    |     NA    |
+-------+-----------------+-----------+-----------+-----------+
|   ZO  |        NA       |     NA    |     NA    |     NA    |
+-------+-----------------+-----------+-----------+-----------+
-}

compilePop :: Expr -> [Type] -> Compiler [InterOpcode]
compilePop (Reg dst) [_]              = pure [Byte $ 0x58 + registerNumber (unLoc dst)]
compilePop (Indexed index base) [_]   = pure $ [Byte 0x8F] <> case (unLoc index, unLoc base) of
  (Imm (I disp :@ _), Name l) -> [modRM 0x0 0x0 0x4, sib 0x0 0x4 0x5, Symbol32 (unLoc l) disp]
  (_, _)                      -> internalError $ "Unsupported instruction 'pop " <> show index <> "(" <> show base <> ") [_]'."
compilePop dst ts                     = internalError $ "Unsupported instruction 'pop " <> show dst <> " " <> show ts <> "'."
