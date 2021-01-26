module Language.NStar.CodeGen.Machine.X64.Push
( -- * Instruction encoding
  -- $encoding

  -- * Compiling
compilePush
) where

import Language.NStar.Syntax.Core (Expr(..), Type)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(..))
import Data.Located (unLoc)
import Internal.Error (internalError)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.X64.Expression (compileExprX64)

{- $encoding

+--------+-------------+-------+-------------+-----------------+-------------+
| Opcode | Instruction | Op/En | 64-bit Mode | Compat/Leg Mode | Description |
+========+=============+=======+=============+=================+=============+
|  FF /6 |  PUSH r/m16 |   M   |    Valid    |      Valid      | Push r/m16. |
+--------+-------------+-------+-------------+-----------------+-------------+
|  FF /6 |  PUSH r/m32 |   M   |     N.E.    |      Valid      | Push r/m32. |
+--------+-------------+-------+-------------+-----------------+-------------+
|  FF /6 |  PUSH r/m64 |   M   |    Valid    |       N.E.      | Push r/m64. |
+--------+-------------+-------+-------------+-----------------+-------------+
|  50+rw |   PUSH r16  |   O   |    Valid    |      Valid      |  Push r16.  |
+--------+-------------+-------+-------------+-----------------+-------------+
|  50+rd |   PUSH r32  |   O   |     N.E.    |      Valid      |  Push r32.  |
+--------+-------------+-------+-------------+-----------------+-------------+
|  50+rd |   PUSH r64  |   O   |    Valid    |       N.E.      |  Push r64.  |
+--------+-------------+-------+-------------+-----------------+-------------+
|  6A ib |  PUSH imm8  |   I   |    Valid    |      Valid      |  Push imm8. |
+--------+-------------+-------+-------------+-----------------+-------------+
|  68 iw |  PUSH imm16 |   I   |    Valid    |      Valid      | Push imm16. |
+--------+-------------+-------+-------------+-----------------+-------------+
|  68 id |  PUSH imm32 |   I   |    Valid    |      Valid      | Push imm32. |
+--------+-------------+-------+-------------+-----------------+-------------+
|   0E   |   PUSH CS   |   ZO  |   Invalid   |      Valid      |   Push CS.  |
+--------+-------------+-------+-------------+-----------------+-------------+
|   16   |   PUSH SS   |   ZO  |   Invalid   |      Valid      |   Push SS.  |
+--------+-------------+-------+-------------+-----------------+-------------+
|   1E   |   PUSH DS   |   ZO  |   Invalid   |      Valid      |   Push DS.  |
+--------+-------------+-------+-------------+-----------------+-------------+
|   06   |   PUSH ES   |   ZO  |   Invalid   |      Valid      |   Push ES.  |
+--------+-------------+-------+-------------+-----------------+-------------+
|  0F A0 |   PUSH FS   |   ZO  |    Valid    |      Valid      |   Push FS.  |
+--------+-------------+-------+-------------+-----------------+-------------+
|  0F A8 |   PUSH GS   |   ZO  |    Valid    |      Valid      |   Push GS.  |
+--------+-------------+-------+-------------+-----------------+-------------+

< > {- Blank line -}

+-------+-----------------+-----------+-----------+-----------+
| Op/En |    Operand 1    | Operand 2 | Operand 3 | Operand 4 |
+=======+=================+===========+===========+===========+
|   M   |  ModRM:r/m (r)  |     NA    |     NA    |     NA    |
+-------+-----------------+-----------+-----------+-----------+
|   O   | opcode + rd (r) |     NA    |     NA    |     NA    |
+-------+-----------------+-----------+-----------+-----------+
|   I   |    imm8/16/32   |     NA    |     NA    |     NA    |
+-------+-----------------+-----------+-----------+-----------+
|   ZO  |        NA       |     NA    |     NA    |     NA    |
+-------+-----------------+-----------+-----------+-----------+
-}

compilePush :: Expr -> [Type] -> Compiler [InterOpcode]
compilePush (Reg src) [_]   = pure [Byte $ 0x50 + registerNumber (unLoc src)]
compilePush src@(Imm _) [_] = mappend [Byte 0x68] <$> compileExprX64 32 src
compilePush exp ts          = internalError $ "Unsupported instruction 'push " <> show exp <> " " <> show ts <> "'."
