module Language.NStar.CodeGen.Machine.X64.Nop
( -- * Instruction encoding
  -- $encoding

  -- * Compiling
compileNop
) where

import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (TypeContext, InterOpcode(..))

{- $encoding

+-------------+-------------+-------+-------------+-----------------+--------------------------------------+
|    Opcode   | Instruction | Op/En | 64-bit Mode | Compat/Leg Mode |              Description             |
+=============+=============+=======+=============+=================+======================================+
|    NP 90    |     NOP     |   ZO  |    Valid    |      Valid      |  One byte no-operation instruction.  |
+-------------+-------------+-------+-------------+-----------------+--------------------------------------+
| NP 0F 1F /0 |  NOP r/m16  |   M   |    Valid    |      Valid      | Multi-byte no-operation instruction. |
+-------------+-------------+-------+-------------+-----------------+--------------------------------------+
| NP 0F 1F /0 |  NOP r/m32  |   M   |    Valid    |      Valid      | Multi-byte no-operation instruction. |
+-------------+-------------+-------+-------------+-----------------+--------------------------------------+

< > {- Blank line -}

+-------+---------------+-----------+-----------+-----------+
| Op/En |   Operand 1   | Operand 2 | Operand 3 | Operand 4 |
+=======+===============+===========+===========+===========+
|   ZO  |       NA      |     NA    |     NA    |     NA    |
+-------+---------------+-----------+-----------+-----------+
|   M   | ModRM:r/m (r) |     NA    |     NA    |     NA    |
+-------+---------------+-----------+-----------+-----------+

-}

compileNop :: TypeContext -> Compiler [InterOpcode]
compileNop _ = pure [Byte 0x90]
