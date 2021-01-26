module Language.NStar.CodeGen.Machine.X64.Jmp
( -- * Instruction encoding
  -- $encoding

  -- * Compiling
compileJmp
) where

import Language.NStar.Syntax.Core (Expr(..), Type)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(..))
import Data.Located (unLoc)
import Internal.Error (internalError)

{- $encoding

+-------------+--------------+-------+-------------+-----------------+-----------------------------------------------------------------------------------------------+
|    Opcode   |  Instruction | Op/En | 64-bit Mode | Compat/Leg Mode |                                          Description                                          |
+=============+==============+=======+=============+=================+===============================================================================================+
|    EB cb    |   JMP rel8   |   D   |    Valid    |      Valid      |              Jump short, RIP = RIP + 8-bit displacement sign extended to 64-bits              |
+-------------+--------------+-------+-------------+-----------------+-----------------------------------------------------------------------------------------------+
|    E9 cw    |   JMP rel16  |   D   |     N.S.    |      Valid      | Jump near, relative, displacement relative to next instruction. Not supported in 64-bit mode. |
+-------------+--------------+-------+-------------+-----------------+-----------------------------------------------------------------------------------------------+
|    E9 cd    |   JMP rel32  |   D   |    Valid    |      Valid      |         Jump near, relative, RIP = RIP + 32-bit displacement sign extended to 64-bits         |
+-------------+--------------+-------+-------------+-----------------+-----------------------------------------------------------------------------------------------+
|    FF /4    |   JMP r/m16  |   M   |     N.S.    |      Valid      |   Jump near, absolute indirect, address = zero-extended r/m16. Not supported in 64-bit mode.  |
+-------------+--------------+-------+-------------+-----------------+-----------------------------------------------------------------------------------------------+
|    FF /4    |   JMP r/m32  |   M   |     N.S.    |      Valid      |      Jump near, absolute indirect, address given in r/m32. Not supported in 64-bit mode.      |
+-------------+--------------+-------+-------------+-----------------+-----------------------------------------------------------------------------------------------+
|    FF /4    |   JMP r/m64  |   M   |    Valid    |       N.E.      |           Jump near, absolute indirect, RIP = 64-Bit offset from register or memory           |
+-------------+--------------+-------+-------------+-----------------+-----------------------------------------------------------------------------------------------+
|    EA cd    | JMP ptr16:16 |   D   |     Inv.    |      Valid      |                          Jump far, absolute, address given in operand                         |
+-------------+--------------+-------+-------------+-----------------+-----------------------------------------------------------------------------------------------+
|    EA cp    | JMP ptr16:32 |   D   |     Inv.    |      Valid      |                          Jump far, absolute, address given in operand                         |
+-------------+--------------+-------+-------------+-----------------+-----------------------------------------------------------------------------------------------+
|    FF /5    |  JMP m16:16  |   D   |    Valid    |      Valid      |                      Jump far, absolute indirect, address given in m16:16                     |
+-------------+--------------+-------+-------------+-----------------+-----------------------------------------------------------------------------------------------+
|    FF /5    |  JMP m16:32  |   D   |    Valid    |      Valid      |                     Jump far, absolute indirect, address given in m16:32.                     |
+-------------+--------------+-------+-------------+-----------------+-----------------------------------------------------------------------------------------------+
| REX.W FF /5 |  JMP m16:64  |   D   |    Valid    |       N.E.      |                     Jump far, absolute indirect, address given in m16:64.                     |
+-------------+--------------+-------+-------------+-----------------+-----------------------------------------------------------------------------------------------+

< > {- Blank line -}

+-------+---------------+-----------+-----------+-----------+
| Op/En |   Operand 1   | Operand 2 | Operand 3 | Operand 4 |
+=======+===============+===========+===========+===========+
|   D   |     Offset    |     NA    |     NA    |     NA    |
+-------+---------------+-----------+-----------+-----------+
|   M   | ModRM:r/m (r) |     NA    |     NA    |     NA    |
+-------+---------------+-----------+-----------+-----------+

-}

compileJmp :: Expr -> [Type] -> Compiler [InterOpcode]
compileJmp (Name n) [] = pure [Byte 0xE9, Jump (unLoc n)]
compileJmp e ts        = internalError $ "Unsupported instruction 'jmp " <> show e <> " " <> show ts <> "'."
