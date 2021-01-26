module Language.NStar.CodeGen.Machine.X64.Ret
( -- * Instruction encoding
  -- $encoding

  -- * Compiling
compileRet
  ) where

import Language.NStar.Syntax.Core (Type)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(..))
import Language.NStar.CodeGen.Compiler (Compiler)
import Internal.Error (internalError)

{- $encoding

+---------+-------------+-------+-------------+-----------------+--------------------------------------------------------------------+
|  Opcode | Instruction | Op/En | 64-bit Mode | Compat/Leg Mode |                             Description                            |
+=========+=============+=======+=============+=================+====================================================================+
|    C3   |     RET     |   ZO  |    Valid    |      Valid      |                  Near return to calling procedure.                 |
+---------+-------------+-------+-------------+-----------------+--------------------------------------------------------------------+
|    CB   |     RET     |   ZO  |    Valid    |      Valid      |                  Far return to calling procedure.                  |
+---------+-------------+-------+-------------+-----------------+--------------------------------------------------------------------+
| C2 /iw/ | RET /imm16/ |   I   |    Valid    |      Valid      | Near return to calling procedure and pop /imm16/ bytes from stack. |
+---------+-------------+-------+-------------+-----------------+--------------------------------------------------------------------+
| CA /iw/ | RET /imm16/ |   I   |    Valid    |      Valid      |  Far return to calling procedure and pop /imm16/ bytes from stack. |
+---------+-------------+-------+-------------+-----------------+--------------------------------------------------------------------+

< >    {- Blank line -}

+-------+-----------+-----------+-----------+-----------+
| Op/En | Operand 1 | Operand 2 | Operand 3 | Operand 4 |
+=======+===========+===========+===========+===========+
|   ZO  |     NA    |     NA    |     NA    |     NA    |
+-------+-----------+-----------+-----------+-----------+
|   I   |   imm16   |     NA    |     NA    |     NA    |
+-------+-----------+-----------+-----------+-----------+

-}


compileRet :: [Type] -> Compiler [InterOpcode]
compileRet []   = pure [Byte 0xC3]
                --      ^^^^^^^^^ Not sure when to generate a far return, so we'll just generate near returns for now, and see later.
compileRet args = internalError $ "Unsupported instruction 'ret " <> show args <> "'."
