module Language.NStar.CodeGen.Machine.X64.Ret
( -- * Instruction encoding
  -- $encoding

  -- * Compiling
compileRet
  ) where

import Language.NStar.CodeGen.Machine.Internal.Intermediate (TypeContext, InterOpcode(..))
import Language.NStar.CodeGen.Compiler (Compiler)
import Internal.Error (internalError)
import Language.NStar.Syntax.Core (Type(RegisterContT))
import Data.Located (Located((:@)))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)

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


compileRet :: TypeContext -> Compiler [InterOpcode]
compileRet (_, _, RegisterContT r :@ _) = pure [Byte 0xFF, modRM 0b11 0x4 (registerNumber r)]
compileRet _                            = internalError $ "Unsupported instruction 'ret'."
