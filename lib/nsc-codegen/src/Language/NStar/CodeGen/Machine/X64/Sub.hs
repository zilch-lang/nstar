module Language.NStar.CodeGen.Machine.X64.Sub
( -- * Instruction encoding
  -- $encoding

  -- * Compiling
compileSub
) where

import Language.NStar.Syntax.Core (Expr(..), Type)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(..))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.X64.Expression (compileExprX64)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Data.Located (unLoc)
import Internal.Error (internalError)

{- $encoding

+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|      Opcode      |    Instruction   | Op/En | 64-bit Mode | Compat/Leg Mode |                     Description                     |
+==================+==================+=======+=============+=================+=====================================================+
|       2C ib      |   SUB AL, imm8   |   I   |    Valid    |      Valid      |                Subtract imm8 from AL.               |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|       2D iw      |   SUB AX, imm16  |   I   |    Valid    |      Valid      |               Subtract imm16 from AX.               |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|       2D id      |  SUB EAX, imm32  |   I   |    Valid    |      Valid      |               Subtract imm32 from EAX.              |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|   REX.W + 2D id  |  SUB RAX, imm32  |   I   |    Valid    |       N.E.      |  Subtract imm32 sign-extended to 64-bits from RAX.  |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|     80 /5 ib     |  SUB r/m8, imm8  |   MI  |    Valid    |      Valid      |               Subtract imm8 from r/m8.              |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|  REX + 80 /5 ib  |  SUB r/m8*, imm8 |   MI  |    Valid    |       N.E.      |               Subtract imm8 from r/m8.              |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|     81 /5 iw     | SUB r/m16, imm16 |   MI  |    Valid    |      Valid      |              Subtract imm16 from r/m16.             |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|     81 /5 id     | SUB r/m32, imm32 |   MI  |    Valid    |      Valid      |              Subtract imm32 from r/m32.             |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
| REX.W + 81 /5 id | SUB r/m64, imm32 |   MI  |    Valid    |       N.E.      | Subtract imm32 sign-extended to 64-bits from r/m64. |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|     83 /5 ib     |  SUB r/m16, imm8 |   MI  |    Valid    |      Valid      |       Subtract sign-extended imm8 from r/m16.       |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|     83 /5 ib     |  SUB r/m32, imm8 |   MI  |    Valid    |      Valid      |       Subtract sign-extended imm8 from r/m32.       |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
| REX.W + 83 /5 ib |  SUB r/m64, imm8 |   MI  |    Valid    |       N.E.      |       Subtract sign-extended imm8 from r/m64.       |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|       28 /r      |   SUB r/m8, r8   |   MR  |    Valid    |      Valid      |                Subtract r8 from r/m8.               |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|    REX + 28 /r   |  SUB r/m8*, r8*  |   MR  |    Valid    |       N.E.      |                Subtract r8 from r/m8.               |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|       29 /r      |  SUB r/m16, r16  |   MR  |    Valid    |      Valid      |               Subtract r16 from r/m16.              |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|       29 /r      |  SUB r/m32, r32  |   MR  |    Valid    |      Valid      |               Subtract r32 from r/m32.              |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|   REX.W + 29 /r  |  SUB r/m64, r64  |   MR  |    Valid    |       N.E.      |               Subtract r64 from r/m64.              |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|       2A /r      |   SUB r8, r/m8   |   RM  |    Valid    |      Valid      |                Subtract r/m8 from r8.               |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|    REX + 2A /r   |  SUB r8*, r/m8*  |   RM  |    Valid    |       N.E.      |                Subtract r/m8 from r8.               |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|       2B /r      |  SUB r16, r/m16  |   RM  |    Valid    |      Valid      |               Subtract r/m16 from r16.              |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|       2B /r      |  SUB r32, r/m32  |   RM  |    Valid    |      Valid      |               Subtract r/m32 from r32.              |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+
|   REX.W + 2B /r  |  SUB r64, r/m64  |   RM  |    Valid    |       N.E.      |               Subtract r/m64 from r64.              |
+------------------+------------------+-------+-------------+-----------------+-----------------------------------------------------+

- * In 64-bit mode, r/m8 cannot be encoded to access the following byte registers if a REX prefix is used: AH, BH, CH, DH.

< > {- Blank line -}

+-------+------------------+---------------+-----------+-----------+
| Op/En |     Operand 1    |   Operand 2   | Operand 3 | Operand 4 |
+=======+==================+===============+===========+===========+
|   I   |   AL/AX/EAX/RAX  |   imm8/16/32  |     NA    |     NA    |
+-------+------------------+---------------+-----------+-----------+
|   MI  | ModRM:r/m (r, w) |   imm8/16/32  |     NA    |     NA    |
+-------+------------------+---------------+-----------+-----------+
|   MR  | ModRM:r/m (r, w) | ModRM:reg (r) |     NA    |     NA    |
+-------+------------------+---------------+-----------+-----------+
|   RM  | ModRM:reg (r, w) | ModRM:r/m (r) |     NA    |     NA    |
+-------+------------------+---------------+-----------+-----------+
-}

compileSub :: Expr -> Expr -> [Type] -> Compiler [InterOpcode]
compileSub src@(ImmE _) (RegE dst) [_, _] = mappend [rexW, Byte 0x81, modRM 0x3 (registerNumber (unLoc dst)) 0x5] <$> compileExprX64 64 src
compileSub src dst ts                     = internalError $ "Unsupported instruction 'sub " <> show src <> "," <> show dst <> " " <> show ts <> "'."
