module Language.NStar.CodeGen.PreProcessor.X64
( preprocessX64
) where

import Language.NStar.Syntax.Core
import Language.NStar.Typechecker.Core
import Data.Located (Located((:@)))

preprocessX64 :: TypedProgram -> TypedProgram
preprocessX64 = patchMovRM2RM

-- | Patches the instruction @mov \@r\/m64 \@r\/m64@, because there is no corresponding opcode.
patchMovRM2RM :: TypedProgram -> TypedProgram
patchMovRM2RM (TProgram d rd ud (TCode c :@ p)) = TProgram d rd ud (TCode newC :@ p)
  where
    newC = c >>= transform

    transform (TInstr (MOV src@(Indexed _ _ :@ _) dst@(Indexed _ _ :@ _) :@ p1) [t1, t2] :@ p2) =
      let r5 = Reg (R5 :@ p1) :@ p1
      in [ TInstr (MOV src r5 :@ p1) [t1, Register 64 :@ p1] :@ p2
         , TInstr (MOV r5 dst :@ p1) [Register 64 :@ p1, t2] :@ p2 ]
    transform i = pure i
