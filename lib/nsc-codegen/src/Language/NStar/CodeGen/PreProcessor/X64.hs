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

    transform (TInstr (MV src@(IndexedE _ _ :@ _) dst@(IndexedE _ _ :@ _) :@ p1) chi sigma epsilon :@ p2) =
      let r5 = RegE (R5 :@ p1) :@ p1
      in [ TInstr (MV src r5 :@ p1) chi sigma epsilon :@ p2
         , TInstr (MV r5 dst :@ p1) chi sigma epsilon :@ p2 ]
    transform i = pure i
