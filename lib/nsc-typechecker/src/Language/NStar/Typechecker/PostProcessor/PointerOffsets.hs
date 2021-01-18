{-# LANGUAGE MultiWayIf #-}

module Language.NStar.Typechecker.PostProcessor.PointerOffsets
( desugarPointerOffsets ) where

import Language.NStar.Typechecker.Core (TypedProgram(..), TypedCodeSection(..), TypedStatement(..), Instruction(..), Type(..))
import Language.NStar.Syntax.Core (Expr(..), Register(..), Immediate(..))
import Data.Located (Located((:@)), unLoc)

desugarPointerOffsets :: TypedProgram -> TypedProgram
desugarPointerOffsets (TProgram dataSect rodataSect udataSect (TCode code :@ p)) =
  TProgram dataSect rodataSect udataSect (TCode newCode :@ p)
  where
    newCode = code >>= transformPointerOffsetIfNeeded

transformPointerOffsetIfNeeded :: Located TypedStatement -> [Located TypedStatement]
transformPointerOffsetIfNeeded i@(TInstr (MOV (Indexed off e1 :@ p1) e2 :@ p2) [t1, t2] :@ p3) =
  case (unLoc off, unLoc e1) of
    (Reg r, e3) ->
      let r0 = Reg (R0 :@ p3) :@ p3
          zero = Imm (I 0 :@ p3) :@ p3
      in case e3 of
        Reg (R0 :@ _) -> [ TInstr (ADD off e1 :@ p3) [Register 64 :@ p3, Register 64 :@ p3] :@ p3
                         , TInstr (MOV (Indexed zero e1 :@ p3) e2 :@ p3) [t1, t2] :@ p3
                         ] <> if | Reg (R0 :@ _) <- unLoc e2 -> []
                                 | otherwise                 -> [ TInstr (SUB off e1 :@ p3) [Register 64 :@ p3, Register 64 :@ p3] :@ p3 ]
        _ -> [ TInstr (PUSH r0 :@ p3) [Register 64 :@ p3] :@ p3
             , TInstr (MOV e1 r0 :@ p3) [t2, Register 64 :@ p3] :@ p3
             , TInstr (ADD off r0 :@ p3) [Register 64 :@ p3, Register 64 :@ p3] :@ p3
             , TInstr (MOV (Indexed zero r0 :@ p3) e2 :@ p3) [t1, t2] :@ p3
             , TInstr (POP r0 :@ p3) [Register 64 :@ p3] :@ p3
             ]
    (Imm _, Name _) -> [i]
    _ -> [i] -- TODO: handle all `mov` cases
transformPointerOffsetIfNeeded (TInstr i ts :@ p) = [TInstr i ts :@ p]
transformPointerOffsetIfNeeded (TLabel name :@ p) = [TLabel name :@ p]
