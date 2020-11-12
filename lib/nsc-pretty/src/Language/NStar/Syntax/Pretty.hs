{-|
  Module: Language.NStar.Syntax.Pretty
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Syntax.Pretty where

import Text.Diagnose (PrettyText(..))
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Language.NStar.Syntax.Core
import qualified Data.Text as Text
import Data.Located (unLoc, Located)
import qualified Data.Map as Map

instance PrettyText Program where
  prettyText (Program stts) =
    vsep (fmap prettyText stts)

instance PrettyText Statement where
  prettyText (Label name ty) = prettyText name <> colon <+> prettyText ty
  prettyText (Instr i)       = prettyText i

instance PrettyText Kind where
  prettyText T8 = text "T8"
  prettyText Ta = text "Ta"
  prettyText Ts = text "Ts"

instance PrettyText Text.Text where
  prettyText t = text (Text.unpack t)

instance PrettyText t => PrettyText (Located t) where
  prettyText = prettyText . unLoc

instance PrettyText Type where
  prettyText (Var v) = text (Text.unpack (unLoc v))
  prettyText (FVar v) = text (Text.unpack (unLoc v))
  prettyText (Register n) = text "r" <> text (show n)
  prettyText (Signed n) = text "s" <> text (show n)
  prettyText (Unsigned n) = text "u" <> text (show n)
  prettyText (Cons t1 t2) = prettyText t1 <> colon <> colon <> prettyText t2
  prettyText (Ptr t) = text "*" <> prettyText t
  prettyText (SPtr t) = text "sptr" <+> prettyText t
  prettyText (Record maps) = encloseSep lbrace rbrace comma (Map.foldlWithKey f [] maps)
    where f list reg ty = (prettyText reg <+> colon <+> prettyText ty) : list
  prettyText (ForAll binds ty) = text "forall" <+> hsep (output <$> binds) <> dot <+> prettyText ty
    where output (var, kind) = parens $ prettyText var <+> colon <+> prettyText kind

instance PrettyText Register where
  prettyText = (text "%" <>) . text . f
    where
      f RAX = "rax"
      f RBX = "rbx"
      f RCX = "rcx"
      f RDX = "rdx"
      f RSI = "rsi"
      f RDI = "rdi"
      f RBP = "rbp"
      f RSP = "rsp"

instance PrettyText Instruction where
  prettyText (MOV s d)     = text "mov" <+> prettyText s <> comma <+> prettyText d
  prettyText (RET)         = text "ret"
  prettyText (JMP lbl tys) = text "jmp" <+> prettyText lbl <> encloseSep langle rangle comma (fmap prettyText tys)

instance PrettyText Expr where
  prettyText (Imm i) = prettyText i
  prettyText (Name n) = text (Text.unpack (unLoc n))
  prettyText (Indexed idx e) = integer (unLoc idx) <> parens (prettyText e)
  prettyText (Reg r) = prettyText r
  prettyText (Spec e ty) = prettyText e <> angles (prettyText ty)

instance PrettyText Immediate where
  prettyText (I i) = integer i
  prettyText (C c) = char c
