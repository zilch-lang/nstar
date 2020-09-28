{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-|
  Module: Language.NStar.Typechecker.Pretty
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}


module Language.NStar.Typechecker.Pretty where

import Text.Diagnose (PrettyText(..))
import Text.PrettyPrint.ANSI.Leijen (text, (<+>), encloseSep, lbrace, rbrace, comma, colon, dot, hsep)
import Language.NStar.Typechecker.Core
import qualified Data.Text as Text
import Data.Located (unLoc, Located)
import qualified Data.Map as Map

instance PrettyText Kind where
  prettyText T8 = text "T8"
  prettyText Ta = text "Ta"
  prettyText Ts = text "Ts"

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
    where output (var, kind) = prettyText var <+> colon <+> prettyText kind

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
