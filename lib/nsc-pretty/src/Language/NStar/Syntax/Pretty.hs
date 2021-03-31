{-|
  Module: Language.NStar.Syntax.Pretty
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Syntax.Pretty where

import Text.Diagnose (PrettyText(..))
import Text.PrettyPrint.ANSI.Leijen
import Language.NStar.Syntax.Core
import qualified Data.Text as Text
import Data.Located (unLoc, Located((:@)))
import qualified Data.Map as Map
import Prelude hiding ((<$>))

instance PrettyText Program where
  prettyText (Program stts) = vsep (fmap prettyText stts)

instance PrettyText Section where
  prettyText (CodeS sect)    = text "section code {" <$> indent 4 (vsep (fmap prettyText sect)) <$> text "}"
  prettyText (DataS sect)    = text "section data {" <$> indent 4 (vsep (fmap prettyText sect)) <$> text "}"
  prettyText (RODataS sect)  = text "section rodata {" <$> indent 4 (vsep (fmap prettyText sect)) <$> text "}"
  prettyText (UDataS sect)   = text "section udata {" <$> indent 4 (vsep (fmap prettyText sect)) <$> text "}"
  prettyText (IncludeS sec)  = text "include {" <$> indent 4 (vsep (fmap prettyText sec)) <$> text "}"
  prettyText (ExternCodeS s) = text "section extern.code {" <$> indent 4 (vsep (fmap prettyText s)) <$> text "}"

instance PrettyText Binding where
  prettyText (Bind name ty cst) = prettyText name <> colon <+> prettyText ty <+> equals <+> prettyText cst

instance PrettyText ReservedSpace where
  prettyText (ReservedBind name ty) = prettyText name <> colon <+> prettyText ty

instance PrettyText Statement where
  prettyText (Label name ty block) = prettyText name <+> align (colon <+> prettyText ty <$> equals <+> prettyBlock block)
    where prettyBlock is = mconcat (punctuate (line <> semi <> space) (fmap pprint is))

          pprint (i, unsafe) = (if unsafe then text "unsafe " else empty) <> prettyText i

instance PrettyText Kind where
  prettyText T8 = text "T8"
  prettyText Ta = text "Ta"
  prettyText Ts = text "Ts"
  prettyText Tc = text "Tc"

instance PrettyText Text.Text where
  prettyText t = text (Text.unpack t)

instance PrettyText t => PrettyText (Located t) where
  prettyText = prettyText . unLoc

instance PrettyText Type where
  prettyText (VarT v) = text (Text.unpack (unLoc v))
  prettyText (FVarT v) = text (Text.unpack (unLoc v))
  prettyText (RegisterT n) = text "r" <> text (show n)
  prettyText (SignedT n) = text "s" <> text (show n)
  prettyText (UnsignedT n) = text "u" <> text (show n)
  prettyText (ConsT t1 t2) = prettyText t1 <> text "∷" <> prettyText t2
  prettyText (PtrT t) = text "*" <> prettyText t
  prettyText (RecordT maps st cont open) =
    lbrace <+> mconcat (punctuate comma (Map.foldlWithKey f [] maps)) <+> text "|" <+> prettyText st <+> text "→" <+> prettyText cont <+> rbrace
    where f list reg ty = (prettyText reg <+> colon <+> prettyText ty) : list
  prettyText (ForAllT binds ty) = text "∀" <> parens (mconcat (punctuate comma $ fmap output binds)) <> dot <> prettyText ty
    where output (var, kind) = prettyText var <+> colon <+> prettyText kind
  prettyText (RegisterContT r) = prettyText r
  prettyText (StackContT i) = prettyText i
  prettyText BangT = text "!"

instance PrettyText Register where
  prettyText = (text "%" <>) . text . f
    where
      f R0 = "r0"
      f R1 = "r1"
      f R2 = "r2"
      f R3 = "r3"
      f R4 = "r4"
      f R5 = "r5"

instance PrettyText Instruction where
  prettyText (MV s d)       = text "mv" <+> prettyText s <> comma <+> prettyText d
  prettyText (RET)          = text "ret"
  prettyText (JMP lbl)      = text "jmp" <+> prettyText lbl
  prettyText (CALL lbl)     = text "call" <+> prettyText lbl
  prettyText (ADD inc dst)  = text "add" <+> prettyText inc <> comma <+> prettyText dst
  prettyText (SUB inc dst)  = text "sub" <+> prettyText inc <> comma <+> prettyText dst
  prettyText (NOP)          = text "nop"
  prettyText (SALLOC t)     = text "salloc" <+> prettyText t
  prettyText (SFREE)        = text "sfree"
  prettyText (SLD s d)      = text "sld" <+> prettyText s <> comma <+> prettyText d
  prettyText (SST s d)      = text "sst" <+> prettyText s <> comma <+> prettyText d
  prettyText (LD s d)       = text "ld" <+> prettyText s <> comma <+> prettyText d
  prettyText (ST s d)       = text "st" <+> prettyText s <> comma <+> prettyText d
  prettyText (SREF n r)     = text "sref" <+> prettyText n <> comma <+> prettyText r

instance PrettyText Constant where
  prettyText (IntegerC (i :@ _))   = integer i
  prettyText (CharacterC (c :@ _)) = squotes (char c)
  prettyText (ArrayC csts)         = lbracket <> hsep (fmap prettyText csts) <+> rbracket

instance PrettyText Expr where
  prettyText (ImmE i) = prettyText i
  prettyText (NameE n tys) = text (Text.unpack (unLoc n)) <> encloseSep langle rangle comma (fmap prettyText tys)
  prettyText (RegE r) = prettyText r
  prettyText (BaseOffsetE s o) = prettyText s <> brackets (prettyText o)
  prettyText (ByteOffsetE o s) = prettyText o <> parens (prettyText s)

instance PrettyText Immediate where
  prettyText (I i) = integer i
  prettyText (C c) = squotes (char c)
