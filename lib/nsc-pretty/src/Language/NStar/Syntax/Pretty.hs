{-|
  Module: Language.NStar.Syntax.Pretty
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Syntax.Pretty where

import Prettyprinter
import Language.NStar.Syntax.Core
import qualified Data.Text as Text
import Data.Located (unLoc, Located((:@)))
import qualified Data.Map as Map
import Prelude hiding ((<$>))

instance Pretty Program where
  pretty (Program stts) = vsep (fmap pretty stts)

instance Pretty Section where
  pretty (CodeS sect)    = "section code {" <> line <> indent 4 (vsep (fmap pretty sect)) <> line <> "}"
  pretty (DataS sect)    = "section data {" <> line <> indent 4 (vsep (fmap pretty sect)) <> line <> "}"
  pretty (RODataS sect)  = "section rodata {" <> line <> indent 4 (vsep (fmap pretty sect)) <> line <> "}"
  pretty (UDataS sect)   = "section udata {" <> line <> indent 4 (vsep (fmap pretty sect)) <> line <> "}"
  pretty (IncludeS sec)  = "include {" <> line <> indent 4 (vsep (fmap pretty sec)) <> line <> "}"
  pretty (ExternCodeS s) = "section extern.code {" <> line <> indent 4 (vsep (fmap pretty s)) <> line <> "}"

instance Pretty Binding where
  pretty (Bind name ty cst) = pretty name <> colon <+> pretty ty <+> equals <+> pretty cst

instance Pretty ReservedSpace where
  pretty (ReservedBind name ty) = pretty name <> colon <+> pretty ty

instance Pretty Statement where
  pretty (Label name ty block) = pretty name <+> align (colon <+> pretty ty <> line <> equals <+> prettyBlock block)
    where prettyBlock is = mconcat (punctuate (line <> semi <> space) (fmap pprint is))

          pprint (i, unsafe) = (if unsafe then "unsafe " else mempty) <> pretty i

instance Pretty Kind where
  pretty (T n) = "T" <> pretty n
  pretty Ta = "Ta"
  pretty Ts = "Ts"
  pretty Tc = "Tc"

instance Pretty t => Pretty (Located t) where
  pretty = pretty . unLoc

instance Pretty Type where
  pretty (VarT v) = pretty (unLoc v)
  pretty (FVarT v) = pretty (unLoc v)
  pretty (RegisterT n) = "r" <> viaShow n
  pretty (SignedT n) = "s" <> viaShow n
  pretty (UnsignedT n) = "u" <> viaShow n
  pretty (ConsT t1 t2) = pretty t1 <> "∷" <> pretty t2
  pretty (PtrT t) = "*" <> pretty t
  pretty (RecordT maps st cont open) =
    lbrace <+> mconcat (punctuate comma (Map.foldlWithKey f [] maps)) <+> "|" <+> pretty st <+> "→" <+> pretty cont <+> rbrace
    where f list reg ty = (pretty reg <+> colon <+> pretty ty) : list
  pretty (ForAllT binds ty) = "∀" <> parens (mconcat (punctuate comma $ fmap output binds)) <> dot <> pretty ty
    where output (var, kind) = pretty var <+> colon <+> pretty kind
  pretty (RegisterContT r) = pretty r
  pretty (StackContT i) = pretty i
  pretty BangT = "!"
  pretty (PackedStructT ts) = encloseSep lparen rparen comma $ fmap pretty ts

instance Pretty Register where
  pretty = ("%" <>) . f
    where
      f R0 = "r0"
      f R1 = "r1"
      f R2 = "r2"
      f R3 = "r3"
      f R4 = "r4"
      f R5 = "r5"

instance Pretty Instruction where
  pretty (MV s d)       = "mv" <+> pretty s <> comma <+> pretty d
  pretty (RET)          = "ret"
  pretty (JMP lbl)      = "jmp" <+> pretty lbl
  pretty (CALL lbl)     = "call" <+> pretty lbl
  pretty (ADD inc dst)  = "add" <+> pretty inc <> comma <+> pretty dst
  pretty (SUB inc dst)  = "sub" <+> pretty inc <> comma <+> pretty dst
  pretty (NOP)          = "nop"
  pretty (SALLOC t)     = "salloc" <+> pretty t
  pretty (SFREE)        = "sfree"
  pretty (SLD s d)      = "sld" <+> pretty s <> comma <+> pretty d
  pretty (SST s d)      = "sst" <+> pretty s <> comma <+> pretty d
  pretty (LD s d)       = "ld" <+> pretty s <> comma <+> pretty d
  pretty (ST s d)       = "st" <+> pretty s <> comma <+> pretty d
  pretty (SREF n r)     = "sref" <+> pretty n <> comma <+> pretty r

instance Pretty Constant where
  pretty (IntegerC (i :@ _))   = pretty  i
  pretty (CharacterC (c :@ _)) = squotes (pretty c)
  pretty (ArrayC csts)         = lbracket <> hsep (fmap pretty csts) <+> rbracket
  pretty (StructC cs)          = tupled (fmap pretty cs)

instance Pretty Expr where
  pretty (ImmE i) = pretty i
  pretty (NameE n tys) = pretty (unLoc n) <> encloseSep langle rangle comma (fmap pretty tys)
  pretty (RegE r) = pretty r
  pretty (BaseOffsetE s o) = pretty s <> brackets (pretty o)
  pretty (ByteOffsetE o s) = pretty o <> parens (pretty s)

instance Pretty Immediate where
  pretty (I i) = pretty i
  pretty (C c) = squotes (pretty c)
