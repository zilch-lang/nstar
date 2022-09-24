-- |
--  Module: Language.NStar.Syntax.Pretty
--  Copyright: (c) Mesabloo, 2020
--  License: BSD3
--  Stability: experimental
module Language.NStar.Syntax.Pretty where

import Data.Located (Located ((:@)), unLoc)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Language.NStar.Syntax.Core
import Prettyprinter
import Prelude hiding ((<$>))

instance Pretty Program where
  pretty (Program stts) = vsep (fmap pretty stts)

instance Pretty Section where
  pretty (CodeS sect) = "section code {" <> line <> indent 4 (vsep (fmap pretty sect)) <> line <> "}"
  pretty (DataS sect) = "section data {" <> line <> indent 4 (vsep (fmap pretty sect)) <> line <> "}"
  pretty (RODataS sect) = "section rodata {" <> line <> indent 4 (vsep (fmap pretty sect)) <> line <> "}"
  pretty (UDataS sect) = "section udata {" <> line <> indent 4 (vsep (fmap pretty sect)) <> line <> "}"
  pretty (IncludeS sec) = "include {" <> line <> indent 4 (vsep (fmap pretty sec)) <> line <> "}"
  pretty (ExternCodeS s) = "section extern.code {" <> line <> indent 4 (vsep (fmap pretty s)) <> line <> "}"

instance Pretty Binding where
  pretty (Bind name ty cst) = pretty name <> colon <+> pretty ty <+> equals <+> pretty cst

instance Pretty ReservedSpace where
  pretty (ReservedBind name ty) = pretty name <> colon <+> pretty ty

instance Pretty Statement where
  pretty (Label name ty block) = pretty name <+> align (colon <+> pretty ty <> line <> equals <+> prettyBlock block)
    where
      prettyBlock is = mconcat (punctuate (line <> semi <> space) (fmap pprint is))

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
    where
      f list reg ty = (pretty reg <+> colon <+> pretty ty) : list
  pretty (ForAllT binds ty) = "∀" <> parens (mconcat (punctuate comma $ fmap output binds)) <> dot <> pretty ty
    where
      output (var, kind) = pretty var <+> colon <+> pretty kind
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
  pretty (MV s d) = "mv" <+> pretty s <> comma <+> pretty d
  pretty (RET) = "ret"
  pretty (JMP lbl) = "jmp" <+> pretty lbl
  pretty (CALL lbl) = "call" <+> pretty lbl
  pretty (ADD inc dst r) = "add" <+> pretty inc <> comma <+> pretty dst <> comma <+> pretty r
  pretty (SUB inc dst r) = "sub" <+> pretty inc <> comma <+> pretty dst <> comma <+> pretty r
  pretty (NOP) = "nop"
  pretty (SALLOC t) = "salloc" <+> pretty t
  pretty (SFREE) = "sfree"
  pretty (SLD s d) = "sld" <+> pretty s <> comma <+> pretty d
  pretty (SST s d) = "sst" <+> pretty s <> comma <+> pretty d
  pretty (LD s d) = "ld" <+> pretty s <> comma <+> pretty d
  pretty (ST s d) = "st" <+> pretty s <> comma <+> pretty d
  pretty (SREF n r) = "sref" <+> pretty n <> comma <+> pretty r
  pretty (AND x y r) = "and" <+> pretty x <> comma <+> pretty y <> comma <+> pretty r
  pretty (OR x y r) = "or" <+> pretty x <> comma <+> pretty y <> comma <+> pretty r
  pretty (NOT x r) = "not" <+> pretty x <> comma <+> pretty r
  pretty (XOR x y r) = "xor" <+> pretty x <> comma <+> pretty y <> comma <+> pretty r
  pretty (CMVZ a b c r) = "cmvz" <+> pretty a <> comma <+> pretty b <> comma <+> pretty c <> comma <+> pretty r
  pretty (CMVNZ a b c r) = "cmvnz" <+> pretty a <> comma <+> pretty b <> comma <+> pretty c <> comma <+> pretty r
  pretty (SHIFTL x y r) = "shiftl" <+> pretty x <> comma <+> pretty y <> comma <+> pretty r
  pretty (SHIFTR x y r) = "shiftr" <+> pretty x <> comma <+> pretty y <> comma <+> pretty r
  pretty (MUL a b r) = "mul" <+> pretty a <> comma <+> pretty b <> comma <+> pretty r
  pretty (CMVL a b c d r) = "cmvl" <+> pretty a <> comma <+> pretty b <> comma <+> pretty c <> comma <+> pretty d <> comma <+> pretty r
  pretty (CMVGE a b c d r) = "cmvge" <+> pretty a <> comma <+> pretty b <> comma <+> pretty c <> comma <+> pretty d <> comma <+> pretty r
  pretty (CMVLE a b c d r) = "cmvle" <+> pretty a <> comma <+> pretty b <> comma <+> pretty c <> comma <+> pretty d <> comma <+> pretty r
  pretty (CMVG a b c d r) = "cmvg" <+> pretty a <> comma <+> pretty b <> comma <+> pretty c <> comma <+> pretty d <> comma <+> pretty r
  pretty (CMVE a b c d r) = "cmve" <+> pretty a <> comma <+> pretty b <> comma <+> pretty c <> comma <+> pretty d <> comma <+> pretty r
  pretty (CMVNE a b c d r) = "cmvne" <+> pretty a <> comma <+> pretty b <> comma <+> pretty c <> comma <+> pretty d <> comma <+> pretty r
  pretty (CJZ a l1 l2) = "cjz" <+> pretty a <> comma <+> pretty l1 <> comma <+> pretty l2
  pretty (CJNZ a l1 l2) = "cjnz" <+> pretty a <> comma <+> pretty l1 <> comma <+> pretty l2
  pretty (CJL a b l1 l2) = "cjl" <+> pretty a <> comma <+> pretty b <> comma <+> pretty l1 <> comma <+> pretty l2
  pretty (CJGE a b l1 l2) = "cjge" <+> pretty a <> comma <+> pretty b <> comma <+> pretty l1 <> comma <+> pretty l2
  pretty (CJLE a b l1 l2) = "cjle" <+> pretty a <> comma <+> pretty b <> comma <+> pretty l1 <> comma <+> pretty l2
  pretty (CJG a b l1 l2) = "cjg" <+> pretty a <> comma <+> pretty b <> comma <+> pretty l1 <> comma <+> pretty l2
  pretty (CJE a b l1 l2) = "cje" <+> pretty a <> comma <+> pretty b <> comma <+> pretty l1 <> comma <+> pretty l2
  pretty (CJNE a b l1 l2) = "cjne" <+> pretty a <> comma <+> pretty b <> comma <+> pretty l1 <> comma <+> pretty l2

instance Pretty Constant where
  pretty (IntegerC (i :@ _)) = pretty i
  pretty (CharacterC (c :@ _)) = squotes (pretty c)
  pretty (ArrayC csts) = lbracket <> hsep (fmap pretty csts) <+> rbracket
  pretty (StructC cs) = tupled (fmap pretty cs)

instance Pretty Expr where
  pretty (ImmE i) = pretty i
  pretty (NameE n tys) = pretty (unLoc n) <> encloseSep langle rangle comma (fmap pretty tys)
  pretty (RegE r) = pretty r
  pretty (BaseOffsetE s o) = pretty s <> brackets (pretty o)
  pretty (ByteOffsetE o s) = pretty o <> parens (pretty s)

instance Pretty Immediate where
  pretty (I i) = pretty i
  pretty (C c) = squotes (pretty c)
