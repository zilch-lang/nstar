{-# LANGUAGE TypeApplications #-}

{-|
  Module: Language.NStar.Typechecker.Pretty
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Typechecker.Pretty where

import Prettyprinter
import Language.NStar.Typechecker.Core
import Data.Located (unLoc, Located((:@)))
import Language.NStar.Syntax.Pretty()
import qualified Data.Map as Map
import Data.List (intersperse)

instance Pretty TypedProgram where
  pretty (TProgram (dataSect :@ _) (rodataSect :@ _) (udataSect :@ _) (codeSect :@ _) (ecodeSect :@ _)) =
    pretty dataSect <> hardline <>
    -- pretty rodataSect <> line <>
    -- pretty udataSect <> line <>
    pretty codeSect <> hardline <>
    pretty ecodeSect

instance Pretty TypedDataSection where
  pretty (TData d) = "section data {" <> line <> indent 4 (vsep (fmap pretty d)) <> line <> "}"

instance Pretty TypedCodeSection where
  pretty (TCode is) = "section code {" <> line <> indent 4 (vsep (fmap pretty is)) <> line <> "}"

instance Pretty TypedExternCodeSection where
  pretty (TExternCode bs) = "section extern.code {" <> line <> indent 4 (vsep (fmap pretty bs)) <> line <> "}"

instance Pretty TypedStatement where
  pretty (TLabel l is)            = pretty (unLoc l) <> colon <> line <> indent 4 (vsep (fmap pretty is))
  pretty (TInstr i chi sigma eps) = pprint chi <> semi <+> pretty sigma <> semi <+> pretty eps <+> "⊢" <+> pretty i
    where pprint c =
            if null c
            then "·"
            else mconcat . intersperse comma . fmap toBind $ Map.toList c
          toBind (r, t) = pretty r <+> colon <+> pretty t

instance Pretty TypedInstruction where
  pretty (MV s d)     = "mv" <+> pretty s <> comma <+> pretty d
  pretty (JMP l)      = "jmp" <+> pretty l
  pretty (NOP)        = "nop"
  pretty (SALLOC n)   = "salloc" <+> pretty n
  pretty (SFREE n)    = "sfree" <+> pretty n
  pretty (SLD n r)    = "sld" <+> pretty n <> comma <+> pretty @Integer 8 <> comma <+> pretty r
  pretty (SST v n)    = "sst" <+> pretty v <> comma <+> pretty n
  pretty (LD o p r)   = "ld" <+> pretty o <> comma <+> pretty p <> comma <+> pretty @Integer 8 <> comma <+> pretty r
  pretty (ST r o p)   = "st" <+> pretty r <> comma <+> pretty o <> comma <+> pretty p
  pretty (SREF n p r) = "sref" <+> pretty n <> comma <+> pretty p <> comma <+> pretty r
