{-|
  Module: Language.NStar.Typechecker.Pretty
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Typechecker.Pretty where

import Text.Diagnose (PrettyText(..))
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Language.NStar.Typechecker.Core
import Data.Located (unLoc, Located((:@)))
import qualified Data.Text as Text
import Language.NStar.Syntax.Pretty()
import qualified Data.Map as Map
import Data.List (intersperse)

instance PrettyText TypedProgram where
  prettyText (TProgram (dataSect :@ _) (rodataSect :@ _) (udataSect :@ _) (codeSect :@ _)) =
    prettyText dataSect <> line <>
    -- prettyText rodataSect <> line <>
    -- prettyText udataSect <> line <>
    prettyText codeSect

instance PrettyText TypedDataSection where
  prettyText (TData d) = text "section data {" <> line <> indent 4 (vsep (fmap prettyText d)) <> line <> text "}"

instance PrettyText TypedCodeSection where
  prettyText (TCode is) = text "section code {" <> line <> indent 4 (vsep (fmap prettyText is)) <> line <> text "}"

instance PrettyText TypedStatement where
  prettyText (TLabel l is)            = text (Text.unpack (unLoc l)) <> colon <> line <> indent 4 (vsep (fmap prettyText is))
  prettyText (TInstr i chi sigma eps) = pprint chi <> semi <+> prettyText sigma <> semi <+> prettyText eps <+> text "⊢" <+> prettyText i
    where pprint c =
            if null c
            then text "·"
            else mconcat . intersperse comma . fmap toBind $ Map.toList c
          toBind (r, t) = prettyText r <+> colon <+> prettyText t

instance PrettyText TypedInstruction where
  prettyText (MV s d)   = text "mv" <+> prettyText s <> comma <+> prettyText d
  prettyText (JMP l)    = text "jmp" <+> prettyText l
  prettyText (NOP)      = text "nop"
  prettyText (SALLOC n) = text "salloc" <+> prettyText n
  prettyText (SFREE n)  = text "sfree" <+> prettyText n
  prettyText (SLD n r)  = text "sld" <+> prettyText n <> comma <+> int 8 <> comma <+> prettyText r
  prettyText (SST v n)  = text "sst" <+> prettyText v <> comma <+> prettyText n
  prettyText (LD o p r) = text "ld" <+> prettyText o <> comma <+> prettyText p <> comma <+> int 8 <> comma <+> prettyText r
  prettyText (ST r o p) = text "st" <+> prettyText r <> comma <+> prettyText o <> comma <+> prettyText p
