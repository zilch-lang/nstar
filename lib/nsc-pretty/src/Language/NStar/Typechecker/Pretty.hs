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

instance PrettyText TypedProgram where
  prettyText (TProgram (dataSect :@ _) (rodataSect :@ _) (udataSect :@ _) (codeSect :@ _)) =
    prettyText dataSect <> line <>
    -- prettyText rodataSect <> line <>
    -- prettyText udataSect <> line <>
    prettyText codeSect

instance PrettyText TypedDataSection where
  prettyText (TData d) = text "section data {" <> line <> vsep (fmap prettyText d) <> line <> text "}"

instance PrettyText TypedCodeSection where
  prettyText (TCode is) = text "section code {" <> line <> vsep (fmap prettyText is) <> line <> text "}"

instance PrettyText TypedStatement where
  prettyText (TLabel l)               = text (Text.unpack (unLoc l)) <> colon
  prettyText (TInstr i chi sigma eps) =
    pprint chi <> semi <+> prettyText sigma <> semi <+> prettyText eps <+> text "⊢" <+> prettyText i
    where
      pprint = encloseSep empty empty comma . fmap toBind . Map.toList
      toBind (r, t) = prettyText r <+> colon <+> prettyText t
