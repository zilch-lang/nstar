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
  prettyText (TData d) = text "section data {" <> line <> indent 4 (vsep (fmap prettyText d)) <> line <> text "}"

instance PrettyText TypedCodeSection where
  prettyText (TCode is) = text "section code {" <> line <> indent 4 (vsep (fmap prettyText is)) <> line <> text "}"

instance PrettyText TypedStatement where
  prettyText (TLabel l is) = text (Text.unpack (unLoc l)) <> colon <> line <> indent 4 (vsep (fmap prettyText is))
  prettyText (TInstr i)    = text "⊢" <+> prettyText i

instance PrettyText TypedInstruction where
  prettyText (RET r)    = text "ret " <+> prettyText r
  prettyText (MV s d)   = text "mv" <+> prettyText s <> comma <+> prettyText d
  prettyText (JMP l)    = text "jmp" <+> prettyText l
  prettyText (CALL l)   = text "call" <+> prettyText l
  prettyText (NOP)      = text "nop"
  prettyText (SALLOC n) = text "salloc" <+> prettyText n
