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
import Data.Located (unLoc)
import qualified Data.Text as Text
import Language.NStar.Syntax.Pretty()

instance PrettyText TypedProgram where
  prettyText (TProgram stts) = vsep (fmap prettyText stts)

instance PrettyText TypedStatement where
  prettyText (TLabel l)    = text (Text.unpack (unLoc l)) <> colon
  prettyText (TInstr i ts) = prettyText i <+> hsep (fmap pprint ts)
                       --         ^^^ waiting to implement `PrettyText` for `Instruction`
    where pprint ty = text "@" <> prettyText ty
