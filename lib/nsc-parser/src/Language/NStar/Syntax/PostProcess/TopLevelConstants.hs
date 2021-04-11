module Language.NStar.Syntax.PostProcess.TopLevelConstants where

import Language.NStar.Syntax.Core (Constant(..), Binding(..), Section(DataS), Program(..))
import Data.Located

desugarTopLevelConstants :: Program -> Program
desugarTopLevelConstants (Program [DataS dat :@ p, rodat, udat, code, ext]) = Program [newDat, rodat, udat, code, ext]
  where newDat = DataS (fmap removeTopLevelConstants dat) :@ p

        removeTopLevelConstants :: Located Binding -> Located Binding
        removeTopLevelConstants (Bind l t c :@ p) = Bind l t (transformConstantIfNeeded c) :@ p

        transformConstantIfNeeded :: Located Constant -> Located Constant
        transformConstantIfNeeded c@(IntegerC _ :@ p) = ArrayC [c] :@ p
        transformConstantIfNeeded c@(CharacterC _ :@ p) = ArrayC [c] :@ p
        transformConstantIfNeeded c = c
