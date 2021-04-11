module Language.NStar.Syntax.PostProcessor where

import Language.NStar.Syntax.Core (Program)
import Language.NStar.Syntax.PostProcess.ConcatenateSections (concatSections)
import Language.NStar.Syntax.PostProcess.TopLevelConstants (desugarTopLevelConstants)

postProcessAST :: Program -> Program
postProcessAST = {- expandMacros . -} desugarTopLevelConstants . concatSections {- . resolveTargets -}
