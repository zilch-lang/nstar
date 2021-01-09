module Language.NStar.Syntax.PostProcessor where

import Language.NStar.Syntax.Core (Program)
import Language.NStar.Syntax.PostProcess.ConcatenateSections (concatSections)

postProcessAST :: Program -> Program
postProcessAST = {- expandMacros . -} concatSections {- . resolveTargets -}
