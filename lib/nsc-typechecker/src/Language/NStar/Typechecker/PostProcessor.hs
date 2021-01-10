module Language.NStar.Typechecker.PostProcessor
( postProcessTypedAST ) where

import Language.NStar.Typechecker.Core (TypedProgram)
import Language.NStar.Typechecker.PostProcessor.PointerOffsets (desugarPointerOffsets)

postProcessTypedAST :: TypedProgram -> TypedProgram
postProcessTypedAST = desugarPointerOffsets
