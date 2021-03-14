module Language.NStar.CodeGen.PreProcessor.X64
( preprocessX64
) where

import Language.NStar.Syntax.Core
import Language.NStar.Typechecker.Core
import Data.Located (Located((:@)))

preprocessX64 :: TypedProgram -> TypedProgram
preprocessX64 = id
