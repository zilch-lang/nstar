module Language.NStar.CodeGen.PreProcessor.X64
( preprocessX64
) where

import Language.NStar.Typechecker.Core

preprocessX64 :: TypedProgram -> TypedProgram
preprocessX64 = id
