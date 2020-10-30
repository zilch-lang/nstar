module Language.NStar.CodeGen.Errors where

import Language.NStar.Typechecker.Core (Instruction, Type)
import Text.Diagnose (Report, reportError, hint, Marker(..), prettyText)

data CodegenError

fromCodegenWarning :: CodegenError -> Report String
fromCodegenWarning _ = undefined
