module Language.NStar.CodeGen.Errors where

import Text.Diagnose (Report)

data CodegenError

fromCodegenWarning :: CodegenError -> Report String
fromCodegenWarning _ = undefined
