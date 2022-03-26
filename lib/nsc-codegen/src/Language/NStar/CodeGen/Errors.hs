module Language.NStar.CodeGen.Errors where

import Error.Diagnose (Report)

data CodegenError

fromCodegenWarning :: CodegenError -> Report String
fromCodegenWarning _ = undefined
