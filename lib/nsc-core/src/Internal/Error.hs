{-# LANGUAGE ExplicitForAll #-}

module Internal.Error where

import GHC.Stack (HasCallStack)

internalError :: forall a. HasCallStack => String -> a
internalError msg = error $ msg <> "\nThis is an internal error. Please report this at <https://github.com/zilch-lang/nsc/issues>."
