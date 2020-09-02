{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
  Module: Language.NStar.Syntax.Hints
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Syntax.Hints where

import Text.Diagnose (Hint)
import Data.Void (Void)

-- | Class of error types able to carry hints.
class Hintable e m where
  -- | Possibly empty list of hints carried by the error.
  hints :: e -> [Hint m]

instance Hintable Void m where
  hints _ = mempty
