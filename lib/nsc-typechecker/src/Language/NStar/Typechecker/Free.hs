{-# LANGUAGE FlexibleInstances #-}

{-|
  Module: Language.NStar.Typechecker.Free
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Typechecker.Free where

import Data.Set (Set)
import qualified Data.Set as Set (singleton, (\\))
import qualified Data.Map as Map (foldr)
import Data.Text (Text)
import Data.Foldable (fold)
import Data.Bifunctor (bimap)
import Language.NStar.Typechecker.Core
import Data.Located (Located, unLoc)

-- | The class of types whose free variables can be extracted from.
class Free t where
  -- | Free variable extractor.
  freeVars :: t -> Set (Located Text)

instance Free t => Free [t] where
  freeVars = fold . fmap freeVars

instance (Free a, Free b) => Free (a, b) where
  freeVars = uncurry (<>) . bimap freeVars freeVars

instance Free (Located Type) where
  freeVars = freeVars . unLoc

instance Free Type where
  freeVars (Cons t1 t2)      = freeVars t1 <> freeVars t2
  freeVars (FVar v)          = Set.singleton v
  freeVars (Ptr t1)          = freeVars t1
  freeVars (SPtr t1)         = freeVars t1
  freeVars (ForAll binds ty) = freeVars ty Set.\\ freeVars (fst <$> binds)
  freeVars (Record fields)   = Map.foldr ((<>) . freeVars) mempty fields
  freeVars (Var _)           = mempty
  freeVars (Signed _)        = mempty
  freeVars (Unsigned _)      = mempty
  freeVars (Register _)      = mempty
  -- Please refrain yourself from putting the three above cases under the same pattern "_".
  -- Those three are separated in order to keep GHC's warning about incomplete pattern matchings.
  --
  -- Yes, it's because I forget to add new types in there everytime.


-- No -XPolyKinds in N*, so no need to make an instance for @Free Kind@.
