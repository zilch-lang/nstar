{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

{-|
  Module: Language.NStar.Typechecker.Env
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Typechecker.Env
( Env
, union
, insert
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Located (Located)

-- | The type of environments associating variables to @t@s.
newtype Env t = Env (Map (Located Text) (Located t))
  deriving (Semigroup, Monoid, Functor)

union :: Env t -> Env t -> Env t
union (Env m1) (Env m2) = Env (Map.union m1 m2)

insert :: Located Text -> Located t -> Env t -> Env t
insert k v (Env m) = Env (Map.insert k v m)
