{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-|
  Module: Language.NStar.Typechecker.Types
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Typechecker.Subst where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Located (Located(..), unLoc)
import Data.Text (Text)
import Language.NStar.Typechecker.Core
import Data.Maybe (fromMaybe)

-- | The type of substitutions, unifying (fresh) type variables with @t@s.
newtype Substitution t
  = Subst (Map (Located Text) (Located t))
 deriving (Functor)

instance (Substitutable t, Subst t ~ Substitution t) => Monoid (Substitution t) where
  mempty = Subst mempty

instance (Substitutable t, Subst t ~ Substitution t) => Semigroup (Substitution t) where
  s1@(Subst m1) <> Subst m2 = Subst $ (apply s1 <$> m2) `Map.union` m1

-- | The class of substitutable types, types which can be modified by 'apply'ing a 'Substitution' on them.
class Substitutable a where
  type Subst (a :: *) :: *
  type Subst a = Substitution a

  -- | Applies the 'Substitution' given to the specific value, generating a new value of type @a@.
  apply :: Subst a -> a -> a

instance Substitutable a => Substitutable [a] where
  type Subst [a] = Subst a

  apply = fmap . apply

instance Substitutable a => Substitutable (Located a) where
  type Subst (Located a) = Subst a

  apply = fmap . apply

instance Substitutable Type where
  apply _ t@(Signed _)              = t
  apply _ t@(Unsigned _)            = t
  apply _ t@(Var _)                 = t
  apply s (Cons t1 t2)              = Cons (apply s t1) (apply s t2)
  apply (Subst s) t@(FVar v)        = fromMaybe t (unLoc <$> Map.lookup v s)
  apply s (Record rts)              = Record (apply s <$> rts)
  apply s (Ptr t)                   = Ptr (apply s t)
  apply s (SPtr t)                  = SPtr (apply s t)
  apply (Subst s) (ForAll binds ty) = ForAll binds (apply newS ty)
    where newS = Subst (Map.withoutKeys s (Set.fromList (keys binds)))

          keys []                     = []
          keys ((Var v :@ _, _) : xs) = v : keys xs
          keys ((t :@ _, _) : _)      = error ("Trying to get name of non-type variable '" <> show t <> "'")
