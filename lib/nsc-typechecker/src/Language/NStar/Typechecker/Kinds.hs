{-# LANGUAGE TypeFamilies #-}

{-|
  Module: Language.NStar.Typechecker.Kinds
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Typechecker.Kinds (kindcheck, runKindchecker, unifyKinds, kindcheckType, requireSized, sizeof) where

{-

    Because there is close to nothing about kind inference in N*,
    there probably won't be anything in this module.

    One thing that belongs here is the kind checking.
    So:
    - `a::s` needs `a: T8|Ta` and `s: Ts` and returns `Ts`
    - `{%r0: a}` needs `a <: T8`
    - `*a` needs `a: T8|Ta` and returns `T8` (on x64)

-}

import Control.Monad.Except
import Data.Located
import Language.NStar.Typechecker.Core
import Text.Diagnose (Report)
import Data.Bifunctor (first, second)
import Data.Text (Text)
import qualified Data.Map as Map
import Control.Applicative (liftA2)
import Language.NStar.Typechecker.Errors
import Console.NStar.Flags (TypecheckerFlags(..))
import Language.NStar.Typechecker.Subst
import Language.NStar.Typechecker.Env (Env)
import qualified Language.NStar.Typechecker.Env as Env
import Control.Monad.State (evalStateT, StateT)

type Kindchecker a = StateT Int (Except KindcheckError) a

data KindcheckError
  = NotAStackType (Located Kind) Position
  | NotADataType (Located Kind) Position
  | NotSized (Located Kind) Position
  | UnboundTypeVariable (Located Text)
  | CannotUnifyKinds (Located Kind) (Located Kind)

--------------------------------------------------------------------------------------------

runKindchecker :: (?tcFlags :: TypecheckerFlags) => Kindchecker a -> Either (Report String) a
runKindchecker = first fromKindcheckError . runExcept . flip evalStateT 0

kindcheck :: (?tcFlags :: TypecheckerFlags) => Located Type -> Either (Report String) ()
kindcheck = second (const ()) . runKindchecker . kindcheckType mempty

fromKindcheckError :: KindcheckError -> Report String
fromKindcheckError (NotAStackType (k :@ p1) p2)             = kindIsNotAStackKind k p1 p2
fromKindcheckError (NotADataType (k :@ p1) p2)              = kindIsNotADataKind k p1 p2
fromKindcheckError (NotSized (k :@ p1) p2)                  = kindIsUnsized k p1 p2
fromKindcheckError (UnboundTypeVariable (v :@ p))           = unboundTypeVariable v p
fromKindcheckError (CannotUnifyKinds (k1 :@ p1) (k2 :@ p2)) = cannotUnifyKinds k1 k2 p1 p2

kindcheckType :: (?tcFlags :: TypecheckerFlags) => Env Kind -> Located Type -> Kindchecker (Located Kind)
kindcheckType _ (SignedT _ :@ p)                                      = pure (T 8 :@ p)
kindcheckType _ (UnsignedT _ :@ p)                                    = pure (T 8 :@ p)
    -- TODO: For now we ignore the size of both types, because there are no sized kinds other than @T8@.
kindcheckType ctx (ForAllT newCtx ty :@ p)                            = (T 8 :@ p) <$ kindcheckType (Env.fromList (first varName <$> newCtx) <> ctx) ty
  where varName (VarT v :@ _) = v
        varName (t :@ _)     = error $ "Cannot fetch name of non type-variable type '" <> show t <> "'."
kindcheckType ctx (VarT v :@ _)                                       = maybe (throwError (UnboundTypeVariable v)) pure (Env.lookup v ctx)
kindcheckType _ (FVarT v :@ _)                                        = throwError (UnboundTypeVariable v)
kindcheckType ctx (PtrT t :@ p)                                       = (T 8 :@ p) <$ kindcheckType ctx t
--kindcheckType ctx (SPtr t@(_ :@ p1) :@ p)                = (T8 :@ p) <$ (requireStackType p1 =<< kindcheckType ctx t)
kindcheckType ctx (ConsT t1@(_ :@ p1) t2@(_ :@ p2) :@ p)              = do
  requireSized p1 =<< kindcheckType ctx t1
  requireStackType p2 =<< kindcheckType ctx t2
  pure (Ts :@ p)
kindcheckType ctx (RecordT mappings st@(_ :@ p2) e@(_ :@ p3) _ :@ p)  = do
  Map.traverseWithKey handleTypeFromRegister mappings
  requireStackType p2 =<< kindcheckType ctx st
  requireCont p3 =<< kindcheckType ctx e

  pure (Ta :@ p)
       -- FIXME: Kind checking does not take into account the size of the types, so if they are sized, they all are 8-bytes big at the moment.
 where handleTypeFromRegister _ ty@(_ :@ p)              = liftA2 (*>) (requireDataType p) (requireSized p) =<< kindcheckType ctx ty
kindcheckType _ (RegisterContT _ :@ p)                                = pure (Tc :@ p)
kindcheckType _ (StackContT _ :@ p)                                   = pure (Tc :@ p)
kindcheckType _ (RegisterT _ :@ p)                                    = pure (T 8 :@ p)
     -- NOTE: just a kind placeholder. rN types never appear after parsing.
kindcheckType _ (BangT :@ p)                                          = pure (T 8 :@ p)
  -- FIXME: bang types cannot be used as stack types or continuations, and cannot be found
  --        in stacks; but they have to be able to be unified to any kind whatsoever.
  --        For now, we only have T8 but we will have to give it T0 later.
kindcheckType g (PackedStructT ts :@ p)                               = do
  sizes <- mapM ((sizeof =<<) . kindcheckType g) ts
  pure (T (sum sizes) :@ p)

--------------------------------------------------------------------------------------------

unifyKinds :: (?tcFlags :: TypecheckerFlags, k ~ Located Kind) => k -> k -> Kindchecker (Subst k)
unifyKinds (k1 :@ p1) (k2 :@ p2) = case (k1, k2) of
  (_, _) | k1 == k2 -> pure mempty
  (Ta, T n) -> pure mempty
----------------------------------------------------------------------------------------
  (Ts, _)  -> throwError (NotAStackType (k2 :@ p2) p2)
  (T n, Ta) -> throwError (NotSized (k2 :@ p2) p2)
  (T n, _)  -> throwError (NotADataType (k2 :@ p2) p2)
  (Ta, _)  -> throwError (NotADataType (k2 :@ p2) p2)
  (_, _)   -> throwError (CannotUnifyKinds (k1 :@ p1) (k2 :@ p2))




sizeof :: Located Kind -> Kindchecker Integer
sizeof (T n :@ _) = pure n
sizeof (k :@ p)  = throwError (NotSized (k :@ p) p)

-----------------------------------------------------------------------------------------------

-- | Requires a kind to be indicating of a stack type.
--
--   Essentially a wrapper around 'isStackType', but in the 'Kindchecker' monad.
requireStackType :: (?tcFlags :: TypecheckerFlags) => Position -> Located Kind -> Kindchecker ()
requireStackType p k = () <$ unifyKinds (Ts :@ p) k

-- | Requires a kind to be indicating of a data type.
--
--   Essentially a wrapper around 'isDataType', but in the 'Kindchecker' monad.
requireDataType :: (?tcFlags :: TypecheckerFlags) => Position -> Located Kind -> Kindchecker ()
requireDataType p k = do
  let s1 = runExcept $ evalStateT (unifyKinds (T 8 :@ p) k) 0
  case s1 of
    Left e -> do
      let s2 = runExcept $ evalStateT (unifyKinds (Ta :@ p) k) 0
      case s2 of
        Left e -> throwError e
        Right x -> pure ()
    Right x -> pure ()

-- | Requires a kind to be sized.
--
--   Essentially a wrapper around 'isSized', but in the 'Kindchecker' monad.
requireSized :: (?tcFlags :: TypecheckerFlags) => Position -> Located Kind -> Kindchecker ()
requireSized p (T n :@ _) = pure ()
requireSized p (k :@ p1)  = throwError (NotSized (k :@ p1) p)

requireCont :: (?tcFlags :: TypecheckerFlags) => Position -> Located Kind -> Kindchecker ()
requireCont p k = () <$ unifyKinds (Tc :@ p) k
