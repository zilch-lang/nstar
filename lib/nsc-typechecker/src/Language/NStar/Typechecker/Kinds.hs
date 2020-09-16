{-|
  Module: Language.NStar.Typechecker.Kinds
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Typechecker.Kinds (kindcheck) where

{-

    Because there is close to nothing about kind inference in N*,
    there probably won't be anything in this module.

    One thing that belongs here is the kind checking.
    So:
    - `a::s` needs `a: T8|Ta` and `s: Ts` and returns `Ts`
    - `{%rax: s}` needs `a: T8`
    - `sptr s` needs `s: Ts` and returns `T8`
    - `*a` needs `a: T8|Ta` and returns `T8` (on x64)

-}

import Control.Monad.Except
import Data.Located
import Language.NStar.Typechecker.Core
import Text.Diagnose (Report)
import Data.Bifunctor (first, second)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative (liftA2)
import Language.NStar.Typechecker.Errors

type Kindchecker a = Except KindcheckError a

data KindcheckError
  = NotAStackType (Located Kind)
  | NotADataType (Located Kind)
  | NotSized (Located Kind)
  | UnboundTypeVariable (Located Text)

--------------------------------------------------------------------------------------------

kindcheck :: Located Type -> Either (Report String) ()
kindcheck = second (const ()) . first fromKindcheckError . runExcept . kindcheckType mempty

fromKindcheckError :: KindcheckError -> Report String
fromKindcheckError (NotAStackType (k :@ p))       = kindIsNotAStackKind k p
fromKindcheckError (NotADataType (k :@ p))        = kindIsNotADataKind k p
fromKindcheckError (NotSized (k :@ p))            = kindIsUnsized k p
fromKindcheckError (UnboundTypeVariable (v :@ p)) = unboundTypeVariable v p

kindcheckType :: Map (Located Text) (Located Kind) -> Located Type -> Kindchecker (Located Kind)
kindcheckType _ (Signed _ :@ p)            = pure (T8 :@ p)
kindcheckType _ (Unsigned _ :@ p)          = pure (T8 :@ p)
    -- TODO: For now we ignore the size of both types, because there are no sized kinds other than @T8@.
kindcheckType ctx (ForAll newCtx ty :@ _)  = kindcheckType (Map.fromList (first varName <$> newCtx) <> ctx) ty
  where varName (Var v :@ _) = v
        varName (t :@ _)     = error $ "Cannot fetch name of non type-variable type '" <> show t <> "'."
kindcheckType ctx (Var v :@ _)             = maybe (throwError (UnboundTypeVariable v)) pure (Map.lookup v ctx)
kindcheckType _ (FVar v :@ _)              = throwError (UnboundTypeVariable v)
kindcheckType ctx (Ptr t :@ p)             = (T8 :@ p) <$ kindcheckType ctx t
kindcheckType ctx (SPtr t :@ p)            = (Ts :@ p) <$ (requireStackType =<< kindcheckType ctx t)
kindcheckType ctx (Cons t1 t2 :@ p)        = do
  liftA2 (*>) requireDataType requireSized =<< kindcheckType ctx t1
  requireStackType =<< kindcheckType ctx t2
  pure (Ts :@ p)
kindcheckType ctx (Record mappings :@ p)   =
  (Ta :@ p) <$ Map.traverseWithKey handleTypeFromRegister mappings
       -- FIXME: Kind checking does not take into account the size of the types, so if they are sized, they all are 8-bytes big at the moment.
 where handleTypeFromRegister (RSP :@ _) ty = requireStackType =<< kindcheckType ctx ty
       handleTypeFromRegister _ ty          = liftA2 (*>) requireDataType requireSized =<< kindcheckType ctx ty

--------------------------------------------------------------------------------------------

-- | Checks whether a kind indicates a stack type, or not.
isStackType :: Located Kind -> Bool
isStackType (Ts :@ _) = True
isStackType _         = False

-- | Checks whether a kind indicates a data type, or not.
--
--   @'isDataType' = 'not' '.' 'isStackType'@
isDataType :: Located Kind -> Bool
isDataType = not . isStackType

-- | Checks whether a kind relates to some sized data or not.
isSized :: Located Kind -> Bool
isSized (T8 :@ _) = True
isSized _         = False

-- | Requires a kind to be indicating of a stack type.
--
--   Essentially a wrapper around 'isStackType', but in the 'Kindchecker' monad.
requireStackType :: Located Kind -> Kindchecker ()
requireStackType k | isStackType k = pure ()
                   | otherwise     = throwError (NotAStackType k)

-- | Requires a kind to be indicating of a data type.
--
--   Essentially a wrapper around 'isDataType', but in the 'Kindchecker' monad.
requireDataType :: Located Kind -> Kindchecker ()
requireDataType k | isDataType k = pure ()
                  | otherwise    = throwError (NotADataType k)

-- | Requires a kind to be sized.
--
--   Essentially a wrapper around 'isSized', but in the 'Kindchecker' monad.
requireSized :: Located Kind -> Kindchecker ()
requireSized k | isSized k = pure ()
               | otherwise = throwError (NotSized k)
