{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Language.NStar.Typechecker.Instructions where

import Language.NStar.Typechecker.Errors
import Language.NStar.Typechecker.TC
import Language.NStar.Typechecker.Free
import Language.NStar.Typechecker.Subst
import Language.NStar.Typechecker.Core
import Language.NStar.Syntax.Core (Expr(..), Immediate(..), Constant(..))
import Data.Located (Located(..), unLoc, getPos, Position)
import qualified Data.Map as Map
import Control.Monad.State (gets)
import Control.Monad.Except (liftEither, throwError, catchError)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromJust)
import Data.Foldable (fold, foldlM)
import Console.NStar.Flags (TypecheckerFlags(..))
import Internal.Error (internalError)
import qualified Language.NStar.Typechecker.Env as Env (lookup)
import Control.Monad (forM)
import Data.Bifunctor (first)
import Language.NStar.Typechecker.Kinds (unifyKinds, kindcheckType, runKindchecker)
import Control.Monad (forM_, when, guard)
import Data.Functor ((<&>))
import Debug.Trace (traceShow)
import Control.Applicative ((<|>))

tc_ret p = do

tc_mv :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Located Expr -> Position -> Typechecker ()
tc_mv (src :@ p1) (dst :@ p2) p3 = do
  {-
     r, d are registers     Ξ; Γ; χ; σ; ε ⊢ᵀ r : ∀().ζ
  ──────────────────────────────────────────────────────── move continuation
      Ξ; Γ; χ; σ; r ⊢ᴵ mv r, d ⊣ χ, d : ∀().ζ; σ; d

     r is a register     Γ ⊢ᴷ t : T8     Ξ; Γ; σ; ε ⊢ᵀ e : t
  ───────────────────────────────────────────────────────────── move value
           Ξ; Γ; χ; σ; ε ⊢ᴵ mv e, r ⊣ χ, r : t; σ; ε
  -}

  e <- gets (epsilon . snd)
  x <- gets (chi . snd)
  case (e, src) of
    (RegisterContT r1 :@ _, RegE r2'@(r2 :@ _)) | r1 == r2 -> do
      -- > Ξ; Γ; χ; σ; ε ⊢ᵀ r : ∀().ζ
      z <- freshVar "ζ" p1
      subZ <- unify (ForAllT [] z :@ p1) (x Map.! r2')
      let ty = apply subZ z

      case dst of
        -- > r, d are registers
        RegE d -> do
          -- > Ξ; Γ; χ; σ; r ⊢ᴵ mv r, d ⊣ χ, d : ∀().ζ; σ; d
          setEpsilon (RegisterContT <$> d)
          extendChi d ty
        _ -> error $ "Unknown mv destination: " <> show dst

      pure ()
    _ -> do
      g <- gets (gamma . snd)

      -- > Ξ; Γ; σ; ε ⊢ᵀ e : t
      t <- typecheckExpr src p1 False
      -- > Γ ⊢ᴷ t : T8
      liftEither (first FromReport . runKindchecker $ unifyKinds (T8 :@ p1) =<< kindcheckType g t)

      case dst of
        -- > r is a register
        RegE r -> do
          e <- gets (epsilon . snd)
          guard ((RegisterContT <$> r) /= e)
            <|> throwError (TryingToOverwriteRegisterContinuation r p3)
          -- > Ξ; χ; σ; ε ⊢ᴵ mv e, r ⊣ χ, r : t; σ; ε
          extendChi r t
        _ -> error $ "Unknown mv destination: " <> show dst

      pure ()

tc_jmp :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Position -> Typechecker ()
tc_jmp _ _ = error "Unimplemented type-checking for 'jmp'"

tc_call :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Position -> Typechecker ()
tc_call _ _ = error "Unimplemented type-checking for 'call'"

tc_nop :: (?tcFlags :: TypecheckerFlags) => Position -> Typechecker ()
tc_nop _ = do
  {-

  ───────────────────────────────────
    Ξ; Γ; χ; σ; ε ⊢ᴵ nop ⊣ χ; σ; ε

  -}
  pure ()

---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------

typecheckExpr :: (?tcFlags :: TypecheckerFlags) => Expr -> Position -> Bool -> Typechecker (Located Type)
typecheckExpr (ImmE (I _ :@ _)) p _  = pure (UnsignedT 64 :@ p)
typecheckExpr (ImmE (C _ :@ _)) p _  = pure (SignedT 8 :@ p)
typecheckExpr (RegE r) p _           = do
  ctx <- gets (chi . snd)
  maybe (throwError (RegisterNotFoundInContext (unLoc r) p (Map.keysSet ctx))) pure (Map.lookup r ctx)
typecheckExpr (IndexedE (off :@ p1) (e :@ p2)) p unsafe    = do
  ty <- typecheckExpr off p1 unsafe
  unify ty (SignedT 64 :@ p)

  ty2 :@ p3 <- typecheckExpr e p2 unsafe
  case ty2 of
    PtrT t  ->
      case off of
        ImmE _ -> pure t -- TODO: check if immediate offset is correct
        _ | unsafe -> pure t
          | otherwise -> throwError (UnsafeOperationOutOfUnsafeBlock p)
    --SPtr s -> error $ "Unimplemented `typecheckExpr` for pointer offset on '" <> show ty2 <> "'."
    -- TODO: handle stack access
    _      -> throwError (NonPointerTypeOnOffset ty2 p3)
typecheckExpr (NameE n@(name :@ _) _) p _ = do
  ctx <- gets (xiD . snd)
  maybe (throwError (UnknownDataLabel name p)) pure (Env.lookup n ctx)
typecheckExpr e p _                 = error $ "Unimplemented `typecheckExpr` for '" <> show e <> "'."

typecheckConstant :: (?tcFlags :: TypecheckerFlags) => Constant -> Position -> Typechecker (Located Type)
typecheckConstant (IntegerC _) p   = pure (UnsignedT 64 :@ p)
typecheckConstant (CharacterC _) p = pure (SignedT 8 :@ p)
typecheckConstant (ArrayC csts) p  =
  if | (c1:cs) <- csts -> do
         types <- forM csts \ (c :@ p) -> typecheckConstant c p
         let (t1:ts) = types
         when (not (null ts)) do
           () <$ foldlM2 (\ acc t1 t2 -> mappend acc <$> unify t1 t2) mempty (t1:ts)
         pure (PtrT t1 :@ p)
     | otherwise       -> do
         v <- freshVar "d" p
         pure (PtrT v :@ p)
  where
    foldlM2 :: (Monad m) => (b -> a -> a -> m b) -> b -> [a] -> m b
    foldlM2 f e l = foldlM (uncurry . f) e (zip l (drop 1 l))

--------------------------------------------------------------------------------

-- | Generates a fresh free type variable based on a given prefix, for the current source position.
freshVar :: (?tcFlags :: TypecheckerFlags) => Text -> Position -> Typechecker (Located Type)
freshVar prefix pos = do
  n <- gets fst
  incrementCounter
  pure (FVarT ((prefix <> Text.pack (show n)) :@ pos) :@ pos)

-- | Unifies two types, and returns the substitution from the first to the second.
unify :: (?tcFlags :: TypecheckerFlags) => (t ~ Located Type) => t -> t -> Typechecker (Subst t)
unify (t1 :@ p1) (t2 :@ p2) = case (t1, t2) of
  _ | t1 == t2 -> pure mempty
  -- Signed types are all coercible.
  (SignedT _, SignedT _) -> pure mempty
  -- Unsigned types are all coercible.
  (UnsignedT _, UnsignedT _) -> pure mempty
  -- Signed and unsigned integers can also be coerced to each other.
  (SignedT _, UnsignedT _) -> pure mempty
  (UnsignedT _, SignedT _) -> pure mempty
  -- Two pointers are coercible if their pointed types are coercible.
  (PtrT t1, PtrT t2) -> unify t1 t2
  -- Free type variables can be bound to anything as long as it does not create an infinite type.
  (FVarT v, t) -> bind (v, p1) (t, p2)
  (t, FVarT v) -> bind (v, p2) (t, p1)
  -- Contexts are coercible if the intersection of their registers are all coercible.
  (RecordT m1 s1 c1 o1, RecordT m2 s2 c2 o2) -> do
    let k1 = Map.keysSet m1
        k2 = Map.keysSet m2
    let common = k1 `Set.intersection` k2
        other1 = k1 Set.\\ common
        other2 = k2 Set.\\ common

    let computeExtension :: Bool -> [Located Register] -> Position -> Typechecker (Subst (Located Type))
        computeExtension _ [] _     = pure mempty
        computeExtension False fs p = throwError (MissingRegistersInContext (unLoc <$> fs) p)
        computeExtension _ fs p     = do
          let newFields = Map.fromList (fs <&> \ k -> (k, m1 Map.! k))
          pure mempty

    subs <- fold <$> forM (Set.toList common) \ k -> unify (m1 Map.! k) (m2 Map.! k)
    sub1 <- computeExtension o2 (Set.toList other1) p2
    sub2 <- computeExtension o1 (Set.toList other2) p1

    sub3 <- unify s1 s2
    sub4 <- unify c1 c2

    pure (sub1 <> sub2 <> sub3 <> sub4 <> subs)
  -- A stack constructor can be unified to another stack constructor if
  -- both stack head and stack tail of each stack can be unified.
  (ConsT t1 t3, ConsT t2 t4) -> unifyMany [t1, t3] [t2, t4]
  -- Any other combination is not possible
  _ -> throwError (Uncoercible (t1 :@ p1) (t2 :@ p2))

-- | Unifies many types, yielding the composition of all the substitutions created.
unifyMany :: (?tcFlags :: TypecheckerFlags) => (t ~ Located Type) => [t] -> [t] -> Typechecker (Subst t)
unifyMany [] []         = pure mempty
unifyMany (x:xs) (y:ys) = do
  sub1 <- unify x y
  sub2 <- unifyMany (apply sub1 xs) (apply sub1 ys)
  pure (sub1 <> sub2)
unifyMany l r           =
  error ("Could not unify " <> show l <> " and " <> show r <> " because they aren't of the same size.")

-- | Tries to bind a free type variable to a type, yielding a substitution from the variable to the type if
--   it succeeded.
bind :: (?tcFlags :: TypecheckerFlags) => (Located Text, Position) -> (Type, Position) -> Typechecker (Subst (Located Type))
bind (var, p1) (FVarT v, p2)
  | var == v              = pure mempty
bind (var, p1) (ty, p2)
  | occursCheck var ty    = throwError (InfiniteType (ty :@ p2) var)
  | otherwise             = pure (Subst (Map.singleton var (ty :@ p2)))
 where
   occursCheck v t = v `Set.member` freeVars t

relax :: Located Type -> Located Type
relax (t :@ p) = relaxType t :@ p
  where
    relaxType (ConsT t1 t2)      = ConsT (relax t1) (relax t2)
    relaxType (VarT n)           = FVarT n
    relaxType (RecordT ms s c o) = RecordT (relax <$> ms) (relax s) c o
    relaxType (PtrT t)           = PtrT (relax t)
    relaxType (ForAllT _ t)      = unLoc $ relax t
    relaxType t                 = t
