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
import qualified Language.NStar.Typechecker.Core as TC (TypedInstruction(..))
import Data.Located (Located(..), unLoc, getPos, Position)
import qualified Data.Map as Map
import Control.Monad.State (gets)
import Control.Monad.Except (liftEither, throwError, catchError)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromJust)
import Data.Foldable (foldrM, fold, foldlM)
import Console.NStar.Flags (TypecheckerFlags(..))
import Internal.Error (internalError)
import qualified Language.NStar.Typechecker.Env as Env (lookup)
import Control.Monad (forM)
import Data.Bifunctor (first)
import Language.NStar.Typechecker.Kinds (sizeof, unifyKinds, kindcheckType, runKindchecker, requireSized)
import Control.Monad (forM_, when, guard)
import Data.Functor ((<&>))
import Debug.Trace (traceShow)
import Control.Applicative ((<|>))

tc_ret :: (?tcFlags :: TypecheckerFlags) => Position -> Typechecker TC.TypedInstruction
tc_ret p = do
  {-
     r is a register    Ξ; Γ; χ; σ; r ⊢ᵀ r : ∀().{ χ́′ | σ → ε }     χ ∼ χ′
  ──────────────────────────────────────────────────────────────────────────── return to register
                       Ξ; Γ; χ; σ; r ⊢ᴵ ret ⊣ χ; σ; r
  -}

  e :@ p1 <- gets (epsilon . snd)
  case e of
    -- > r is a register
    RegisterContT r -> do
      x <- gets (chi . snd)
      s <- gets (sigma . snd)
      e <- freshVar "ε" p

      ty <- typecheckExpr (RegE (r :@ p)) p False
      -- > Ξ; Γ; χ; σ; r ⊢ᵀ r : ∀().{ χ́′ | σ → ε }
      -- > χ ∼ χ′
      unify ty (ForAllT [] (RecordT x s e False :@ p) :@ p) -- `catchError` const (throwError (NoReturnAddress p r x))

      -- > Ξ; Γ; χ; σ; r ⊢ᴵ ret ⊣ χ; σ; r
      pure (TC.JMP (RegE (r :@ p) :@ p))
    -- > n ∈ ℕ
    StackContT n -> throwError (CannotReturnToStackContinuation e p)
    VarT _ -> throwError (AbstractContinuationOnReturn p (e :@ p1))
    _ -> internalError $ "invalid return continuation " <> show e

tc_mv :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Located Register -> Position -> Typechecker TC.TypedInstruction
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

      -- > r, d are registers
      -- > Ξ; Γ; χ; σ; r ⊢ᴵ mv r, d ⊣ χ, d : ∀().ζ; σ; d
      setEpsilon (RegisterContT dst :@ p2)
      extendChi (dst :@ p2) ty

      pure ()
    _ -> do
      g <- gets (gamma . snd)

      -- > Ξ; Γ; σ; ε ⊢ᵀ e : t
      t <- typecheckExpr src p1 False
      -- > Γ ⊢ᴷ t : T8
      liftEither (first FromReport . runKindchecker $ unifyKinds (T8 :@ p1) =<< kindcheckType g t)

      -- > r is a register
      e <- gets (epsilon . snd)
      when ((RegisterContT dst :@ p2) == e) do
        throwError (TryingToOverwriteRegisterContinuation (dst :@ p2) p3)
      -- > Ξ; χ; σ; ε ⊢ᴵ mv e, r ⊣ χ, r : t; σ; ε
      extendChi (dst :@ p2) t

      pure ()

  pure (TC.MV (src :@ p1) (dst :@ p2))

tc_jmp :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Position -> Typechecker TC.TypedInstruction
tc_jmp (e :@ p1) p2 = do
  {-
     Ξ; Γ; χ; σ; ε ⊢ᵀ l<v⃗> : ∀().{ χ′ | σ → ε }     χ ∼ χ′
  ─────────────────────────────────────────────────────────────── jump to component
            Ξ; Γ; χ; σ; ε ⊢ᴵ jmp l<v⃗> ⊣ χ; σ; ε
  -}

  -- > Ξ; Γ; χ; σ; ε ⊢ᵀ l<v⃗> : ∀().{ χ′ | σ → ε }
  -- > χ ∼ χ′
  ty <- typecheckExpr e p1 False

  x <- gets (chi . snd)
  s <- gets (sigma . snd)
  e' <- gets (epsilon . snd)
  unify (ForAllT [] (RecordT x s e' False :@ p1) :@ p1) ty

  pure (TC.JMP (e :@ p1))

tc_call :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Position -> Typechecker TC.TypedInstruction
tc_call (ex :@ p1) p2 = do
  {-
     r is a register      Ξ; Γ; χ; σ; r ⊢ᵀ l<v⃗> : ∀().{ χ′ | σ → r }
         Ξ; Γ; χ; σ; r ⊢ᵀ r : ∀().{ χ′′ | σ′ → ε′ }      χ ∼ χ′
  ────────────────────────────────────────────────────────────────────────
                 Ξ; Γ; χ; σ; r ⊢ᴵ call l<v⃗> ⊣ χ; σ; r

       n ∈ ℕ     n ≤ p       Ξ; Γ; χ; σ; n ⊢ᵀ l<v⃗> : ∀().{ χ′ | σ → n }
      tₙ ∼ ∀().{ χ′′ | σ′ → ε′ }      χ ∼ χ′      σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s
  ─────────────────────────────────────────────────────────────────────────────
                    Ξ; Γ; σ; n ⊢ᴵ call l<v⃗> ⊣ χ; σ; n
  -}

  e@(e' :@ p3) <- gets (epsilon . snd)
  x <- gets (chi . snd)
  s <- gets (sigma . snd)
  -- > Ξ; Γ; χ; σ; ε ⊢ᵀ l<v⃗> : ∀().{ χ′ | σ → ε }
  -- > χ ∼ χ′
  ty <- typecheckExpr ex p1 False
  unify (ForAllT [] (RecordT x s e False :@ p1) :@ p1) ty

  case e' of
    -- > r is a register
    RegisterContT r -> do
      -- > Ξ; Γ; χ; σ; r ⊢ᵀ r : ∀().{ χ′′ | σ′ → ε′ }
      ty <- typecheckExpr (RegE (r :@ p2)) p2 False
      s' <- freshVar "σ" p1
      e' <- freshVar "ε" p1
      unify (ForAllT [] (RecordT mempty s' e' False :@ p2) :@ p2) ty

      pure ()
    -- > n ∈ ℕ
    StackContT n -> do
      -- > σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s
      -- > n ≤ p
      -- > tₙ ∼ ∀().{ χ′′ | σ′ → ε′ }
      ty <- getNthFromStack n s
      s' <- freshVar "σ" p1
      e' <- freshVar "ε" p1
      unify (ForAllT [] (RecordT mempty s' e' False :@ p2) :@ p2) ty

      pure ()
    VarT _ -> throwError (CannotCallWithAbstractContinuation e' p2)
    _ -> internalError $ "Unknown return continuation " <> show e

  pure (TC.JMP (ex :@ p1))

tc_nop :: (?tcFlags :: TypecheckerFlags) => Position -> Typechecker TC.TypedInstruction
tc_nop _ = do
  {-

  ───────────────────────────────────
    Ξ; Γ; χ; σ; ε ⊢ᴵ nop ⊣ χ; σ; ε

  -}
  pure TC.NOP

tc_salloc :: (?tcFlags :: TypecheckerFlags) => Located Type -> Position -> Typechecker TC.TypedInstruction
tc_salloc (t :@ p1) p2 = do
  {-
      n, m ∈ ℕ       Γ ⊢ᴷ t : Tm        σ′ = t ∷ σ
  ────────────────────────────────────────────────────── salloc with stack continuation
        Ξ; Γ; χ; σ; n ⊢ᴵ salloc t ⊣ χ; σ′; n + 1

      m ∈ ℕ      Γ ⊢ᴷ t : Tm        σ′ = t ∷ σ
  ─────────────────────────────────────────────────
       Ξ; Γ; χ; σ; ε ⊢ᴵ salloc t ⊣ χ; σ′; ε
  -}

  g <- gets (gamma . snd)
  s <- gets (sigma . snd)
  e :@ p3 <- gets (epsilon . snd)

  -- > σ′ = t ∷ σ
  let s' = ConsT (t :@ p1) s :@ p1

  -- > m ∈ ℕ
  -- > Γ ⊢ᴷ t : Tm
  m <- liftEither $ first FromReport $ runKindchecker do
    k <- kindcheckType g (t :@ p1)
    requireSized p1 k
    sizeof k

  case e of
    -- > n ∈ ℕ
    -- > Ξ; Γ; χ; σ; n ⊢ᴵ salloc t ⊣ χ; σ′; n + 1
    StackContT n -> setEpsilon (StackContT (n + 1) :@ p3)
    -- > Ξ; Γ; χ; σ; ε ⊢ᴵ salloc t ⊣ χ; σ′; ε
    _ -> pure ()

  setStack s'

  pure (TC.SALLOC (m :@ p1))

tc_sfree :: (?tcFlags :: TypecheckerFlags) => Position -> Typechecker TC.TypedInstruction
tc_sfree p = do
  {-
     n ≥ 1      n ≤ p      σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s      σ′ = t1 ∷ … ∷ tₚ ∷ s
  ────────────────────────────────────────────────────────────────────────────── sfree with stack continuation
                    Ξ; Γ; χ; σ; n ⊢ᴵ sfree ⊣ χ; σ′; n-1

     σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s      σ′ = t1 ∷ … ∷ tₚ ∷ s
  ────────────────────────────────────────────────────────
            Ξ; Γ; χ; σ; ε ⊢ᴵ sfree ⊣ χ; σ′; ε
  -}

  s <- gets (sigma . snd)
  e@(e' :@ p1) <- gets (epsilon . snd)
  g <- gets (gamma . snd)

  t <- freshVar "τ" p
  s' <- freshVar "σ" p

  let ty = ConsT t s' :@ p
  sub <- unify ty s
  let ConsT t s' :@ _ = apply sub ty

  m <- liftEither $ first FromReport $ runKindchecker do
    k <- kindcheckType g t
    requireSized p k
    sizeof k

  case e' of
    StackContT n -> do
      when (n == 0) do
        throwError (CannotDiscardContinuationFromStackTop p)

      setEpsilon (StackContT (n - 1) :@ p)

      pure ()
    _ -> do
      setStack s'

  pure (TC.SFREE (m :@ p))

---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------

getNthFromStack :: Integer -> Located Type -> Typechecker (Located Type)
getNthFromStack n s = do
  let (_, tN) = foldl (\ (p, r) t -> (p + 1, r <|> if p == n then Just t else Nothing)) (0, Nothing) (unCons s)
  case tN of
    Nothing -> throwError (StackIsNotBigEnough n (unLoc s) (getPos s))
    Just t  -> pure t
  where
    unCons (ConsT t1 t2 :@ _) = t1 : unCons t2
    unCons t                  = [t]


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
typecheckExpr (NameE n@(name :@ _) ts) p _ = do
  xiD <- gets (xiD . snd)
  xiC <- gets (xiC . snd)
  ty <- maybe (throwError (UnknownDataLabel name p)) pure (Env.lookup n xiD <|> Env.lookup n xiC)

  case ty of
    ForAllT binds t :@ p2 -> do
      let n = length ts
          p' = length binds

      case p' `compare` n of
        LT -> throwError (TooMuchSpecialization n p' p)
        GT -> throwError (CannotInferSpecialization n p' p)
        EQ -> pure ()

      g <- gets (gamma . snd)
      specKinds <- forM ts \ t -> do
        liftEither $ first FromReport (runKindchecker (kindcheckType g t))
      let kindsToUnify = zip (snd <$> binds) specKinds
      forM_ kindsToUnify \ (k1, k2) -> do
        liftEither $ first FromReport (runKindchecker (unifyKinds k1 k2))

      let sub = Subst (Map.fromList (zip (fromVar . fst <$> binds) ts))

      pure $ apply sub (relax $ ForAllT [] t :@ p)
    _ -> do
      when (not $ null ts) do
        throwError (TooMuchSpecialization (length ts) 0 p)
      pure ty
  where
    fromVar (VarT n :@ _) = n
    fromVar t             = internalError $ "Cannot get name of non type-variable " <> show t
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
  (ForAllT [] r1, ForAllT [] r2) -> unify r1 r2
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
    relaxType (ForAllT b t)      = ForAllT b (relax t)
      -- NOTE: This will probably need to be tweaked
      --       Because we don't rename the variables bound, any flexible and rigid variable can be confused.
    relaxType t                  = t
