{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Language.NStar.Typechecker.Instructions where

import Console.NStar.Flags (TypecheckerFlags (..))
import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, when)
import Control.Monad.Except (liftEither, throwError)
import Control.Monad.State (gets)
import Data.Bifunctor (first)
import Data.Foldable (fold, foldlM)
import Data.Functor ((<&>))
import Data.Located (Located (..), getPos, unLoc)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceShow)
import Error.Diagnose
import Internal.Error (internalError)
import Language.NStar.Syntax.Core (Constant (..), Immediate (..))
import Language.NStar.Typechecker.Core
import qualified Language.NStar.Typechecker.Core as TC (TypedInstruction (..))
import qualified Language.NStar.Typechecker.Env as Env (lookup)
import Language.NStar.Typechecker.Errors
import Language.NStar.Typechecker.Free
import Language.NStar.Typechecker.Kinds (kindcheckType, requireSized, runKindchecker, sizeof, unifyKinds)
import Language.NStar.Typechecker.Subst
import Language.NStar.Typechecker.TC

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

      (ty, _) <- typecheckExpr (RegE (r :@ p)) p False
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
             t ≠ !
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
      extendChi (dst :@ p2) (ForAllT [] ty :@ p1)

      pure ()
    _ -> do
      g <- gets (gamma . snd)

      -- > Ξ; Γ; σ; ε ⊢ᵀ e : t
      (t, _) <- typecheckExpr src p1 False
      -- > Γ ⊢ᴷ t : T8
      liftEither (first FromReport . runKindchecker $ unifyKinds (T 8 :@ p1) =<< kindcheckType g t)

      -- > t ≠ !
      case (t, src) of
        (BangT :@ _, RegE r) -> throwError (RegisterNotFoundInContext (unLoc r) p1 (Map.keysSet $ Map.filter (/= (BangT :@ p1)) x))
        _ -> pure ()

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
  (ty, _) <- typecheckExpr e p1 False

  x <- gets (chi . snd)
  s <- gets (sigma . snd)
  e' <- gets (epsilon . snd)
  unify (ForAllT [] (RecordT x s e' False :@ p1) :@ p1) ty

  pure (TC.JMP (e :@ p1))

tc_call :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Position -> Typechecker TC.TypedInstruction
tc_call (ex :@ p1) p2 = do
  {-
            r is a register           ρ = χ′′ ∖ χ′′′
     Ξ; Γ; χ; σ; r ⊢ᵀ l<v⃗> : ∀().{ χ′, r : ∀().{ χ′′′ | σ′ → ε′ } | σ → r }
         Ξ; Γ; χ; σ; r ⊢ᵀ r : ∀().{ χ′′ | σ′ → ε′ }      χ ∼ χ′ ∪ ρ
                         ⋀(r ∉ ρ | r ∈ bang(χ′))
  ──────────────────────────────────────────────────────────────────────────────
                 Ξ; Γ; χ; σ; r ⊢ᴵ call l<v⃗> ⊣ χ; σ; r

       n ∈ ℕ     n ≤ p       Ξ; Γ; χ; σ; n ⊢ᵀ l<v⃗> : ∀().{ χ′ | σ^ → n }
         σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s         σ^ = t₀′ ∷ t₁′ ∷ … ∷ tₚ′ ∷ s
        tₙ ∼ ∀().{ χ′′ | σ′ → ε′ }        tₙ′ ∼ ∀().{ χ′′′ | σ′ → ε′ }
         ρ = χ′′ ∖ χ′′′     χ ∼ χ′ ∪ ρ        ⋀(r ∉ ρ | r ∈ bang(χ′))
  ─────────────────────────────────────────────────────────────────────────────
                    Ξ; Γ; σ; n ⊢ᴵ call l<v⃗> ⊣ χ; σ; n
  -}

  e <- gets (epsilon . snd)
  x <- gets (chi . snd)
  s <- gets (sigma . snd)

  -- > Ξ; Γ; χ; σ; ε ⊢ᵀ l<v⃗> : ∀().{ χ′ | σ → ε }
  ep <- freshVar "ε" p1
  (ty, _) <- typecheckExpr ex p1 False
  sub <- unify (ForAllT [] (RecordT mempty s ep True :@ p1) :@ p1) ty
  let ForAllT [] (RecordT x' ŝ _ _ :@ _) :@ _ = ty

  (x'', x''') <- case apply sub ep of
    -- > r is a register
    RegisterContT r :@ _ -> do
      -- > r : ∀().{ χ′′′ | σ′ → ε′ } ∈ χ′

      contTy <- maybe (throwError $ RegisterNotFoundInContext r p1 (Map.keysSet x')) pure $ Map.lookup (r :@ p1) x'

      s' <- freshVar "σ" p1
      e' <- freshVar "ε" p1
      sub1 <- unify (ForAllT [] (RecordT mempty s' e' True :@ p1) :@ p1) contTy

      let ForAllT [] (RecordT x''' _ _ _ :@ _) :@ _ = contTy

      -- > Ξ; Γ; χ; σ; r ⊢ᵀ r : ∀().{ χ′′ | σ′ → ε′ }
      (ty2, _) <- typecheckExpr (RegE (r :@ p2)) p2 False
      unify (ForAllT [] (RecordT mempty (apply sub1 s') (apply sub1 e') True :@ p2) :@ p2) ty2

      let ForAllT [] (RecordT x'' _ _ _ :@ _) :@ _ = ty2

      pure (x'', x''')
    -- > n ∈ ℕ
    StackContT n :@ _ -> do
      -- > σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s
      -- > n ≤ p
      -- > tₙ ∼ ∀().{ χ′′ | σ′ → ε′ }
      (_, ty) <- getNthFromStack n s
      s' <- freshVar "σ" p1
      e' <- freshVar "ε" p1
      sub1 <- unify (ForAllT [] (RecordT mempty s' e' True :@ p2) :@ p2) ty

      let ForAllT [] (RecordT x'' _ _ _ :@ _) :@ _ = ty

      -- > σ^ = t₀′ ∷ t₁′ ∷ … ∷ tₚ′ ∷ s
      -- > n ≤ p
      -- > tₙ′ ∼ ∀().{ χ′′′ | σ′ → ε′ }
      (_, ty2) <- getNthFromStack n ŝ
      unify (ForAllT [] (RecordT mempty (apply sub1 s') (apply sub1 e') True :@ p2) :@ p2) ty2

      let ForAllT [] (RecordT x''' _ _ _ :@ _) :@ _ = ty2

      pure (x'', x''')
    VarT _ :@ _ -> throwError (CannotCallWithAbstractContinuation (unLoc ep) p2)
    _ -> internalError $ "Unknown return continuation " <> show e

  -- > ρ = χ′′ ∖ χ′′′
  let p = Map.difference x'' x'''

  -- > χ ∼ χ′ ∪ ρ
  s' <- freshVar "σ" p1
  e' <- freshVar "ε" p1
  unify (RecordT x s' e' False :@ p1) (RecordT (Map.union x' p) s' e' False :@ p1)

  -- > ⋀(r ∉ ρ | r ∈ bang(χ′))
  let bangs = Map.keys $ Map.filter (== (BangT :@ p1)) x'
      conj = any (`Map.member` p) bangs
  when conj do
    throwError (RegisterCannotBePropagated (Map.keys p) bangs p1 p2)

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

  let t' = close t

  -- > σ′ = t ∷ σ
  let s' = ConsT (t' :@ p1) s :@ p1

  -- > m ∈ ℕ
  -- > Γ ⊢ᴷ t : Tm
  m <- liftEither $
    first FromReport $ runKindchecker do
      k <- kindcheckType g (t' :@ p1)
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

  -- > σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s
  t <- freshVar "τ" p
  s' <- freshVar "σ" p

  let ty = ConsT t s' :@ p
  sub <- unify ty s
  -- > σ′ = t1 ∷ … ∷ tₚ ∷ s
  let ConsT t s' :@ _ = apply sub ty

  m <- liftEither $
    first FromReport $ runKindchecker do
      k <- kindcheckType g t
      requireSized p k
      sizeof k

  case e' of
    -- > n ≥ 1
    -- > n ≤ p
    StackContT n -> do
      when (n == 0) do
        throwError (CannotDiscardContinuationFromStackTop p)

      -- > Ξ; Γ; χ; σ; n ⊢ᴵ sfree ⊣ χ; σ′; n-1
      setEpsilon (StackContT (n - 1) :@ p)
      setStack s'

      pure ()
    _ -> do
      -- > Ξ; Γ; χ; σ; ε ⊢ᴵ sfree ⊣ χ; σ′; ε
      setStack s'

  pure (TC.SFREE (m :@ p))

tc_sld :: (?tcFlags :: TypecheckerFlags) => Located Integer -> Located Register -> Position -> Typechecker TC.TypedInstruction
tc_sld (n :@ p1) (r :@ p2) p3 = do
  {-
     r is a register      n ∈ ℕ      n ≤ p       σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s      tₙ ∼ ∀().ζ
  ────────────────────────────────────────────────────────────────────────────────────────── load continuation from stack in register
                    Ξ; Γ; χ; σ; n ⊢ᴵ sld n, r ⊣ χ, r : tₙ; σ; r

     r is a register      n ∈ ℕ      n ≤ p       σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s        Γ ⊢ᴷ tₙ : T8      r ≠ ε
  ─────────────────────────────────────────────────────────────────────────────────────────────────────────
                             Ξ; Γ; χ; σ; ε ⊢ᴵ sld n, r ⊣ χ, r : tₙ; σ, ε
  -}

  e :@ p4 <- gets (epsilon . snd)
  g <- gets (gamma . snd)
  s <- gets (sigma . snd)

  -- > n ∈ ℕ
  -- > n ≤ p
  -- > σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s
  (m, ty) <- getNthFromStack n s

  case e of
    StackContT n' | n == n' -> do
      -- > tₙ ∼ ∀().ζ
      z <- freshVar "ζ" p3
      unify (ForAllT [] z :@ p3) ty

      -- > Ξ; Γ; χ; σ; n ⊢ᴵ sld n, r ⊣ χ, r : tₙ; σ; r
      setEpsilon (RegisterContT r :@ p2)
      extendChi (r :@ p2) ty
      pure ()
    RegisterContT r' | r == r' -> throwError (TryingToOverwriteRegisterContinuation (r :@ p2) p3)
    _ -> do
      -- > Γ ⊢ᴷ tₙ : T8
      -- > r ≠ ε
      liftEither $
        first FromReport $ runKindchecker do
          requireSized p3 =<< kindcheckType g ty

      -- > Ξ; Γ; χ; σ; ε ⊢ᴵ sld n, r ⊣ χ, r : tₙ; σ, ε
      extendChi (r :@ p2) ty
      pure ()

  pure (TC.SLD (m :@ p3) (r :@ p2))

tc_sst :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Located Integer -> Position -> Typechecker TC.TypedInstruction
tc_sst (ex :@ p1) (n :@ p2) p3 = do
  {-
     r is a register      n ∈ ℕ      n ≤ p       tₙ ∼ ∀().ζ        σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s
                      σ′ = σ[∀().ζ ∖ tₙ]       Ξ; Γ; χ; σ; r ⊢ᵀ r : ∀().ζ
  ────────────────────────────────────────────────────────────────────────────────────────────── move continuaton onto the stack
                          Ξ; Γ; χ; σ; r ⊢ᴵ sst r, n ⊣ χ; σ′; n

     n ∈ ℕ       n ≤ p       σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s       Ξ; Γ; χ; σ; ε ⊢ᵀ e : tₙ
                              e ≠ !
  ──────────────────────────────────────────────────────────────────────────────────── store value onto the stack
                       Ξ; Γ; χ; σ; ε ⊢ᴵ sst e, n ⊣ χ; σ; ε
  -}

  g <- gets (gamma . snd)
  s <- gets (sigma . snd)
  e <- gets (epsilon . snd)

  -- > n ∈ ℕ
  -- > n ≤ p
  -- > σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s
  (m, tN) <- getNthFromStack n s

  case (e, ex) of
    (RegisterContT r :@ _, RegE (r2 :@ _)) | r == r2 -> do
      -- > Ξ; Γ; χ; σ; r ⊢ᵀ r : ∀().ζ
      (ty, _) <- typecheckExpr ex p1 False

      z <- freshVar "ζ" p1
      unify (ForAllT [] z :@ p1) ty

      -- > tₙ ∼ ∀().ζ
      unify ty tN

      -- > Ξ; Γ; χ; σ; r ⊢ᴵ sst r, n ⊣ χ; σ′; n
      s' <- setNthInStack n s ty
      setStack s'
      setEpsilon (StackContT n :@ p3)

      pure ()
    _ -> do
      -- > Ξ; Γ; χ; σ; ε ⊢ᵀ e : tₙ
      (ty, _) <- typecheckExpr ex p1 False
      unify ty tN

      x <- gets (chi . snd)

      case (tN, ex) of
        (BangT :@ _, RegE r) -> throwError (RegisterNotFoundInContext (unLoc r) p1 (Map.keysSet $ Map.filter (/= (BangT :@ p1)) x))
        _ -> pure ()

      -- > Ξ; Γ; χ; σ; ε ⊢ᴵ sst e, n ⊣ χ; σ; ε
      pure ()

  pure (TC.SST (ex :@ p1) (m :@ p2))

tc_ld :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Located Expr -> Bool -> Position -> Typechecker TC.TypedInstruction
tc_ld (ptr :@ p1) (RegE r :@ p2) unsafe p3 = do
  {-
      r is a register     r ≠ ε      Ξ; Γ; χ; σ; ε ⊢ᵀ o(p) : *τ
  ──────────────────────────────────────────────────────────────────
           Ξ; Γ; χ; σ; ε ⊢ᴵ ld o(p), r ⊣ χ, r : τ; σ; ε

      r is a register     r ≠ ε      Ξ; Γ; χ; σ; ε ⊢ᵀ p[o] : *τ
  ──────────────────────────────────────────────────────────────────
           Ξ; Γ; χ; σ; ε ⊢ᴵ ld p[o], r ⊣ χ, r : τ; σ; ε
  -}

  -- > Ξ; Γ; χ; σ; ε ⊢ᵀ o(p) : *τ
  (t, ptrOffset) <- typecheckExpr ptr p1 unsafe
  tau <- freshVar "τ" p1
  sub <- unify (PtrT tau :@ p1) t

  -- > r is a register
  -- > r ≠ ε
  e :@ p4 <- gets (epsilon . snd)
  case e of
    RegisterContT r2 | unLoc r == r2 -> throwError (TryingToOverwriteRegisterContinuation r p3)
    _ -> do
      -- > Ξ; Γ; χ; σ; ε ⊢ᴵ ld o(p), r ⊣ χ, r : τ; σ; ε
      extendChi r (apply sub tau)

  case ptrOffset of
    ByteOffsetE offset pointer :@ _ -> pure (TC.LD offset pointer r)
    _ -> internalError $ "Invalid 'ld' source " <> show ptrOffset
tc_ld ptr r unsafe p3 = internalError $ "Invalid 'ld' destination " <> show r

tc_st :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Located Expr -> Bool -> Position -> Typechecker TC.TypedInstruction
tc_st (src :@ p1) (ptr :@ p2) unsafe p3 = do
  {-
      Ξ; Γ; χ; σ; ε ⊢ᵀ e : τ       e ≠ ε      Ξ; Γ; χ; σ; ε ⊢ᵀ o(p) : *τ
  ───────────────────────────────────────────────────────────────────────────
                 Ξ; Γ; χ; σ; ε ⊢ᵀ st e, o(p) ⊣ χ; σ; ε

      Ξ; Γ; χ; σ; ε ⊢ᵀ e : τ       e ≠ ε      Ξ; Γ; χ; σ; ε ⊢ᵀ p[o] : *τ
  ───────────────────────────────────────────────────────────────────────────
                 Ξ; Γ; χ; σ; ε ⊢ᵀ st e, p[o] ⊣ χ; σ; ε
  -}

  e :@ p4 <- gets (epsilon . snd)

  -- > Ξ; Γ; χ; σ; ε ⊢ᵀ e : τ
  (ty, _) <- typecheckExpr src p1 unsafe

  case (src, e) of
    -- > e ≠ ε
    (RegE r1, RegisterContT r2)
      | unLoc r1 == r2 ->
        throwError (CannotStoreContinuationOntoHeap r2 p1 p3)
    _ -> pure ()

  -- > Ξ; Γ; χ; σ; ε ⊢ᵀ o(p) : *τ
  (ty2, ptrOffset) <- typecheckExpr ptr p2 unsafe
  unify (PtrT ty :@ p1) ty2

  case ptrOffset of
    ByteOffsetE offset pointer :@ _ -> pure (TC.ST (src :@ p1) offset pointer)
    _ -> internalError $ "Invalid 'st' destination " <> show ptrOffset

tc_sref :: (?tcFlags :: TypecheckerFlags) => Located Integer -> Located Register -> Position -> Typechecker TC.TypedInstruction
tc_sref (n :@ p1) (r :@ p2) p3 = do
  {-
      r is a register          n ∈ ℕ         n ≤ p         σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s
                         tₙ ≁ !        tₙ ≁ ∀().ζ          ε ≠ n
  ──────────────────────────────────────────────────────────────────────────────────────
                   Ξ; Γ; χ; σ; ε ⊢ᴵ sref n, r ⊣ χ, r : *tₙ; σ; ε
  -}

  g <- gets (gamma . snd)
  s <- gets (sigma . snd)

  -- > n ∈ ℕ
  -- > n ≤ p
  -- > σ = t₀ ∷ t₁ ∷ … ∷ tₚ ∷ s
  (offset, tN) <- getNthFromStack n s
  sizeofTy <- liftEither $
    first FromReport $ runKindchecker do
      k <- kindcheckType g tN
      -- > tₙ ≁ !
      requireSized (getPos tN) k
      sizeof k

  -- > tₙ ≁ ∀().ζ
  -- > ε ≠ n
  case tN of
    ForAllT _ _ :@ _ -> throwError (CannotTakeReferenceToFunctionPointerOnStack s n p1 p3)
    _ -> pure ()

  extendChi (r :@ p2) (PtrT tN :@ getPos tN)

  pure (TC.SREF (offset :@ p3) (sizeofTy :@ p3) (r :@ p3))

tc_and :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Located Expr -> Located Register -> Position -> Typechecker TC.TypedInstruction
tc_and (x :@ p1) (y :@ p2) (r :@ p3) p4 = do
  {-
     Ξ; Γ; χ; σ; ε ⊢ᵀ x : uN                Ξ; Γ; χ; σ; ε ⊢ᵀ y : uN
               r is a register              r ≠ ε
  ──────────────────────────────────────────────────────────────────────
           Ξ; Γ; χ; σ; ε ⊢ᴵ and x, y, r ⊣ χ, r : uN; σ; ε

     Ξ; Γ; χ; σ; ε ⊢ᵀ x : sN                Ξ; Γ; χ; σ; ε ⊢ᵀ y : sN
               r is a register              r ≠ ε
  ──────────────────────────────────────────────────────────────────────
           Ξ; Γ; χ; σ; ε ⊢ᴵ and x, y, r ⊣ χ, r : sN; σ; ε
  -}
  g <- gets (gamma . snd)
  e :@ p4 <- gets (epsilon . snd)

  case e of
    -- r ≠ ε
    RegisterContT r2 | r == r2 -> throwError (TryingToOverwriteRegisterContinuation (r :@ p3) p3)
    _ -> pure ()

  -- Ξ; Γ; χ; σ; ε ⊢ᵀ x : uN or Ξ; Γ; χ; σ; ε ⊢ᵀ x : sN
  (τx, x) <- typecheckExpr x p1 False
  -- Ξ; Γ; χ; σ; ε ⊢ᵀ y : uN or Ξ; Γ; χ; σ; ε ⊢ᵀ y : sN
  (τy, y) <- typecheckExpr y p2 False

  case τx of
    UnsignedT _ :@ _ -> unify τx τy
    SignedT _ :@ _ -> unify τx τy
    t :@ p5 -> throwError (ExpectedIntegralType t p5)

  extendChi (r :@ p3) τx

  pure (TC.AND x y (r :@ p3))

tc_or :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Located Expr -> Located Register -> Position -> Typechecker TC.TypedInstruction
tc_or (x :@ p1) (y :@ p2) (r :@ p3) p4 = do
  {-
     Ξ; Γ; χ; σ; ε ⊢ᵀ x : uN                Ξ; Γ; χ; σ; ε ⊢ᵀ y : uN
               r is a register              r ≠ ε
  ──────────────────────────────────────────────────────────────────────
           Ξ; Γ; χ; σ; ε ⊢ᴵ or x, y, r ⊣ χ, r : uN; σ; ε

     Ξ; Γ; χ; σ; ε ⊢ᵀ x : sN                Ξ; Γ; χ; σ; ε ⊢ᵀ y : sN
               r is a register              r ≠ ε
  ──────────────────────────────────────────────────────────────────────
           Ξ; Γ; χ; σ; ε ⊢ᴵ or x, y, r ⊣ χ, r : sN; σ; ε
  -}
  g <- gets (gamma . snd)
  e :@ p4 <- gets (epsilon . snd)

  case e of
    -- r ≠ ε
    RegisterContT r2 | r == r2 -> throwError (TryingToOverwriteRegisterContinuation (r :@ p3) p3)
    _ -> pure ()

  -- Ξ; Γ; χ; σ; ε ⊢ᵀ x : uN or Ξ; Γ; χ; σ; ε ⊢ᵀ x : sN
  (τx, x) <- typecheckExpr x p1 False
  -- Ξ; Γ; χ; σ; ε ⊢ᵀ y : uN or Ξ; Γ; χ; σ; ε ⊢ᵀ y : sN
  (τy, y) <- typecheckExpr y p2 False

  case τx of
    UnsignedT _ :@ _ -> unify τx τy
    SignedT _ :@ _ -> unify τx τy
    t :@ p5 -> throwError (ExpectedIntegralType t p5)

  extendChi (r :@ p3) τx

  pure (TC.OR x y (r :@ p3))

tc_xor :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Located Expr -> Located Register -> Position -> Typechecker TC.TypedInstruction
tc_xor (x :@ p1) (y :@ p2) (r :@ p3) p4 = do
  {-
     Ξ; Γ; χ; σ; ε ⊢ᵀ x : uN                Ξ; Γ; χ; σ; ε ⊢ᵀ y : uN
               r is a register              r ≠ ε
  ──────────────────────────────────────────────────────────────────────
           Ξ; Γ; χ; σ; ε ⊢ᴵ xor x, y, r ⊣ χ, r : uN; σ; ε

     Ξ; Γ; χ; σ; ε ⊢ᵀ x : sN                Ξ; Γ; χ; σ; ε ⊢ᵀ y : sN
               r is a register              r ≠ ε
  ──────────────────────────────────────────────────────────────────────
           Ξ; Γ; χ; σ; ε ⊢ᴵ xor x, y, r ⊣ χ, r : sN; σ; ε
  -}
  g <- gets (gamma . snd)
  e :@ p4 <- gets (epsilon . snd)

  case e of
    -- r ≠ ε
    RegisterContT r2 | r == r2 -> throwError (TryingToOverwriteRegisterContinuation (r :@ p3) p3)
    _ -> pure ()

  -- Ξ; Γ; χ; σ; ε ⊢ᵀ x : uN or Ξ; Γ; χ; σ; ε ⊢ᵀ x : sN
  (τx, x) <- typecheckExpr x p1 False
  -- Ξ; Γ; χ; σ; ε ⊢ᵀ y : uN or Ξ; Γ; χ; σ; ε ⊢ᵀ y : sN
  (τy, y) <- typecheckExpr y p2 False

  case τx of
    UnsignedT _ :@ _ -> unify τx τy
    SignedT _ :@ _ -> unify τx τy
    t :@ p5 -> throwError (ExpectedIntegralType t p5)

  extendChi (r :@ p3) τx

  pure (TC.XOR x y (r :@ p3))

---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------

getNthFromStack :: (?tcFlags :: TypecheckerFlags) => Integer -> Located Type -> Typechecker (Integer, Located Type)
getNthFromStack n s = do
  let st = unCons s
  when (n > fromIntegral (length st)) do
    throwError (StackIsNotBigEnough n (unLoc s) (getPos s))

  (_, size, tN) <- foldlM addSizeAndGetType (0, 0, Nothing) st
  case tN of
    Nothing -> internalError $ "Unfolding the stack until nth index must always yield some type"
    Just t -> pure (size, t)
  where
    unCons (ConsT t1 t2 :@ _) = t1 : unCons t2
    unCons t = [t]

    addSizeAndGetType (p, s, r@(Just _)) t = pure (p + 1, s, r)
    addSizeAndGetType (p, s, r) t = do
      g <- gets (gamma . snd)
      m <- liftEither $
        first FromReport $ runKindchecker do
          k <- kindcheckType g t
          requireSized (getPos t) k
          sizeof k

      pure if p == n then (p + 1, s, Just t) else (p + 1, s + m, Nothing)

setNthInStack :: (?tcFlags :: TypecheckerFlags) => Integer -> Located Type -> Located Type -> Typechecker (Located Type)
setNthInStack n s t@(_ :@ p) = do
  let (size, st) = replaceNth n t (unCons s)

  when (n > size) do
    throwError (StackIsNotBigEnough n (unLoc s) (getPos s))

  pure (foldr1 cons st)
  where
    unCons (ConsT t1 t2 :@ _) = t1 : unCons t2
    unCons t = [t]

    replaceNth n e = foldl (\(i, l) e' -> (i + 1, (if i == n then e else e') : l)) (0, [])
    cons t1 ts = ConsT t1 ts :@ p

typecheckExpr :: (?tcFlags :: TypecheckerFlags) => Expr -> Position -> Bool -> Typechecker (Located Type, Located Expr)
typecheckExpr e@(ImmE (I _ :@ _)) p _ =
  {-
      i is an integer immediate
  ────────────────────────────────
      Ξ; Γ; χ; σ; ε ⊢ᵀ i : u64
  -}
  pure (UnsignedT 64 :@ p, e :@ p)
typecheckExpr e@(ImmE (C _ :@ _)) p _ =
  {-
     c is a character immediate
  ─────────────────────────────────
      Ξ; Γ; χ; σ; ε ⊢ᵀ c : s8
  -}
  pure (SignedT 8 :@ p, e :@ p)
typecheckExpr e@(RegE r) p _ = do
  {-

  ────────────────────────────────────
     Ξ; Γ; χ, r : τ; σ; ε ⊢ᵀ r : τ
  -}
  ctx <- gets (chi . snd)
  maybe (throwError (RegisterNotFoundInContext (unLoc r) p (Map.keysSet ctx))) (pure . (,e :@ p)) (Map.lookup r ctx)
typecheckExpr e@(NameE n@(name :@ _) ts) p _ = do
  {-
           l : τ ∈ ΞD
  ────────────────────────────── data label
     Ξ; Γ; χ; σ; ε ⊢ᵀ l : *τ

     l : ∀(v).ζ ∈ ΞC      Γ ⊢ᴷ s₁ : k₁, s₂ : k₂, … sₚ : kₚ     s = { s₁ : k₁, s₂ : k₂, …, sₚ : kₚ }     s ∼ v
  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────── code label
                                     Ξ; Γ; χ; σ; ε ⊢ᵀ l<s> : ∀().ζ
  -}
  xiD <- gets (xiD . snd)
  xiC <- gets (xiC . snd)
  ty <- maybe (throwError (UnknownDataLabel name p)) pure (Env.lookup n xiD <|> Env.lookup n xiC)

  case ty of
    -- > l : ∀(v).ζ ∈ ΞC
    ForAllT binds t :@ p2 -> do
      let n = length ts
          p' = length binds

      -- > Γ ⊢ᴷ s₁ : k₁, s₂ : k₂, … sₚ : kₚ
      -- > s = { s₁ : k₁, s₂ : k₂, …, sₚ : kₚ }
      -- > s ∼ v
      case p' `compare` n of
        LT -> throwError (TooMuchSpecialization n p' p)
        GT -> throwError (CannotInferSpecialization n p' p)
        EQ -> pure ()

      g <- gets (gamma . snd)
      specKinds <- forM ts \t -> do
        liftEither $ first FromReport (runKindchecker (kindcheckType g t))
      let kindsToUnify = zip (snd <$> binds) specKinds
      forM_ kindsToUnify \(k1, k2) -> do
        liftEither $ first FromReport (runKindchecker (unifyKinds k1 k2))

      let sub = Subst (Map.fromList (zip (fromVar . fst <$> binds) ts))

      -- > Ξ; Γ; χ; σ; ε ⊢ᵀ l<s> : ∀().ζ
      pure (apply sub (relax $ ForAllT [] t :@ p), e :@ p)
    -- > l : τ ∈ ΞD
    _ -> do
      when (not $ null ts) do
        throwError (TooMuchSpecialization (length ts) 0 p)

      -- > Ξ; Γ; χ; σ; ε ⊢ᵀ l : *τ
      pure (ty, e :@ p)
  where
    fromVar (VarT n :@ _) = n
    fromVar t = internalError $ "Cannot get name of non type-variable " <> show t
typecheckExpr e@(ByteOffsetE off ptr) p unsafe = do
  {-
      Ξ; Γ; χ; σ; ε ⊢ᵀ p : *τ       Ξ; Γ; χ; σ; ε ⊢ᵀ o : s64
  ─────────────────────────────────────────────────────────────── pointer byte-offset
                Ξ; Γ; χ; σ; ε ⊢ᵀ o(p) : *τ
  -}
  case unLoc off of
    ImmE (I 0 :@ _) -> pure ()
    _ -> do
      when (not unsafe) do
        throwError (UnsafeOperationOutOfUnsafeBlock p)

  -- > Ξ; Γ; χ; σ; ε ⊢ᵀ o : s64
  (ty1, _) <- typecheckExpr (unLoc off) (getPos off) unsafe
  unify ty1 (SignedT 64 :@ p)

  -- > Ξ; Γ; χ; σ; ε ⊢ᵀ p : *τ
  (ty2, _) <- typecheckExpr (unLoc ptr) (getPos ptr) unsafe
  t <- freshVar "τ" (getPos ptr)
  unify (PtrT t :@ getPos ptr) ty2

  -- > Ξ; Γ; χ; σ; ε ⊢ᵀ o(p) : *τ
  pure (ty2, e :@ p)
typecheckExpr (BaseOffsetE ptr off) p unsafe = do
  {-
      Ξ; Γ; χ; σ; ε ⊢ᵀ p : *τ       Ξ; Γ; χ; σ; ε ⊢ᵀ o : s64
  ─────────────────────────────────────────────────────────────── pointer base-offset
                Ξ; Γ; χ; σ; ε ⊢ᵀ p[o] : *τ

     Γ ⊢ᴷ t₀ : Tn₀, t₁ : Tn₁, …, tₘ : Tnₘ      n₀, n₁, …, nₘ ∈ ℕ
                Ξ; Γ; χ; σ; ε ⊢ᵀ p : *(t₀, t₁, …, tₘ)
  ────────────────────────────────────────────────────────────────── structure pointer offset
                  Ξ; Γ; χ; σ; ε ⊢ᵀ p[nᵢ] : *tᵢ
  -}
  -- > Ξ; Γ; χ; σ; ε ⊢ᵀ o : s64
  (ty1, _) <- typecheckExpr (unLoc off) (getPos off) unsafe
  unify ty1 (SignedT 64 :@ p)

  -- > Ξ; Γ; χ; σ; ε ⊢ᵀ p : *τ
  -- ──────────────────────────
  -- > Ξ; Γ; χ; σ; ε ⊢ᵀ p : *(t₀, t₁, …, tₘ)
  (ty2, _) <- typecheckExpr (unLoc ptr) (getPos ptr) unsafe
  t <- freshVar "τ" (getPos ptr)
  sub <- unify (PtrT t :@ getPos ptr) ty2

  (ty2, off2) <- case (unLoc off, apply sub t) of
    (ImmE (I o :@ _), PackedStructT ts :@ _) -> do
      g <- gets (gamma . snd)

      memberSizes <- liftEither $
        first FromReport $ runKindchecker do
          mapM ((sizeof =<<) . kindcheckType g) ts

      let s = fromIntegral $ length memberSizes
      when (o > s - 1) do
        throwError (OutOfBoundsStructureAccess o s p)

      let ty = ts !! fromIntegral o
          offset = sum $ take (fromIntegral o) memberSizes

      pure (PtrT ty :@ p, ImmE (I offset :@ p))
    (o, PackedStructT ts :@ p1) -> do
      throwError (StructureOffsetMustBeKnown p (getPos ptr) (getPos off))
    (ImmE (I o :@ _), _) -> do
      g <- gets (gamma . snd)
      m <- liftEither $
        first FromReport $ runKindchecker do
          sizeof =<< kindcheckType g (apply sub t)
      pure (ty2, ImmE (I (o * m) :@ p))
    (RegE r, _) -> do
      when (not unsafe) do
        throwError (UnsafeOperationOutOfUnsafeBlock p)
      pure (ty2, RegE r)
    o -> internalError $ "Invalid pointer offset " <> show o

  -- > Ξ; Γ; χ; σ; ε ⊢ᵀ p[o] : *τ
  pure (ty2, ByteOffsetE (off2 :@ getPos off) ptr :@ p)
typecheckExpr e p _ = error $ "Unimplemented `typecheckExpr` for '" <> show e <> "'."

typecheckConstant :: (?tcFlags :: TypecheckerFlags) => Constant -> Position -> Typechecker (Located Type)
typecheckConstant (IntegerC _) p =
  {-
      i is an integer immediate
  ────────────────────────────────
      Ξ; Γ; χ; σ; ε ⊢ᵀ i : u64
  -}
  pure (UnsignedT 64 :@ p)
typecheckConstant (CharacterC _) p =
  {-
     c is a character immediate
  ─────────────────────────────────
      Ξ; Γ; χ; σ; ε ⊢ᵀ c : s8
  -}
  pure (SignedT 8 :@ p)
typecheckConstant (ArrayC csts) p =
  {-
      n ∈ ℕ       Γ ⊢ᴷ τ : Tn       Γ ⊢ᵀ c₁, c₂, …, cₙ : τ
  ────────────────────────────────────────────────────────────
                  Γ ⊢ᵀ [c₁, c₂, …, cₙ] : *τ
  -}
  if
      | (c1 : cs) <- csts -> do
        -- > Γ ⊢ᴷ τ : Tn
        -- > Γ ⊢ᵀ c₁, c₂, …, cₙ : τ
        types <- forM csts \(c :@ p) -> typecheckConstant c p
        let (t1 : ts) = types
        when (not (null ts)) do
          () <$ foldlM2 (\acc t1 t2 -> mappend acc <$> unify t1 t2) mempty (t1 : ts)

        -- > Γ ⊢ᵀ [c₁, c₂, …, cₙ] : *τ
        pure t1
      | otherwise -> do
        -- > Γ ⊢ᴷ τ : Tn
        v <- freshVar "d" p

        -- > Γ ⊢ᵀ [] : *τ
        pure v
  where
    foldlM2 :: (Monad m) => (b -> a -> a -> m b) -> b -> [a] -> m b
    foldlM2 f e l = foldlM (uncurry . f) e (zip l (drop 1 l))
typecheckConstant (StructC csts) p =
  {-
         Γ ⊢ᵀ c₁ : τ₁, c₂ : τ₂, …, cₚ : τₚ
  ──────────────────────────────────────────────
      Γ ⊢ᵀ (c₁, c₂, …, cₚ) : (τ₁, τ₂, …, τₚ)
  -}
  (:@ p) . PackedStructT <$> mapM (\(c :@ p) -> typecheckConstant c p) csts

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
    let k1 = Map.keysSet (Map.filter (/= (BangT :@ p1)) m1)
        k2 = Map.keysSet (Map.filter (/= (BangT :@ p2)) m2)
    let common = k1 `Set.intersection` k2
        other1 = k1 Set.\\ common
        other2 = k2 Set.\\ common

    let notBang p m r = maybe True ((/= (BangT :@ p))) (Map.lookup r m)

        computeExtension :: Bool -> [Located Register] -> Map (Located Register) (Located Type) -> Position -> Typechecker (Subst (Located Type), Map (Located Register) (Located Type))
        computeExtension _ [] _ _ = pure mempty
        computeExtension False fs m p
          | not (null (filter (notBang p m) fs)) = throwError (MissingRegistersInContext (unLoc <$> fs) p)
        computeExtension _ fs m p = do
          let newFields = Map.fromList (fs <&> \k -> (k, m Map.! k))
          pure (mempty, newFields)

    subs <- fold <$> forM (Set.toList common) \k -> unify (m1 Map.! k) (m2 Map.! k)
    (sub1, _) <- computeExtension o2 (Set.toList other1) m2 p2
    (sub2, _) <- computeExtension o1 (Set.toList other2) m1 p1

    sub3 <- unify s1 s2
    sub4 <- unify c1 c2

    pure (sub1 <> sub2 <> sub3 <> sub4 <> subs)
  -- A stack constructor can be unified to another stack constructor if
  -- both stack head and stack tail of each stack can be unified.
  (ConsT t1 t3, ConsT t2 t4) -> unifyMany [t1, t3] [t2, t4]
  (ForAllT [] r1, ForAllT [] r2) -> unify r1 r2
  (BangT, _) -> pure mempty
  (_, BangT) -> pure mempty
  -- Any other combination is not possible
  _ -> throwError (Uncoercible (t1 :@ p1) (t2 :@ p2))

-- | Unifies many types, yielding the composition of all the substitutions created.
unifyMany :: (?tcFlags :: TypecheckerFlags) => (t ~ Located Type) => [t] -> [t] -> Typechecker (Subst t)
unifyMany [] [] = pure mempty
unifyMany (x : xs) (y : ys) = do
  sub1 <- unify x y
  sub2 <- unifyMany (apply sub1 xs) (apply sub1 ys)
  pure (sub1 <> sub2)
unifyMany l r =
  error ("Could not unify " <> show l <> " and " <> show r <> " because they aren't of the same size.")

-- | Tries to bind a free type variable to a type, yielding a substitution from the variable to the type if
--   it succeeded.
bind :: (?tcFlags :: TypecheckerFlags) => (Located Text, Position) -> (Type, Position) -> Typechecker (Subst (Located Type))
bind (var, p1) (FVarT v, p2)
  | var == v = pure mempty
bind (var, p1) (ty, p2)
  | occursCheck var ty = throwError (InfiniteType (ty :@ p2) var)
  | otherwise = pure (Subst (Map.singleton var (ty :@ p2)))
  where
    occursCheck v t = v `Set.member` freeVars t

relax :: Located Type -> Located Type
relax (t :@ p) = relaxType t :@ p
  where
    relaxType (ConsT t1 t2) = ConsT (relax t1) (relax t2)
    relaxType (VarT n) = FVarT n
    relaxType (RecordT ms s c o) = RecordT (relax <$> ms) (relax s) (relax c) o
    relaxType (PtrT t) = PtrT (relax t)
    relaxType (ForAllT b t) = ForAllT b (relax t)
    -- NOTE: This will probably need to be tweaked
    --       Because we don't rename the variables bound, any flexible and rigid variable can be confused.
    relaxType t = t

close :: Type -> Type
close (ForAllT b t) = ForAllT b (close <$> t)
close (RecordT x s e _) = RecordT x s e False
close t = t
