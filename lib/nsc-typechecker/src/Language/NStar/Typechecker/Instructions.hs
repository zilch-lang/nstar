{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Language.NStar.Typechecker.Instructions where

import Language.NStar.Typechecker.Errors
import Language.NStar.Typechecker.TC
import Language.NStar.Typechecker.Free
import Language.NStar.Typechecker.Subst
import Language.NStar.Typechecker.Core
import Language.NStar.Syntax.Core (Expr(..), Immediate(..))
import Data.Located (Located(..), unLoc, getPos, Position)
import qualified Data.Map as Map
import Control.Monad.State (gets)
import Control.Monad.Except (liftEither, throwError, catchError)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromJust)
import Data.Foldable (fold)
import Console.NStar.Flags (TypecheckerFlags(..))
import Internal.Error (internalError)
import qualified Language.NStar.Typechecker.Env as Env (lookup)
import Control.Monad (forM)
import Data.Bifunctor (first)
import Language.NStar.Typechecker.Kinds (unifyKinds, kindcheckType, runKindchecker)
import Control.Monad (forM_)
import Data.Functor ((<&>))

tc_ret :: (?tcFlags :: TypecheckerFlags) => Position -> Typechecker [Located Type]
tc_ret p = do
  -- `reŧ` needs the return address on top of the stack.
  -- Any remainding register values are left to the developer to choose.
  --
  -- So when a `reŧ` comes, the stack (so `%rsp` on x64) needs to be
  -- `sptr *r0::s0` where `r0` is a record.
  --
  --
  -- According to the x86 docs, we could also make `ret` take one parameter:
  -- a number of bytes to pop off the stack, after popping the return address off.
  -- While this may be useful, I won't handle this because it won't probably
  -- be useful in N*, as a language backend.

  -- if the unification suceeded, we know that there is a pointer to some sort of context
  -- on top of the stack, which we may pop and compare to the current context.

  -- TODO: check that when can return
  -- 1- we must have crossed a label at some point. We will take the last found
  -- 2- we must be able to extract a pointer to a context, on top of the stack
  -- 3- everything we want to return in the context must already be found in the current context

  funName <- gets (currentLabel . snd) >>= \case
    Nothing -> throwError (ToplevelReturn p)
    Just l  -> pure l


  stackVar <- freshVar "@" p
  let minimalCtx = Record (Map.singleton (SP :@ p) (SPtr (Cons (Ptr (Record mempty True :@ p) :@ p) stackVar :@ p) :@ p)) True :@ p
  currentCtx <- gets (currentTypeContext . snd)

  catchError (unify (Record currentCtx False :@ p) minimalCtx)
             (const $ throwError (NoReturnAddress p currentCtx))

  let removeStackTop (SPtr (Cons _ t :@ _) :@ p1) = SPtr t :@ p1
      removeStackTop (t :@ _)                     = error $ "Cannot extract stack top of type '" <> show t <> "'"

      getStackTop (SPtr (Cons t _ :@ _) :@ _) = t
      getStackTop (t :@ _)                    = error $ "Cannot extract stack top of type '" <> show t <> "'"

  let returnShouldBe = Ptr (Record (Map.adjust removeStackTop (SP :@ p) currentCtx) False :@ p) :@ p
      returnCtx = getStackTop $ fromJust (Map.lookup (SP :@ p) currentCtx)

  let changeErrorIfMissingKey (DomainsDoNotSubtype (m1 :@ _) (m2 :@ _)) =
        ContextIsMissingOnReturn p (getPos returnCtx) (Map.keysSet m1 Set.\\ Map.keysSet m2)
      changeErrorIfMissingKey (MissingRegistersInContext rs p)          =
        ContextIsMissingOnReturn p (getPos returnCtx) (Set.fromList ((:@ p) <$> rs))
      changeErrorIfMissingKey e                                         = e

  catchError (unify returnCtx returnShouldBe)
             (throwError . changeErrorIfMissingKey)

  pure []


tc_mov :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Located Expr -> Bool -> Position -> Typechecker [Located Type]
tc_mov (src :@ p1) (dest :@ p2) unsafe p = do
  -- There are many ways of handling `mov`s, and it all depends on what arguments are given.
  --
  -- > mov <immediate>, <register>
  -- sets "<register>: typeof (<immediate>)" in the current context
  -- > mov <register2>, <register1>
  -- sets "<register1>: typeof (<register2>)" in the current context
  -- > mov <immediate>, (<address:type>)
  -- TODO: UNSAFE ???
  -- > mov (<address:type>), <register>
  -- UNSAFE sets "<register>: <type>" in the current context
  -- > mov (<address2:type2>), (<address1:type1>)
  -- TODO: UNSAFE ???
  -- > mov <immediate>, <offset>(<register>)
  -- UNSAFE assert that `<register>: *typeof (<immediate>)` and leave the context untouched
  -- > mov <register2>, <offset>(<register1>)
  -- UNSAFE assert that `<register1>: *typeof (<register2>)` and leave the context untouched
  --
  --
  -- For all those assertions, we also have to check that type sizes do match.
  case (src, dest) of
    (Imm i, Reg r) -> do
      ty <- typecheckExpr src p1
      -- TODO: size check
      -- At the moment, this isn't a problem: we only handle immediates that are actually
      -- smaller than the max size (8 bytes) possible.
      gets (currentTypeContext . snd) >>= setCurrentTypeContext . Map.insert r ty
      pure [ty, Register 64 :@ p2] -- TODO: fetch actual size of register
    (Reg r1, Reg r2) -> do
      ty1 <- typecheckExpr src p1
      -- TODO: size check
      -- No need at the moment, we only have 8-bytes big registers.
      gets (currentTypeContext . snd) >>= setCurrentTypeContext . Map.insert r2 ty1
      pure [Register 64 :@ p1, Register 64 :@ p2] -- TODO: fetch actual size of register
    _ -> error $ "Missing `mov` typechecking implementation for '" <> show src <> "' and '" <> show dest <> "'."

tc_jmp :: (?tcFlags :: TypecheckerFlags) => Located Expr -> [Located Type] -> Position -> Typechecker [Located Type]
tc_jmp (Name n :@ p1) tys p = do
  -- Preconditions to check before jumping:
  --
  -- - the context we want to jump to shares a subset of the current context.
  --   so essentially the condition is: @newContext ⊆ currentContext@
  -- - the address we want to jump too (via a label, at least for now) has to exist in the scope
  --   (the current file or one of the imported files, unless the symbols is marked as "dynamic").
  -- - type applications must hold (most of it is basic kind checking to see if the types are actually in
  --   valid places).
  --   so we have to be able to unify the given specialisation and a relaxed version of the target context where
  --   type variables are substituted (we really need to check that the kind of the type variable at position N
  --   unifies with the kind of the specialisation at the index N).

  typeEnv <- gets (typeEnvironment . snd)
  labelCtx <- maybe (throwError (UnknownLabel n)) pure (Env.lookup n typeEnv)
  let (typeVars, ctx) = fetchRigidTypevars labelCtx
      nbOfSpec        = length tys
      nbOfTVars       = length typeVars
  case nbOfSpec `compare` nbOfTVars of
    LT -> throwError (CannotInferSpecialization nbOfSpec nbOfTVars p)
    GT -> throwError (TooMuchSpecialization nbOfSpec nbOfTVars p)
    EQ -> pure ()

  kindCtx <- gets (currentKindContext . snd)
  specKinds <- forM tys \ ty -> do
    liftEither $ first FromReport (runKindchecker (kindcheckType kindCtx ty))

  let toUnify = zip (snd <$> typeVars) specKinds
  forM_ toUnify \ (k1, k2) ->
    liftEither $ first FromReport (runKindchecker (unifyKinds k1 k2))

  let sub = Subst (Map.fromList (zip (fromVar . fst <$> typeVars) tys))
  typeCtx <- gets (currentTypeContext . snd)
  catchError (unify (sub `apply` relax ctx) (Record typeCtx False :@ p))
             (throwError . CannotJumpBecauseOf p)

  pure []
  where
    fetchRigidTypevars (ForAll binds ty :@ _) = (binds, ty)
    fetchRigidTypevars ty                     = ([], ty)

    fromVar (Var n :@ _) = n
    fromVar t            = internalError $ "Cannot get name of non type-variable " <> show t
tc_jmp (t :@ p1) tys p = internalError $ "Cannot handle jump to non-label expression " <> show t

tc_call :: (?tcFlags :: TypecheckerFlags) => Located Expr -> [Located Type] -> Position -> Typechecker [Located Type]
tc_call (Name n :@ p1) tys p = do
  -- Typechecking a @call@ instruction is a little bit harder than typechecking a @jmp@ instruction.
  -- Reason is that there are more preconditions, and there are also postconditions.
  --
  -- \* Preconditions:
  --
  -- - the jump context is a subset of the current context (modulo specialization).
  -- - the label we jump to exists (so it's in the current file or statically/dynamically bound).
  -- - type application kinds have to be able to be unified with those of the target context, and the current context
  --   has to be a subset of the target specialized context.
  -- - the target label has to have a return context on top of its stack (@%rsp@, @%esp@ or anything else)
  --   or abstract it away through specialization.
  --
  -- \* Postconditions:
  --
  -- - the new current context is the context found at the top of the specialized target context's stack.
  -- - the new context should have the stack set to the stack in the specialized context, stripped off the
  --   top return address (context pointer).

  -- check all the jump preconditions

  typeEnv <- gets (typeEnvironment . snd)
  labelCtx <- maybe (throwError (UnknownLabel n)) pure (Env.lookup n typeEnv)
  let (typeVars, ctx) = fetchRigidTypevars labelCtx
      nbOfSpec        = length tys
      nbOfTVars       = length typeVars
  case nbOfSpec `compare` nbOfTVars of
    LT -> throwError (CannotInferSpecialization nbOfSpec nbOfTVars p)
    GT -> throwError (TooMuchSpecialization nbOfSpec nbOfTVars p)
    EQ -> pure ()

  kindCtx <- gets (currentKindContext . snd)
  specKinds <- forM tys \ ty -> do
    liftEither $ first FromReport (runKindchecker (kindcheckType kindCtx ty))

  let toUnify = zip (snd <$> typeVars) specKinds
  forM_ toUnify \ (k1, k2) ->
    liftEither $ first FromReport (runKindchecker (unifyKinds k1 k2))

  let sub = Subst (Map.fromList (zip (fromVar . fst <$> typeVars) tys))
  typeCtx <- gets (currentTypeContext . snd)
  let ctxTy = sub `apply` relax ctx

  let addContextOnTop Nothing   = Just (SPtr (Ptr (Record mempty True :@ p) :@ p) :@ p)
      addContextOnTop (Just ty) = case ty of
        SPtr stack :@ p3 -> Just $ SPtr (Cons (Ptr (Record mempty True :@ p) :@ p) stack :@ p) :@ p3
        t :@ _           -> internalError $ "Trying to add a return context on top of a non-stack type " <> show t
  let newTypeCtx = Map.alter addContextOnTop (SP :@ p) typeCtx
  let targetCtx = Record newTypeCtx False :@ p

  catchError (unify ctxTy targetCtx)
             (throwError . CannotJumpBecauseOf p)

  -- check all postconditions

  let popContextOffStack (SPtr (Cons ctx _ :@ _) :@ _) = ctx
      popContextOffStack t                             = internalError $ "Cannot pop stack or non-stack type " <> show t

      Record ctx _ :@ _               = ctxTy
      Ptr (Record newCtx _ :@ _) :@ _ = popContextOffStack (fromJust $ Map.lookup (SP :@ p) ctx)

  setCurrentTypeContext newCtx

  pure []
  where
    fetchRigidTypevars (ForAll binds ty :@ _) = (binds, ty)
    fetchRigidTypevars ty                     = ([], ty)

    fromVar (Var n :@ _) = n
    fromVar t            = internalError $ "Cannot get name of non type-variable " <> show t
tc_call (t :@ p1) ty sp = internalError $ "Cannot handle call to non-label expression " <> show t

---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------

typecheckExpr :: (?tcFlags :: TypecheckerFlags) => Expr -> Position -> Typechecker (Located Type)
typecheckExpr (Imm (I _ :@ _)) p  = pure (Unsigned 64 :@ p)
typecheckExpr (Imm (C _ :@ _)) p  = pure (Signed 8 :@ p)
typecheckExpr (Reg r) p           = do
  ctx <- gets (currentTypeContext . snd)
  maybe (throwError (RegisterNotFoundInContext (unLoc r) p (Map.keysSet ctx))) pure (Map.lookup r ctx)
typecheckExpr (Indexed _ e) p     = typecheckExpr (unLoc e) p
typecheckExpr e p                 = error $ "Unimplemented `typecheckExpr` for '" <> show e <> "'."

--------------------------------------------------------------------------------

-- | Generates a fresh free type variable based on a given prefix, for the current source position.
freshVar :: (?tcFlags :: TypecheckerFlags) => Text -> Position -> Typechecker (Located Type)
freshVar prefix pos = do
  n <- gets fst
  incrementCounter
  pure (FVar ((prefix <> Text.pack (show n)) :@ pos) :@ pos)

-- | Unifies two types, and returns the substitution from the first to the second.
unify :: (?tcFlags :: TypecheckerFlags) => (t ~ Located Type) => t -> t -> Typechecker (Subst t)
unify (t1 :@ p1) (t2 :@ p2) = case (t1, t2) of
  _ | t1 == t2 -> pure mempty
  -- Signed types are all coercible.
  (Signed _, Signed _) -> pure mempty
  -- Unsigned types are all coercible.
  (Unsigned _, Unsigned _) -> pure mempty
  -- Signed and unsigned integers can also be coerced to each other.
  (Signed _, Unsigned _) -> pure mempty
  (Unsigned _, Signed _) -> pure mempty
  -- Two pointers are coercible if their pointed types are coercible.
  (Ptr t1, Ptr t2) -> unify t1 t2
  (SPtr t1, SPtr t2) -> unify t1 t2
  -- Free type variables can be bound to anything as long as it does not create an infinite type.
  (FVar v, t) -> bind (v, p1) (t, p2)
  (t, FVar v) -> bind (v, p2) (t, p1)
  -- Contexts are coercible if the intersection of their registers are all coercible.
  (Record m1 o1, Record m2 o2) -> do
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

    pure (sub1 <> sub2 <> subs)
  -- A stack constructor can be unified to another stack constructor if
  -- both stack head and stack tail of each stack can be unified.
  (Cons t1 t3, Cons t2 t4) -> unifyMany [t1, t3] [t2, t4]
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
bind (var, p1) (FVar v, p2)
  | var == v              = pure mempty
bind (var, p1) (ty, p2)
  | occursCheck var ty    = throwError (InfiniteType (ty :@ p2) var)
  | otherwise             = pure (Subst (Map.singleton var (ty :@ p2)))
 where
   occursCheck v t = v `Set.member` freeVars t

relax :: Located Type -> Located Type
relax (t :@ p) = relaxType t :@ p
  where
    relaxType (Cons t1 t2)  = Cons (relax t1) (relax t2)
    relaxType (Var n)       = FVar n
    relaxType (Record ms o) = Record (relax <$> ms) o
    relaxType (Ptr t)       = Ptr (relax t)
    relaxType (SPtr t)      = SPtr (relax t)
    relaxType (ForAll _ t)  = unLoc $ relax t
    relaxType t             = t
