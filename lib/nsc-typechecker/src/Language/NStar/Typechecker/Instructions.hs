{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad.Except (throwError, catchError)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromJust)
import Data.Foldable (fold)
import Console.NStar.Flags (TypecheckerFlags(..))

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
  let minimalCtx = Record (Map.singleton (RSP :@ p) (SPtr (Cons (Ptr (Record mempty :@ p) :@ p) stackVar :@ p) :@ p)) :@ p
  currentCtx <- gets (currentTypeContext . snd)

  catchError (unify minimalCtx (Record currentCtx :@ p))
             (const $ throwError (NoReturnAddress p currentCtx))


  let removeStackTop (SPtr (Cons _ t :@ _) :@ p1) = SPtr t :@ p1
      removeStackTop (t :@ _)                     = error $ "Cannot extract stack top of type '" <> show t <> "'"

      getStackTop (SPtr (Cons t _ :@ _) :@ _) = t
      getStackTop (t :@ _)                    = error $ "Cannot extract stack top of type '" <> show t <> "'"

  let returnShouldBe = Ptr (Record (Map.adjust removeStackTop (RSP :@ p) currentCtx) :@ p) :@ p
      returnCtx = getStackTop $ fromJust (Map.lookup (RSP :@ p) currentCtx)

  let changeErrorIfMissingKey (DomainsDoNotSubtype (m1 :@ _) (m2 :@ _)) =
        ContextIsMissingOnReturn p (getPos returnCtx) (Map.keysSet m1 Set.\\ Map.keysSet m2)
      changeErrorIfMissingKey e                                         = e

  catchError (unify returnCtx returnShouldBe)
             (throwError . changeErrorIfMissingKey)
  pure []


tc_mov :: (?tcFlags :: TypecheckerFlags) => Located Expr -> Located Expr -> Position -> Typechecker [Located Type]
tc_mov (src :@ p1) (dest :@ p2) p = do
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
tc_jmp (to :@ p1) tys p = do




  pure []

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
  -- FIXME: Handle records properly
  (Record m1, Record m2) -> do
    -- @m2@ should contain at least all the keys in @m1@:
    let k1 = Map.keysSet m1
        k2 = Map.keysSet m2
    if not (k1 `Set.isSubsetOf` k2)
    then throwError (DomainsDoNotSubtype (m1 :@ p1) (m2 :@ p2))
    else do
      -- All the values from the keys of @m1@ in @m2@ should be 'unify'able with the values in @m1@:
      let doUnify k v = catchError (unify v (m2 Map.! k)) (\ e -> throwError $ RecordUnify e (m1 :@ p1) (m2 :@ p2))
      subs <- fold <$> Map.traverseWithKey doUnify m1
      pure subs
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
