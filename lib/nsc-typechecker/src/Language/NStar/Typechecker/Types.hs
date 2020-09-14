{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-|
  Module: Language.NStar.Typechecker.Types
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Typechecker.Types
( typecheck ) where

import Control.Monad.State
import Language.NStar.Typechecker.Core
import Language.NStar.Syntax.Core hiding (Token(..))
import Language.NStar.Typechecker.Env (Env)
import qualified Language.NStar.Typechecker.Env as Env
import Data.Located
import Data.Bifunctor (first, second)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import Control.Monad.Except
import Text.Diagnose hiding (Kind)
import Language.NStar.Typechecker.Subst
import Language.NStar.Typechecker.Errors
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Language.NStar.Typechecker.Free
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List (union)
import Data.Foldable (fold)
import Debug.Trace (trace)
import Data.Maybe (fromJust)

type Typechecker a = StateT (Integer, Context) (WriterT [TypecheckError] (Except TypecheckError)) a

data TypecheckError
  = Uncoercible (Located Type) (Located Type)
  | NoReturnAddress Position (Map (Located Register) (Located Type))
  | InfiniteType (Located Type) (Located Text)
  | DomainsDoNotSubtype (Located (Map (Located Register) (Located Type))) (Located (Map (Located Register) (Located Type)))
  | RecordUnify TypecheckError (Located (Map (Located Register) (Located Type))) (Located (Map (Located Register) (Located Type)))
  | ToplevelReturn Position
  | ContextIsMissingOnReturn Position Position (Set (Located Register))

-- | The data type of contexts in typechecking.
data Context
  = Ctx
  { typeEnvironment :: Env Type                               -- ^ An 'Env'ironment containing labels associated to their expected contexts
  , kindEnvironment :: Env Kind                               -- ^ An 'Env'ironment keeping tracks of kinds of type variables
  , currentContext  :: Map (Located Register) (Located Type)  -- ^ The current typechecking context
  , currentLabel    :: Maybe (Located Text)                   -- ^ The last crossed label
  }

-- | Adds a kind to the environment.
addKind :: Located Text -> Located Kind -> Typechecker ()
addKind k v = modify $ second modifyKindContext
  where modifyKindContext ctx@Ctx{..} = ctx { kindEnvironment = Env.insert k v kindEnvironment }

-- | Adds a type to the environment.
addType :: Located Text -> Located Type -> Typechecker ()
addType k v = modify $ second modifyTypeContext
  where modifyTypeContext ctx@Ctx{..} = ctx { typeEnvironment = Env.insert k v typeEnvironment }

-- | Increments the counter in the 'State' by one, effectively simulating a @counter++@ operation.
incrementCounter :: Typechecker ()
incrementCounter = modify $ first (+ 1)

setCurrentContext :: Map (Located Register) (Located Type) -> Typechecker ()
setCurrentContext newCtx = modify $ second putContext
  where putContext ctx = ctx { currentContext = newCtx }

setKindEnvironment :: Env Kind -> Typechecker ()
setKindEnvironment newEnv = modify $ second setKindEnv
  where setKindEnv ctx = ctx { kindEnvironment = newEnv }

setTypeEnvironment :: Env Type -> Typechecker ()
setTypeEnvironment newEnv = modify $ second setTypeEnv
  where setTypeEnv ctx = ctx { typeEnvironment = newEnv }

setLabel :: Located Text -> Typechecker ()
setLabel n = modify $ second setLbl
  where setLbl ctx = ctx { currentLabel = Just n }

--------------------------------------------------------

-- | Runs the typechecker on a given program, returning either an error or a well-formed program.
typecheck :: Program -> Either (Diagnostic s String m) (Program, [Report String])
typecheck p = second (second $ fmap fromTypecheckError) $
              first toDiagnostic $ runExcept (runWriterT (evalStateT (typecheckProgram p) (0, Ctx mempty mempty mempty Nothing)))
  where toDiagnostic = (diagnostic <++>) . fromTypecheckError

-- | Transforms a typechcking error into a report.
fromTypecheckError :: TypecheckError -> Report String
fromTypecheckError (Uncoercible (t1 :@ p1) (t2 :@ p2))         = uncoercibleTypes (t1, p1) (t2, p2)
fromTypecheckError (InfiniteType (t :@ p1) (v :@ p2))          = infiniteType (t, p1) (v, p2)
fromTypecheckError (NoReturnAddress p ctx)                     = retWithoutReturnAddress p ctx
fromTypecheckError (DomainsDoNotSubtype (m1 :@ p1) (m2 :@ p2)) = recordDomainsDoNotSubset (m1, p1) (m2, p2)
fromTypecheckError (RecordUnify err (m1 :@ p1) (m2 :@ p2))     = fromTypecheckError err <> reportWarning "\n" [] [] <> recordValuesDoNotUnify (m1, p1) (m2, p2)
                                                                                   --      ^^^^^^^^^^^^^^^^^^^^^^^^
                                                                                   -- This is just to insert a newline between error
fromTypecheckError (ToplevelReturn p)                          = returnAtTopLevel p
fromTypecheckError (ContextIsMissingOnReturn p1 p2 regs)       = contextIsMissingOnReturnAt (Set.toList regs) p1 p2

--------------------------------------------------------

-- | Entry point of the typechecker.
--
--   Typechecks a program, and return an elaborated form of it.
typecheckProgram :: Program -> Typechecker Program
typecheckProgram p@(Program []) = pure p
typecheckProgram (Program stts) = do
  registerAllLabels stts
  res <- Program <$> mapM typecheckStatement stts

  -- Check that the type of `main` is actually usable, and that states
  -- can be guaranteed at compile time.
  -- So the only possible type will probably be `forall (s: Ts). { %rsp: sptr s }`.
  --
  -- A problem lies here:
  -- we cannot `ret` at the end of `main`, which is what all programs do (or `int`, which I find a bit dirtier)
  -- because `reŧ` needs a pointer to some state on top of the stack, which cannot be guaranteed from the
  -- above type. It also doesn't quite make sense to "return to something" when stopping a program.
  -- One could argue that we return to the caller, in order for them to stop the process and do some
  -- other stuff. Also, `ret` takes less space than `int`.
  -- But we have to figure out how to typecheck the return. Maybe we could do `forall (s: Ts). { %rsp: sptr *{}::s }`
  -- as a kind of "template" type, used only to guarantee type safety, but having no sort of meaning.
  -- Using gcc, `main` is `call`ed from a special `_start` procedure. So this would be our exit point
  -- (if we link against the special `crt0.o`, but we could use our own if we don't want to use the C runtime).
  --
  -- For more information, see this: <http://dbp-consulting.com/tutorials/debugging/linuxProgramStartup.html>

  pure res

-- | Brings all the labels with their corresponding types into the type environment.
--
--   This is useful for jumps which go to a label which is further down into the code, so that
--   we can still guarantee the type safety.
--
--   This unfortunately has to perform a complete lookup into the program before-hand,
--   worsening the overall complexity of the typechecking.
registerAllLabels :: [Located Statement] -> Typechecker ()
registerAllLabels = mapM_ addLabelType . filter isLabel
  where isLabel (unLoc -> Label _ _) = True
        isLabel _                    = False

        addLabelType (unLoc -> Label name ty) = addType name ty
        addLabelType (unLoc -> t)             = error ("Adding type of non-label '" <> show t <> "' is impossible.")

-- | Typechecks a statement.
--
--   When it's a label, simply override the current context with the label's type,
--   and add kind bindings of the @forall@ if there is one.
--
--   When it's an instruction, just typecheck the instruction accordingly.
typecheckStatement :: Located Statement -> Typechecker (Located Statement)
typecheckStatement s@(Label name ty :@ _) = do
  setCurrentContext (toRegisterMap (removeForallQuantifierIfAny ty))
  setKindEnvironment (Env.fromList (forallBindersIfAny ty))
  setLabel name
  pure s
 where
   removeForallQuantifierIfAny (unLoc -> ForAll _ ty) = ty
   removeForallQuantifierIfAny ty                     = ty

   forallBindersIfAny (unLoc -> ForAll binders _) = first getVarName <$> binders
   forallBindersIfAny _                           = []

   getVarName (unLoc -> Var v) = v
   getVarName (unLoc -> t)     = error $ "Cannot get type variable name from type '" <> show t <> "'"

   toRegisterMap (unLoc -> Record m) = m
   toRegisterMap (unLoc -> t)        = error $ "Cannot retrieve register mappings from non-record type '" <> show t <> "'"
typecheckStatement s@(Instr i :@ p)       = do
  typecheckInstruction i p
  pure s

typecheckInstruction :: Instruction -> Position -> Typechecker ()
typecheckInstruction i p = case i of
  RET -> do
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
    currentCtx <- gets (currentContext . snd)

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
    pure ()
  _ -> pure ()

--------------------------------------------------------------------------------

-- | Generates a fresh free type variable based on a given prefix, for the current source position.
freshVar :: Text -> Position -> Typechecker (Located Type)
freshVar prefix pos = do
  n <- gets fst
  incrementCounter
  pure (FVar ((prefix <> Text.pack (show n)) :@ pos) :@ pos)

-- | Unifies two types, and returns the substitution from the first to the second.
unify :: (t ~ Located Type) => t -> t -> Typechecker (Subst t)
unify (t1 :@ p1) (t2 :@ p2) = case (t1, t2) of
  _ | t1 == t2 -> pure mempty
  -- Signed types are all coercible.
  (Signed _, Signed _) -> pure mempty
  -- Unsigned types are all coercible.
  (Unsigned _, Unsigned _) -> pure mempty
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
unifyMany :: (t ~ Located Type) => [t] -> [t] -> Typechecker (Subst t)
unifyMany [] []         = pure mempty
unifyMany (x:xs) (y:ys) = do
  sub1 <- unify x y
  sub2 <- unifyMany (apply sub1 xs) (apply sub1 ys)
  pure (sub1 <> sub2)
unifyMany l r           =
  error ("Could not unify " <> show l <> " and " <> show r <> " because they aren't of the same size.")

-- | Tries to bind a free type variable to a type, yielding a substitution from the variable to the type if
--   it succeeded.
bind :: (Located Text, Position) -> (Type, Position) -> Typechecker (Subst (Located Type))
bind (var, p1) (FVar v, p2)
  | var == v              = pure mempty
bind (var, p1) (ty, p2)
  | occursCheck var ty    = throwError (InfiniteType (ty :@ p2) var)
  | otherwise             = pure (Subst (Map.singleton var (ty :@ p2)))
 where
   occursCheck v t = v `Set.member` freeVars t
