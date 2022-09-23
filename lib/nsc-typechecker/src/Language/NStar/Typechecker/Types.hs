{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
--  Module: Language.NStar.Typechecker.Types
--  Copyright: (c) Mesabloo, 2020
--  License: BSD3
--  Stability: experimental
module Language.NStar.Typechecker.Types (typecheck) where

import Console.NStar.Flags (TypecheckerFlags (..))
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor (bimap, first, second)
import Data.Foldable (foldl')
import Data.Located
import qualified Data.Map as Map
import Error.Diagnose (Diagnostic, addReport, def)
import Language.NStar.Syntax.Core hiding (Instruction (..), Token (..))
import qualified Language.NStar.Syntax.Core as SC (Instruction (..))
import Language.NStar.Typechecker.Core
import qualified Language.NStar.Typechecker.Env as Env
import Language.NStar.Typechecker.Errors
import Language.NStar.Typechecker.Instructions
import Language.NStar.Typechecker.Kinds (kindcheck)
import Language.NStar.Typechecker.TC

-- | Runs the typechecker on a given program, returning either an error or a well-formed program.
typecheck :: (?tcFlags :: TypecheckerFlags) => Program -> Either (Diagnostic String) (TypedProgram, Diagnostic String)
typecheck p = bimap toDiagnostic (second toDiagnostic') $ runExcept (runWriterT (evalStateT (typecheckProgram p) mempty))
  where
    toDiagnostic = addReport def . fromTypecheckError
    toDiagnostic' = foldl' addReport def . fmap fromTypecheckWarning

--------------------------------------------------------

-- | Entry point of the typechecker.
--
--   Typechecks a program, and return an elaborated form of it.
typecheckProgram :: (?tcFlags :: TypecheckerFlags) => Program -> TC TypedProgram
typecheckProgram (Program [DataS dataSect :@ p1, RODataS rodataSect :@ p2, UDataS udataSect :@ p3, CodeS code :@ p4, ExternCodeS ecode :@ p5]) = do
  registerAllLabels code
  registerAllDataLabels dataSect
  registerAllExternCodeLabels ecode

  instrs <- concat <$> mapM (typecheckStatement) code

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

  pure (TProgram (TData dataSect :@ p1) (TROData [] :@ p2) (TUData [] :@ p3) (TCode instrs :@ p4) (TExternCode ecode :@ p5))
typecheckProgram (Program _) = error "Unexpected invalid Program"

-- | Brings all the labels with their corresponding types into the type environment.
--
--   This is useful for jumps which go to a label which is further down into the code, so that
--   we can still guarantee the type safety.
--
--   This unfortunately has to perform a complete lookup into the program before-hand,
--   worsening the overall complexity of the typechecking.
registerAllLabels :: (?tcFlags :: TypecheckerFlags) => [Located Statement] -> TC ()
registerAllLabels = mapM_ addLabelType
  where
    addLabelType (unLoc -> Label name ty _) = liftEither (first FromReport $ kindcheck ty) *> addType name ty
    addLabelType (unLoc -> t) = error ("Adding type of non-label '" <> show t <> "' is impossible.")

-- | Brings all un-initialized and extern code labels into the type environnement.
--
--   This allows to jump to currently undefined labels of some type (pre-conditions).
registerAllExternCodeLabels :: (?tcFlags :: TypecheckerFlags) => [Located ReservedSpace] -> TC ()
registerAllExternCodeLabels = mapM_ addLabelType
  where
    addLabelType (unLoc -> ReservedBind name ty) = liftEither (first FromReport $ kindcheck ty) *> addType name ty

-- | Brings all the data labels with their current types into the context.
--
--   This allows to access labels that are not in the @code@ section when typechecking instructions.
registerAllDataLabels :: (?tcFlags :: TypecheckerFlags) => [Located Binding] -> TC ()
registerAllDataLabels = mapM_ addBinding
  where
    addBinding (unLoc -> Bind name ty (val :@ p)) = do
      TCCtx xiC xiD <- get
      let __unusedSigma = VarT ("@__s" :@ p) :@ p
          __unusedEpsilon = VarT ("@__e" :@ p) :@ p

      liftEither (first FromReport $ kindcheck ty)
      lift $ evalStateT (unify ty . ((:@ p) . PtrT) =<< typecheckConstant val p) (0, Ctx xiC xiD mempty mempty __unusedSigma __unusedEpsilon)
      addDataLabel name (ty)

-- | Typechecks a statement.
--
--   When it's a label, simply override the current context with the label's type,
--   and add kind bindings of the @forall@ if there is one.
--
--   When it's an instruction, just typecheck the instruction accordingly.
typecheckStatement :: (?tcFlags :: TypecheckerFlags) => Located Statement -> TC [Located TypedStatement]
typecheckStatement (Label name ty is :@ p) = do
  TCCtx xiC xiD <- get
  let (binders, RecordT chi sigma epsilon _ :@ _) = removeForallQuantifierIfAny ty
  let gamma = Env.fromList (first toVarName <$> binders)

  typed <- lift $
    flip evalStateT (0, Ctx xiC xiD gamma chi sigma epsilon) $
      forM is \(i :@ p1, isUnsafe) -> do
        typecheckInstruction i p1 isUnsafe

  pure [TLabel name typed :@ p]
  where
    removeForallQuantifierIfAny (unLoc -> ForAllT b ty) = (b, ty)
    removeForallQuantifierIfAny ty = (mempty, ty)

    toVarName (VarT v :@ _) = v
    toVarName (t :@ _) = error $ "Cannot get name of non-type variable type '" <> show t <> "'."

typecheckInstruction :: (?tcFlags :: TypecheckerFlags) => SC.Instruction -> Position -> Bool -> Typechecker TypedStatement
typecheckInstruction i p unsafe = do
  Ctx _ _ _ chi sigma epsilon <- gets snd
  let chi' = flip Map.filter chi \case
        BangT :@ _ -> False
        _ -> True

  ti <- case i of
    SC.NOP -> tc_nop p
    SC.MV src dst -> tc_mv src dst p
    SC.RET -> tc_ret p
    SC.JMP l -> tc_jmp l p
    SC.CALL l -> tc_call l p
    SC.SALLOC t -> tc_salloc t p
    SC.SFREE -> tc_sfree p
    SC.SLD n r -> tc_sld n r p
    SC.SST v n -> tc_sst v n p
    SC.LD ptr r -> tc_ld ptr r unsafe p
    SC.ST e ptr -> tc_st e ptr unsafe p
    SC.SREF n r -> tc_sref n r p
    SC.AND x y r -> tc_and x y r p
    SC.OR x y r -> tc_or x y r p
    SC.XOR x y r -> tc_xor x y r p
    SC.NOT e r -> tc_not e r p
    SC.CMVZ a b c r -> tc_cmvz a b c r p
    SC.CMVNZ a b c r -> tc_cmvz a c b r p
    SC.ADD a b r -> tc_add a b r p
    SC.SHIFTL a b r -> tc_shiftl a b r p
    SC.SHIFTR a b r -> tc_shiftr a b r p
    SC.SUB a b r -> tc_sub a b r p
    SC.MUL a b r -> tc_mul a b r p
    _ -> error $ "Unrecognized instruction '" <> show i <> "'."

  pure (TInstr (ti :@ p) chi' sigma epsilon)
