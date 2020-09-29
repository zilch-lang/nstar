{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

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
import Data.Located
import Data.Bifunctor (first, second)
import Control.Monad.Except
import Text.Diagnose hiding (Kind)
import Language.NStar.Typechecker.Errors
import Control.Monad.Writer
import qualified Data.Map as Map
import Language.NStar.Typechecker.Kinds (kindcheck)
import Language.NStar.Typechecker.Instructions
import Language.NStar.Typechecker.TC
import Console.NStar.Flags (TypecheckerFlags(..))


-- | Runs the typechecker on a given program, returning either an error or a well-formed program.
typecheck :: (?tcFlags :: TypecheckerFlags) => Program -> Either (Diagnostic s String m) (TypedProgram, Diagnostic s String m)
typecheck p = second (second toDiag) $
              first toDiagnostic $ runExcept (runWriterT (evalStateT (typecheckProgram p) (0, Ctx mempty mempty mempty Nothing)))
  where toDiagnostic = (diagnostic <++>) . fromTypecheckError
        toDiag = foldl ((. fromTypecheckError) . (<++>)) diagnostic

--------------------------------------------------------

-- | Entry point of the typechecker.
--
--   Typechecks a program, and return an elaborated form of it.
typecheckProgram :: (?tcFlags :: TypecheckerFlags) => Program -> Typechecker TypedProgram
typecheckProgram p@(Program []) = pure (TProgram [])
typecheckProgram (Program stts) = do
  registerAllLabels stts
  res <- TProgram <$> mapM typecheckStatement stts

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
registerAllLabels :: (?tcFlags :: TypecheckerFlags) => [Located Statement] -> Typechecker ()
registerAllLabels = mapM_ addLabelType . filter isLabel
  where isLabel (unLoc -> Label _ _) = True
        isLabel _                    = False

        addLabelType (unLoc -> Label name ty) = liftEither (first FromReport $ kindcheck ty) *> addType name ty
        addLabelType (unLoc -> t)             = error ("Adding type of non-label '" <> show t <> "' is impossible.")

-- | Typechecks a statement.
--
--   When it's a label, simply override the current context with the label's type,
--   and add kind bindings of the @forall@ if there is one.
--
--   When it's an instruction, just typecheck the instruction accordingly.
typecheckStatement :: (?tcFlags :: TypecheckerFlags) => Located Statement -> Typechecker (Located TypedStatement)
typecheckStatement (Label name ty :@ p) = do
  let (binders, record) = removeForallQuantifierIfAny ty
  setCurrentTypeContext (toRegisterMap record)
  setCurrentKindContext (Map.fromList $ first toVarName <$> binders)
  setLabel name
  pure (TLabel name :@ p)
 where
   removeForallQuantifierIfAny (unLoc -> ForAll b ty) = (b, ty)
   removeForallQuantifierIfAny ty                     = (mempty, ty)

   toRegisterMap (unLoc -> Record m) = m
   toRegisterMap (unLoc -> t)        = error $ "Cannot retrieve register mappings from non-record type '" <> show t <> "'"

   toVarName (Var v :@ _) = v
   toVarName (t :@ _)     = error $ "Cannot get name of non-type variable type '" <> show t <> "'."
typecheckStatement (Instr i :@ p)       = do
  (:@ p) <$> typecheckInstruction i p

typecheckInstruction :: (?tcFlags :: TypecheckerFlags) => Instruction -> Position -> Typechecker TypedStatement
typecheckInstruction i p = do
  uncurry TInstr . (i :@ p ,) <$>
    case i of
      RET         -> tc_ret p
      MOV src dst -> tc_mov src dst p
      _           -> error $ "Unrecognized instruction '" <> show i <> "'."
