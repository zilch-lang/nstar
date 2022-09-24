{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--  Module: Language.NStar.Typechecker.Errors
--  Copyright: (c) Mesabloo, 2020
--  License: BSD3
--  Stability: experimental
module Language.NStar.Typechecker.Errors where

import Data.List (intercalate)
import Data.Located (Located (..), Position (..), getPos)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Error.Diagnose (Marker (..), Report, err, warn)
import Language.NStar.Syntax.Core (Register)
import Language.NStar.Typechecker.Core (Expr, Kind, Type (RecordT, VarT))
import Language.NStar.Typechecker.Pretty ()
import Prettyprinter

data TypecheckError
  = Uncoercible (Located Type) (Located Type)
  | NoReturnAddress Position Register (Map (Located Register) (Located Type))
  | InfiniteType (Located Type) (Located Text)
  | DomainsDoNotSubtype (Located (Map (Located Register) (Located Type))) (Located (Map (Located Register) (Located Type)))
  | RecordUnify TypecheckError (Located (Map (Located Register) (Located Type))) (Located (Map (Located Register) (Located Type)))
  | ToplevelReturn Position
  | ContextIsMissingOnReturn Position Position (Set (Located Register))
  | FromReport (Report String)
  | RegisterNotFoundInContext Register Position (Set (Located Register))
  | UnknownLabel (Located Text)
  | CannotInferSpecialization Int Int Position
  | TooMuchSpecialization Int Int Position
  | CannotJumpBecauseOf Position TypecheckError
  | MissingRegistersInContext [Register] Position
  | NonPointerTypeOnOffset Type Position
  | UnknownDataLabel Text Position
  | UnsafeOperationOutOfUnsafeBlock Position
  | NonStackPointerRegister Type Position Position
  | CannotPopCodeAddress Type Position
  | CannotMovToDestination Type Type Position Position
  | CannotPopIntoDestination Type Type Type Position Position
  | AbstractContinuationOnReturn Position (Located Type)
  | TryingToOverwriteRegisterContinuation (Located Register) Position
  | StackIsNotBigEnough Integer Type Position
  | CannotReturnToStackContinuation Type Position
  | CannotCallWithAbstractContinuation Type Position
  | CannotDiscardContinuationFromStackTop Position
  | CannotStoreContinuationOntoHeap Register Position Position
  | RegisterCannotBePropagated [Located Register] [Located Register] Position Position
  | CannotTakeReferenceToFunctionPointerOnStack (Located Type) Integer Position Position
  | StructureOffsetMustBeKnown Position Position Position
  | OutOfBoundsStructureAccess Integer Integer Position
  | ExpectedIntegralType Type Position
  | CannotConditionallyMoveContinuation Position
  | CannotCompareTypes (Located Type) (Located Type) Position

data TypecheckWarning

fromTypecheckWarning :: TypecheckWarning -> Report String
fromTypecheckWarning _ = warn "" [] []

-- | Transforms a typechcking error into a report.
fromTypecheckError :: TypecheckError -> Report String
fromTypecheckError (Uncoercible (t1 :@ p1) (t2 :@ p2)) = uncoercibleTypes (t1, p1) (t2, p2)
fromTypecheckError (InfiniteType (t :@ p1) (v :@ p2)) = infiniteType (t, p1) (v, p2)
fromTypecheckError (NoReturnAddress p r ctx) = retWithoutReturnAddress p r ctx
fromTypecheckError (DomainsDoNotSubtype (m1 :@ p1) (m2 :@ p2)) = recordDomainsDoNotSubset (m1, p1) (m2, p2)
fromTypecheckError (RecordUnify err (m1 :@ p1) (m2 :@ p2)) = fromTypecheckError err <> warn "\n" [] [] <> recordValuesDoNotUnify (m1, p1) (m2, p2)
--      ^^^^^^^^^^^^^^^
-- This is just to insert a newline between error
fromTypecheckError (ToplevelReturn p) = returnAtTopLevel p
fromTypecheckError (ContextIsMissingOnReturn p1 p2 regs) = contextIsMissingOnReturnAt (Set.toList regs) p1 p2
fromTypecheckError (FromReport r) = r
fromTypecheckError (RegisterNotFoundInContext r p ctx) = registerNotFoundInContext r p (Set.toList ctx)
fromTypecheckError (UnknownLabel (n :@ p)) = unknownLabel n p
fromTypecheckError (CannotInferSpecialization nbGot nbExpect p) = cannotInferSpecialization nbGot nbExpect p
fromTypecheckError (TooMuchSpecialization ng ne p) = tooMuchSpecialization ng ne p
fromTypecheckError (CannotJumpBecauseOf p err) = cannotJumpAt p <> warn "\n" [] [] <> fromTypecheckError err
fromTypecheckError (MissingRegistersInContext rs p) = missingRegistersInContext rs p
fromTypecheckError (NonPointerTypeOnOffset t p) = typeIsNotAPointer t p
fromTypecheckError (UnknownDataLabel n p) = unknownDataLabel n p
fromTypecheckError (UnsafeOperationOutOfUnsafeBlock p) = unsafeNotInUnsafeBlock p
fromTypecheckError (NonStackPointerRegister ty p p') = spIsNotAStackRegister ty p p'
fromTypecheckError (CannotPopCodeAddress t p) = cannotPopCodespaceAddress t p
fromTypecheckError (CannotMovToDestination s d p1 p2) = cannotMovToDestination s d p1 p2
fromTypecheckError (CannotPopIntoDestination t1 t2 t3 p1 p2) = cannotPopIntoDestination t1 t2 t3 p1 p2
fromTypecheckError (AbstractContinuationOnReturn p1 ty) = cannotReturnToUnknownContinuation p1 ty
fromTypecheckError (TryingToOverwriteRegisterContinuation r p) = cannotOverwriteRegisterContinuation r p
fromTypecheckError (StackIsNotBigEnough n t p) = stackIsNotBigEnough n t p
fromTypecheckError (CannotReturnToStackContinuation t p) = cannotReturnToStackContinuation t p
fromTypecheckError (CannotCallWithAbstractContinuation t p) = cannotCallOnAbstractContinuation t p
fromTypecheckError (CannotDiscardContinuationFromStackTop p) = cannotDiscardContinuationFromStack p
fromTypecheckError (CannotStoreContinuationOntoHeap r p1 p2) = cannotStoreContOnHeap r p1 p2
fromTypecheckError (RegisterCannotBePropagated rs bs p1 p2) = cannotPropagateRegister rs bs p1 p2
fromTypecheckError (CannotTakeReferenceToFunctionPointerOnStack s n p1 p2) = cannotReferenceFunctionPointerOnStack s n p1 p2
fromTypecheckError (StructureOffsetMustBeKnown p1 p2 p3) = structureOffsetMustBeACompileTimeConstant p1 p2 p3
fromTypecheckError (OutOfBoundsStructureAccess o s p) = outOfBoundsStructureAccess o s p
fromTypecheckError (ExpectedIntegralType t p) = expectedIntegralType t p
fromTypecheckError (CannotConditionallyMoveContinuation p) = cannotConditionallyMoveContinuation p
fromTypecheckError (CannotCompareTypes t1 t2 p) = cannotCompareTypes t1 t2 p

-- | Happens when there is no possible coercion from the first type to the second type.
uncoercibleTypes :: (Type, Position) -> (Type, Position) -> Report String
uncoercibleTypes (t1, p1) (t2, p2) =
  err
    ("Type '" <> show (pretty t1) <> "' cannot be coerced to '" <> show (pretty t2) <> "'.")
    [(p1, This "")]
    ["Visit <https://github.com/nihil-lang/nsc/blob/develop/docs/type-coercion.md> to learn about type coercion in N*."]

-- | Happens when the stack infered on a @ret@ call does not have a pointer to some code on the top.
retWithoutReturnAddress :: Position -> Register -> Map (Located Register) (Located Type) -> Report String
retWithoutReturnAddress p r ctx =
  err
    ("The `ret` instruction expects a return address to be stored in the continuation register, but did not find one.")
    [(p, Where $ "Found some data of type '" <> show (pretty $ ctx Map.! (r :@ p)) <> "'")]
    []

-- | Happens when we try to create a substitution like @a ~ [a]@, where a given free type variable would be infinitely replaced,
--   thus leading to an infinite type.
infiniteType :: (Type, Position) -> (Text, Position) -> Report String
infiniteType (ty, p1) (var, p2) =
  err
    ("Cannot create the infinite type from the relation '" <> show (pretty ty) <> " ~ " <> Text.unpack var <> "'.")
    [ (p2, Where $ "The type variable '" <> Text.unpack var <> "' is infered from the context"),
      (p1, Where $ "The type '" <> show (pretty ty) <> "' is bound here")
    ]
    ["Learn more about infinite types at <https://github.com/nihil-lang/nsc/blob/develop/docs/infinite-type.md>."]

-- | Happens when the union of the keys of the first map and of the second map is not equal to the keys of the first map.
recordDomainsDoNotSubset :: (m ~ Map (Located Register) (Located Type)) => (m, Position) -> (m, Position) -> Report String
recordDomainsDoNotSubset (m1, p1) (m2, p2) =
  err
    ("All keys in '" <> show (pretty m1) <> "' are not present in '" <> show (pretty m2) <> "'")
    [(p1, This "")]
    []

-- needed for the above error
instance Pretty (Map (Located Register) (Located Type)) where
  pretty = encloseSep lbrace rbrace comma . fmap prettyBind . Map.toList
    where
      prettyBind (r, t) = pretty r <+> colon <+> pretty t

-- | Happens when two records fields cannot be coerced to each other.
recordValuesDoNotUnify :: (m ~ Map (Located Register) (Located Type)) => (m, Position) -> (m, Position) -> Report String
recordValuesDoNotUnify (m1, p1) (m2, p2) =
  let s1 = VarT ("_s1" :@ p1) :@ p1
      s2 = VarT ("_s2" :@ p2) :@ p2
      e1 = VarT ("_e1" :@ p1) :@ p1
      e2 = VarT ("_e2" :@ p2) :@ p2
   in err
        ("Record types cannot be coerced because at least one of their common fields cannot be coerced to each other.")
        [(p1, Where ("'" <> show (pretty (RecordT m1 s1 e1 False)) <> "' cannot be coerced to '" <> show (pretty (RecordT m2 s2 e2 False)) <> "'"))]
        ["Visit <https://github.com/nihil-lang/nsc/blob/develop/docs/type-coercion.md> to learn about type coercion in N*."]

-- | Happens when a @ret@ instruction is not in a function. This also means that the instruction has no enclosing scope.
returnAtTopLevel :: Position -> Report String
returnAtTopLevel p =
  err
    ("Attempting to `ret` when no label has been crossed so far.")
    [(p, This "")]
    []

-- | Happens when there are some unset registers on a @ret@ instruction.
contextIsMissingOnReturnAt :: [Located Register] -> Position -> Position -> Report String
contextIsMissingOnReturnAt regs p1 p2 =
  err
    "Current context is missing some registers in order to return safely."
    [ (p1, This ("Unset (missing) register" <> (if length regs /= 1 then "s" else "") <> ": " <> intercalate ", " (show . pretty <$> regs))),
      (p2, Where "The expected return context was found here")
    ]
    []

-- | Happens when a kind was found where a @Ts@ was expected.
kindIsNotAStackKind :: Kind -> Position -> Position -> Report String
kindIsNotAStackKind k p1 p2 =
  err
    ("Kind '" <> show (pretty k) <> "' cannot be used in place of a stack kind.")
    [ (p2, This "Expected to be of kind `Ts`"),
      (p1, Where "Kind is infered from here")
    ]
    []

-- | Happens when a kind was found but it was expected to be either @Ta@ or @T8@.
kindIsNotADataKind :: Kind -> Position -> Position -> Report String
kindIsNotADataKind k p1 p2 =
  err
    ("Kind '" <> show (pretty k) <> "' was expected to be a data kind, but was found not to be one.")
    [ (p2, This "Expected to be of kind `Ta` or `T8`"),
      (p1, Where "Kind is infered from here")
    ]
    []

-- | Happens when a kind was found but wasn't @TN@ where @N@ is any positive natural number multiple of 2 lower than 16.
kindIsUnsized :: Kind -> Position -> Position -> Report String
kindIsUnsized k p1 p2 =
  err
    ("Kind '" <> show (pretty k) <> "' is unsized, but was expected to be sized.")
    [ (p2, This "Expected to be of a sized kind"),
      (p1, Where "Kind is infered from here")
    ]
    []

-- | Happens when a type variable has not been found in the current analysis scope.
unboundTypeVariable :: Text -> Position -> Report String
unboundTypeVariable v p =
  err
    ("Type variable '" <> Text.unpack v <> "' was not found in scope.")
    [ (p, This "Variable not in context"),
      (p, Maybe "Did you bind it in a `forall` quantifier?")
    ]
    []

-- | Happens when trying to read from a register that has not yet been set.
registerNotFoundInContext :: Register -> Position -> [Located Register] -> Report String
registerNotFoundInContext r p ctx =
  err
    ("Register '" <> show (pretty r) <> "' was not found in the current context, or is not set.")
    [ (p, This ""),
      (p, Where $ "Bound register" <> (if length ctx /= 1 then "s" else "") <> " at this point: " <> intercalate ", " (show . pretty <$> ctx)),
      (p, Maybe "Try to set this register with a `mov`, or add it to the context of the nearest label.")
    ]
    []

unknownLabel :: Text -> Position -> Report String
unknownLabel name callPos =
  err
    ("Trying to jump to the label '" <> Text.unpack name <> "' but it has not been found in the file.")
    [ (callPos, This "Label not found in file"),
      (callPos, Maybe "Did you forget to declare its linkage type?")
    ]
    []

cannotInferSpecialization :: Int -> Int -> Position -> Report String
cannotInferSpecialization nbGot nbExpected p =
  err
    ("Cannot infer type variable specialization for the missing " <> show (nbExpected - nbGot) <> " type parameters.")
    [(p, This $ "Expected " <> show nbExpected <> " type parameters")]
    ["Type inference in not yet available when specializing type parameters. You have to specify all of them by hand for now."]

tooMuchSpecialization :: Int -> Int -> Position -> Report String
tooMuchSpecialization nbGot nbExpected p =
  err
    ("Specialized label only has " <> show nbExpected <> " type parameters but was expected to have " <> show nbGot <> ".")
    [(p, This "Too much type parameters given")]
    []

cannotJumpAt :: Position -> Report String
cannotJumpAt p =
  err
    "Cannot jump to the label because of the following error:"
    [(p, This "Jumping was forbidden here")]
    []

missingRegistersInContext :: [Register] -> Position -> Report String
missingRegistersInContext rs p =
  err
    ("Context is missing some register binds.")
    [(p, This ("Missing registers: " <> intercalate ", " (show . pretty <$> rs)))]
    []

typeIsNotAPointer :: Type -> Position -> Report String
typeIsNotAPointer ty p =
  err
    ("Infered type is not a pointer.")
    [(p, This ("Type infered: " <> show (pretty ty)))]
    []

unknownDataLabel :: Text -> Position -> Report String
unknownDataLabel name p =
  err
    ("Label '" <> Text.unpack name <> "' not found in data sections.")
    [(p, This "Expected to be found in any data section, but not found")]
    []

unsafeNotInUnsafeBlock :: Position -> Report String
unsafeNotInUnsafeBlock p =
  err
    "Unsafe operation not enclosed in an 'unsafe' block."
    [(p, This "This is considered an unsafe operation, therefore must be placed in an 'unsafe block'")]
    []

spIsNotAStackRegister :: Type -> Position -> Position -> Report String
spIsNotAStackRegister ty p1 p =
  err
    "%sp is not a stack pointer."
    [ (p1, This $ "%sp was found to be of the type: '" <> show (pretty ty) <> "'"),
      (p, Where "Infered from here")
    ]
    []

cannotPopCodespaceAddress :: Type -> Position -> Report String
cannotPopCodespaceAddress ty p =
  err
    "Cannot pop a code-space address off the stack."
    [ (p, This "Tried to pop the top of the stack"),
      (p, Where $ "%sp has type '" <> show (pretty ty) <> "'")
    ]
    []

cannotMovToDestination :: Type -> Type -> Position -> Position -> Report String
cannotMovToDestination ty1 ty2 p1 p2 =
  err
    "Unable to move the source operand into the destination."
    [ (p1, This $ "The source operand is of type " <> show (pretty ty1)),
      (p2, Where $ "The destination operand was found to accept values of type " <> show (pretty ty2))
    ]
    []

cannotPopIntoDestination :: Type -> Type -> Type -> Position -> Position -> Report String
cannotPopIntoDestination expected sHead sTail p1 p2 =
  err
    "Unable to pop the stack pointer into the destination."
    [ (p2, This $ "Trying to pop a value of type " <> show (pretty sHead)),
      (p2, Where $ "The stack is infered to '" <> show (pretty sHead) <> "::" <> show (pretty sTail) <> "'"),
      (p1, Where $ "The destination operand was found to accept values of type " <> show (pretty expected))
    ]
    []

cannotUnifyKinds :: Kind -> Kind -> Position -> Position -> Report String
cannotUnifyKinds k1 k2 p1 p2 =
  err
    ("Cannot unify kinds " <> show (pretty k1) <> " and " <> show (pretty k2))
    [ (p1, Where $ show (pretty k1) <> " is infered from here"),
      (p2, Where $ show (pretty k2) <> " is infered from here")
    ]
    []

cannotReturnToUnknownContinuation :: Position -> Located Type -> Report String
cannotReturnToUnknownContinuation p1 cont =
  err
    "Trying to jump back to a continuation hidden behind a type variable."
    [ (p1, This $ "Trying to return from here"),
      (getPos cont, Where $ "The return continuation was infered to " <> show (pretty cont))
    ]
    [ "Because the continuation is abstract, I could not define where you wanted me to return to."
        <> "Therefore I took the liberty to inform you that I'm a bit confused."
    ]

cannotOverwriteRegisterContinuation :: Located Register -> Position -> Report String
cannotOverwriteRegisterContinuation (r :@ p1) p2 =
  err
    "Cannot overwrite the register containing the current continuation."
    [ (p1, Where $ "This register holds the current continuation, therefore cannot be the destination"),
      (p2, This $ "While trying to type-check this instruction")
    ]
    []

stackIsNotBigEnough :: Integer -> Type -> Position -> Report String
stackIsNotBigEnough n t p =
  err
    ("Stack infered must be at least of length " <> show n)
    [(p, This $ "Stack is infered from here to " <> show (pretty t))]
    []

cannotReturnToStackContinuation :: Type -> Position -> Report String
cannotReturnToStackContinuation t p =
  err
    "Cannot return to a continuation stored on the stack."
    [(p, This $ "The current continuation is " <> show (pretty t) <> ", which is not a register")]
    []

cannotCallOnAbstractContinuation :: Type -> Position -> Report String
cannotCallOnAbstractContinuation t p =
  err
    "Cannot call a function when the current continuation is abstract."
    [(p, Where $ "The return continuation was infered to '" <> show (pretty t))]
    []

cannotDiscardContinuationFromStack :: Position -> Report String
cannotDiscardContinuationFromStack p =
  err
    "Cannot discard the continuation stored on top of the stack."
    [(p, This $ "Trying to discard stack top but the current continuation is stored there")]
    []

cannotStoreContOnHeap :: Register -> Position -> Position -> Report String
cannotStoreContOnHeap r p1 p2 =
  err
    "Cannot move the current continuation into a heap object."
    [ (p2, This $ "When trying to type-check this instruction"),
      (p1, Where $ "The register " <> show (pretty r) <> " contains the current continuation")
    ]
    []

cannotPropagateRegister :: [Located Register] -> [Located Register] -> Position -> Position -> Report String
cannotPropagateRegister rs bs p1 p2 =
  err
    "Cannot propagate some of the registers because they were marked as potentially overwritten."
    [ (p2, This "When trying to call a function"),
      (p1, Where $ "The type of this label marks " <> showCommaSep bs <> " as potentially overwriting\nbut trying to propagate registers " <> showCommaSep rs <> " to the continuation")
    ]
    ["Some registers may need to be caller-saved (saved somewhere, e.g. on the stack, by the caller) according to some coding conventions."]
  where
    showCommaSep regs = "{" <> intercalate "," (show . pretty <$> regs) <> "}"

cannotReferenceFunctionPointerOnStack :: Located Type -> Integer -> Position -> Position -> Report String
cannotReferenceFunctionPointerOnStack s n p1 p2 =
  err
    "Trying to reference a function pointer is disallowed for safety reasons."
    [ (p1, This $ "Taking a reference to the stack cell #" <> show n <> " which is a function pointer"),
      (p2, Where $ "The current stack was inferred to " <> show (pretty s))
    ]
    ["This specific feature has been completely disallowed in N* because of how raw pointers can be used in the language. Moreover, functions are implicitly boxed so this operation should never be necessary to perform (if it weren't forbidden)."]

structureOffsetMustBeACompileTimeConstant :: Position -> Position -> Position -> Report String
structureOffsetMustBeACompileTimeConstant p1 p2 p3 =
  err
    "Trying to access a structure field with an unknown index."
    [ (p2, This $ "This was found to be a structure pointer"),
      (p3, This $ "But offsetting a structure pointer requires a compile-time known integral constant")
    ]
    ["What would it mean to have variable access to structure members anyway?"]

outOfBoundsStructureAccess :: Integer -> Integer -> Position -> Report String
outOfBoundsStructureAccess n s p =
  err
    "Out of bounds member access in structure."
    [(p, This $ "Trying to access the " <> show (n + 1) <> ordinalSuffix (n + 1) <> " member in a structure which only contains " <> show s <> " members")]
    []
  where
    ordinalSuffix i =
      let i' = abs i `mod` 100
       in if i' > 10 && i' < 20
            then "th"
            else case i' `mod` 10 of
              1 -> "st"
              2 -> "nd"
              3 -> "rd"
              _ -> "th"

expectedIntegralType :: Type -> Position -> Report String
expectedIntegralType t p =
  err
    "Expected an integral type."
    [(p, This $ "Got type " <> show (pretty t) <> " instead")]
    []

cannotConditionallyMoveContinuation :: Position -> Report String
cannotConditionallyMoveContinuation p =
  err
    "Trying to move continuation conditionally."
    [(p, This "While typechecking this instruction.")]
    []

cannotCompareTypes :: Located Type -> Located Type -> Position -> Report String
cannotCompareTypes (t1 :@ p1) (t2 :@ p2) p3 =
  err
    "Cannot compare values of these types together."
    [ (p3, This "While checking that comparison can be performed here"),
      (p1, Where $ "This has type '" <> show (pretty t1) <> "'"),
      (p2, Where $ "This has type '" <> show (pretty t2) <> "'")
    ]
    []
