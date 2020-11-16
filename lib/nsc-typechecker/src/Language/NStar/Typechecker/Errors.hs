{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

{-|
  Module: Language.NStar.Typechecker.Errors
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}


module Language.NStar.Typechecker.Errors where

import Text.Diagnose (Report, Marker(..), hint, reportError, reportWarning, prettyText)
import Language.NStar.Typechecker.Core (Type(Record), Register(RSP), Kind)
import Data.Located (Position(..), Located(..))
import Language.NStar.Typechecker.Pretty()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set

data TypecheckError
  = Uncoercible (Located Type) (Located Type)
  | NoReturnAddress Position (Map (Located Register) (Located Type))
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

-- | Transforms a typechcking error into a report.
fromTypecheckError :: TypecheckError -> Report String
fromTypecheckError (Uncoercible (t1 :@ p1) (t2 :@ p2))          = uncoercibleTypes (t1, p1) (t2, p2)
fromTypecheckError (InfiniteType (t :@ p1) (v :@ p2))           = infiniteType (t, p1) (v, p2)
fromTypecheckError (NoReturnAddress p ctx)                      = retWithoutReturnAddress p ctx
fromTypecheckError (DomainsDoNotSubtype (m1 :@ p1) (m2 :@ p2))  = recordDomainsDoNotSubset (m1, p1) (m2, p2)
fromTypecheckError (RecordUnify err (m1 :@ p1) (m2 :@ p2))      = fromTypecheckError err <> reportWarning "\n" [] [] <> recordValuesDoNotUnify (m1, p1) (m2, p2)
                                                                                    --      ^^^^^^^^^^^^^^^^^^^^^^^^
                                                                                    -- This is just to insert a newline between error
fromTypecheckError (ToplevelReturn p)                           = returnAtTopLevel p
fromTypecheckError (ContextIsMissingOnReturn p1 p2 regs)        = contextIsMissingOnReturnAt (Set.toList regs) p1 p2
fromTypecheckError (FromReport r)                               = r
fromTypecheckError (RegisterNotFoundInContext r p ctx)          = registerNotFoundInContext r p (Set.toList ctx)
fromTypecheckError (UnknownLabel (n :@ p))                      = unknownLabel n p
fromTypecheckError (CannotInferSpecialization nbGot nbExpect p) = cannotInferSpecialization nbGot nbExpect p
fromTypecheckError (TooMuchSpecialization ng ne p)              = tooMuchSpecialization ng ne p
fromTypecheckError (CannotJumpBecauseOf p err)                  = cannotJumpAt p <> reportWarning "\n" [] [] <> fromTypecheckError err
fromTypecheckError (MissingRegistersInContext rs p)             = missingRegistersInContext rs p

-- | Happens when there is no possible coercion from the first type to the second type.
uncoercibleTypes :: (Type, Position) -> (Type, Position) -> Report String
uncoercibleTypes (t1, p1) (t2, p2) =
  reportError ("Type '" <> show (prettyText t1) <> "' cannot be coerced to '" <> show (prettyText t2) <> "'.")
    [ (p1, This "") ]
    [ hint "Visit <https://github.com/nihil-lang/nsc/blob/develop/docs/type-coercion.md> to learn about type coercion in N*." ]

-- | Happens when the stack infered on a @ret@ call does not have a pointer to some code on the top.
retWithoutReturnAddress :: Position -> Map (Located Register) (Located Type) -> Report String
retWithoutReturnAddress p ctx =
  reportError ("The `ret` instruction expects a return address to be on top of the stack, but did not find any.")
    [ (p, Where if | Just stack <- rsp -> "The stack infered at this point is: `" <> show (prettyText stack) <> "`"
                   | otherwise         -> "No stack found at this point") ]
    []
 where
   rsp = Map.lookup (RSP :@ dummyPos) ctx
   dummyPos = Position (1, 1) (1, 1) "dummy"

-- | Happens when we try to create a substitution like @a ~ [a]@, where a given free type variable would be infinitely replaced,
--   thus leading to an infinite type.
infiniteType :: (Type, Position) -> (Text, Position) -> Report String
infiniteType (ty, p1) (var, p2) =
  reportError ("Cannot create the infinite type from the relation '" <> show (prettyText ty) <> " ~ " <> Text.unpack var <> "'.")
    [ (p2, Where $ "The type variable '" <> Text.unpack var <> "' is infered from the context")
    , (p1, Where $ "The type '" <> show (prettyText ty) <> "' is bound here") ]
    [ hint "Learn more about infinite types at <https://github.com/nihil-lang/nsc/blob/develop/docs/infinite-type.md>." ]

-- | Happens when the union of the keys of the first map and of the second map is not equal to the keys of the first map.
recordDomainsDoNotSubset :: (m ~ Map (Located Register) (Located Type)) => (m, Position) -> (m, Position) -> Report String
recordDomainsDoNotSubset (m1, p1) (m2, p2) =
  reportError ("All keys in '" <> show (prettyText $ Record m1 False) <> "' are not present in '" <> show (prettyText $ Record m2 False) <> "'")
    [ (p1, This "") ]
    []

-- | Happens when two records fields cannot be coerced to each other.
recordValuesDoNotUnify :: (m ~ Map (Located Register) (Located Type)) => (m, Position) -> (m, Position) -> Report String
recordValuesDoNotUnify (m1, p1) (m2, p2) =
  reportError ("Record types cannot be coerced because at least one of their common fields cannot be coerced to each other.")
    [ (p1, Where ("'" <> show (prettyText (Record m1 False)) <> "' cannot be coerced to '" <> show (prettyText (Record m2 False)) <> "'")) ]
    [ hint "Visit <https://github.com/nihil-lang/nsc/blob/develop/docs/type-coercion.md> to learn about type coercion in N*." ]

-- | Happens when a @ret@ instruction is not in a function. This also means that the instruction has no enclosing scope.
returnAtTopLevel :: Position -> Report String
returnAtTopLevel p =
  reportError ("Attempting to `ret` when no label has been crossed so far.")
    [ (p, This "") ]
    []

-- | Happens when there are some unset registers on a @ret@ instruction.
contextIsMissingOnReturnAt :: [Located Register] -> Position -> Position -> Report String
contextIsMissingOnReturnAt regs p1 p2 =
  reportError "Current context is missing some registers in order to return safely."
    [ (p1, This ("Unset (missing) register" <> (if length regs /= 1 then "s" else "") <> ": " <> intercalate ", " (show . prettyText <$> regs)))
    , (p2, Where "The expected return context was found here") ]
    []

-- | Happens when a kind was found where a @Ts@ was expected.
kindIsNotAStackKind :: Kind -> Position -> Position -> Report String
kindIsNotAStackKind k p1 p2 =
  reportError ("Kind '" <> show (prettyText k) <> "' cannot be used in place of a stack kind.")
    [ (p2, This "Expected to be of kind `Ts`")
    , (p1, Where "Kind is infered from here") ]
    []

-- | Happens when a kind was found but it was expected to be either @Ta@ or @T8@.
kindIsNotADataKind :: Kind -> Position -> Position -> Report String
kindIsNotADataKind k p1 p2 =
  reportError ("Kind '" <> show (prettyText k) <> "' was expected to be a data kind, but was found not to be one.")
    [ (p2, This "Expected to be of kind `Ta` or `T8`")
    , (p1, Where "Kind is infered from here") ]
    []

-- | Happens when a kind was found but wasn't @TN@ where @N@ is any positive natural number multiple of 2 lower than 16.
kindIsUnsized :: Kind -> Position -> Position -> Report String
kindIsUnsized k p1 p2 =
  reportError ("Kind '" <> show (prettyText k) <> "' is unsized, but was expected to be sized.")
    [ (p2, This "Expected to be of a sized kind")
    , (p1, Where "Kind is infered from here") ]
    []

-- | Happens when a type variable has not been found in the current analysis scope.
unboundTypeVariable :: Text -> Position -> Report String
unboundTypeVariable v p =
  reportError ("Type variable '" <> Text.unpack v <> "' was not found in scope.")
    [ (p, This "Variable not in context")
    , (p, Maybe "Did you bind it in a `forall` quantifier?")]
    []

-- | Happens when trying to read from a register that has not yet been set.
registerNotFoundInContext :: Register -> Position -> [Located Register] -> Report String
registerNotFoundInContext r p ctx =
  reportError ("Register '" <> show (prettyText r) <> "' was not found in the current context, or is not set.")
    [ (p, This "")
    , (p, Where $ "Bound register" <> (if length ctx /= 1 then "s" else "") <> " at this point: " <> intercalate ", " (show . prettyText <$> ctx))
    , (p, Maybe "Try to set this register with a `mov`, or add it to the context of the nearest label.") ]
    []

unknownLabel :: Text -> Position -> Report String
unknownLabel name callPos =
  reportError ("Trying to jump to the label '" <> Text.unpack name <> "' but it has not been found in the file.")
    [ (callPos, This "Label not found in file")
    , (callPos, Maybe "Did you forget to declare its linkage type?") ]
    []

cannotInferSpecialization :: Int -> Int -> Position -> Report String
cannotInferSpecialization nbGot nbExpected p =
  reportError ("Cannot infer type variable specialization for the missing " <> show (nbExpected - nbGot) <> " type parameters.")
    [ (p, This $ "Expected " <> show nbExpected <> " type parameters") ]
    [ hint "Type inference in not yet available when specializing type parameters. You have to specify all of them by hand for now." ]

tooMuchSpecialization :: Int -> Int -> Position -> Report String
tooMuchSpecialization nbGot nbExpected p =
  reportError ("Specialized label only has " <> show nbExpected <> " type parameters but was expected to have " <> show nbGot <> ".")
    [ (p, This "Too much type parameters given") ]
    []

cannotJumpAt :: Position -> Report String
cannotJumpAt p =
  reportError "Cannot jump to the label because of the following error:"
    [ (p, This "Jumping was forbidden here") ]
    []

missingRegistersInContext :: [Register] -> Position -> Report String
missingRegistersInContext rs p =
  reportError ("Context is missing some register binds")
    [ (p, This ("Missing registers: " <> intercalate ", " (show . prettyText <$> rs))) ]
    []
