{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

module Language.NStar.Typechecker.Errors where

import Text.Diagnose (Report, Marker(..), hint, reportError, reportWarning, prettyText)
import Language.NStar.Typechecker.Core (Type(Record), Register(RSP))
import Data.Located (Position(..), Located(..))
import Language.NStar.Typechecker.Pretty()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

-- | Happens when there is no possible coercion from the first type to the second type.
uncoercibleTypes :: (Type, Position) -> (Type, Position) -> Report String
uncoercibleTypes (t1, p1) (t2, p2) =
  reportError ("Type '" <> show (prettyText t1) <> "' cannot be coerced to '" <> show (prettyText t2) <> "'.")
    [ (p1, This "") ]
    [ hint "See <https://github.com/nihil-lang/nsc/blob/develop/docs/type-coercion.md> for more information about type coercion." ]

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
  reportError ("All keys in '" <> show (prettyText $ Record m1) <> "' are not present in '" <> show (prettyText $ Record m2) <> "'")
    [ (p1, This "") ]
    []

recordValuesDoNotUnify :: (m ~ Map (Located Register) (Located Type)) => (m, Position) -> (m, Position) -> Report String
recordValuesDoNotUnify (m1, p1) (m2, p2) =
  reportError ("Record types cannot be coerced because at least one of their common fields cannot be coerced to each other.")
    [ (p1, Where ("'" <> show (prettyText (Record m1)) <> "' cannot be coerced to '" <> show (prettyText (Record m2)) <> "'")) ]
    [ hint "Visit <https://github.com/nihil-lang/nsc/blob/develop/docs/type-coercion.md> to learn about type coercion in N*." ]
