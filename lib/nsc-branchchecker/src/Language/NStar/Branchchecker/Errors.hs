module Language.NStar.Branchchecker.Errors where

import Data.Located
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Diagnose (Report, Marker(..), hint, reportError, prettyText)

data BranchcheckerError
  = ControlFlowLeak (Located Text) (Located Text)
  | NonReturningCall (Located Text)

fromBranchcheckerError :: BranchcheckerError -> Report String
fromBranchcheckerError (ControlFlowLeak (from :@ p1) (to :@ p2)) = controlFlowLeak (from, p1) (to, p2)
fromBranchcheckerError (NonReturningCall (from :@ p))            = nonReturningCall (from, p)

controlFlowLeak :: (Text, Position) -> (Text, Position) -> Report String
controlFlowLeak (from, p1) (to, p2@(Position (l1, _) _ file)) =
  reportError ("Control flow leak from scope of label '" <> Text.unpack from <> "' into scope of label '" <> Text.unpack to <> "'.")
    [ (p1, Where "Leaked from here")
    , (p2, Where "Leaked into here")
    , (p3, Maybe $ "Consider adding `jmp " <> Text.unpack to <> "` here to prevent the leak")]
    [ hint "There is no way of typechecking a program where control flow leaks through labels." ]
  where
    p3 = Position (l1 - 1, 1) (l1 - 1, 1) file

nonReturningCall :: (Text, Position) -> Report String
nonReturningCall (from, p) =
  reportError ("A call to '" <> Text.unpack from <> "' does not return.")
    [ (p, Where "The `call`ed label is here") ]
    [ hint "Every `call` should have a corresponding `ret`. Not returning from a `call` is considered a stack leak and is forbidden in N*." ]
