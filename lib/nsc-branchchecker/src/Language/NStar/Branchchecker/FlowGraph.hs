module Language.NStar.Branchchecker.FlowGraph
( JumpGraph, JumpStyle(..) ) where

import Algebra.Graph.Labelled.AdjacencyMap (AdjacencyMap)
import Data.Text (Text)

type JumpGraph = AdjacencyMap JumpStyle Text

data JumpStyle
  = Jump
  | Call
  | Ret
  | Empty
  deriving (Show, Eq, Ord)

instance Semigroup JumpStyle where
  j <> Empty = j
  _ <> j     = j

instance Monoid JumpStyle where
  mempty = Empty
