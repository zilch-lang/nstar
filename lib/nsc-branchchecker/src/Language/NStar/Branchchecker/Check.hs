module Language.NStar.Branchchecker.Check (branchcheck) where

import Language.NStar.Branchchecker.FlowGraph
import Algebra.Graph.Labelled.AdjacencyMap ((-<), (>-))
import qualified Algebra.Graph.Labelled.AdjacencyMap as Graph
import Text.Diagnose (Diagnostic, diagnostic, (<++>), Position(..))
import Control.Monad.Except
import Control.Monad.State
import Language.NStar.Branchchecker.Errors
import Language.NStar.Typechecker.Core
import Data.Bifunctor (first, second)
import Control.Monad (forM_, unless)
import Data.Located (Located(..), unLoc)
import Data.Text (Text)
import Data.List (foldl')
import Data.Functor ((<&>))
import qualified Data.Set as Set
import Control.Monad.Reader
import Debug.Trace (traceShow)
import Internal.Error (internalError)
import Control.Applicative ((<|>))

type Checker a = StateT (Maybe (Located Text), JumpGraph) (Except BranchcheckerError) a

branchcheck :: TypedProgram -> Either (Diagnostic s String m) ()
branchcheck p = first toDiagnostic $ runExcept (evalStateT (branchcheckProgram p) (Nothing, Graph.empty))
  where toDiagnostic = (diagnostic <++>) . fromBranchcheckerError

branchcheckProgram :: TypedProgram -> Checker ()
branchcheckProgram (TProgram stts) = do
  graph <- gets snd
  let newGraph = maybe graph (\ m -> (("_start" :@ dummyPos)-<Call>-m) `Graph.overlay` graph) (programMain stts)
                                                      --  ^^^^^^^^^^^ "a-<i>-b" really means "a -i-> b"
  modify (second (const newGraph))
  -- Remove the edge from "_start" to "main" if our AST does not contain a `main` label.
  -- Else it causes problems like not branch-checking on an empty file.

  forM_ stts registerEdges

  checkJumpgraphForConsistency
  where
    programMain []                            = Nothing
    programMain ((TLabel l@(n :@ _) :@ _):ss) = (if n == "main" then Just l else Nothing) <|> programMain ss
    programMain (_:ss)                        = programMain ss

    dummyPos = Position (1, 1) (1, 2) ""

registerEdges :: Located TypedStatement -> Checker ()
registerEdges (TLabel name :@ _) =
  modify (first (const (Just name)))
registerEdges (TInstr i _ :@ p) = case unLoc i of
  RET -> do
    (lbl, graph) <- get
    let Just label = lbl

    -- fetch upwards the last call that got to the current label
    -- and insert an edge for each of the parents of those calls.
    let parents  = fetchAllCallParents label graph
        newGraph = foldl' (\ g p -> Graph.overlay g (label-<Ret>-p)) graph parents
    modify (second (const newGraph))

    pure ()

  -- other instruction do not act on the control flow.
  _ -> pure ()

fetchAllCallParents :: Located Text -> JumpGraph -> [Located Text]
fetchAllCallParents root graph =
  let predec  = Graph.preSet root graph
      labels  = Set.toList predec <&> \ p -> (p, Graph.edgeLabel p root graph)
      parents = labels >>= \ (p, l) -> case l of
        Call -> [p]
        _    -> fetchAllCallParents p (Graph.removeEdge p root graph)
  in parents

-----------------------------------------------------------------------------

checkJumpgraphForConsistency :: Checker ()
checkJumpgraphForConsistency = do
  checkCallsHaveRet

  pure ()

checkCallsHaveRet :: Checker ()
checkCallsHaveRet = do
  graph <- gets snd
  let edges    = Graph.edgeList graph
  let allCalls = filter (\ (j, _, _) -> j == Call) edges


  unless (null edges || null allCalls) do
    let (_, r, _):_ = edges
    -- the first edge is the root of the whole jump graph
    -- this is ideal because we have to start from the root
    -- (because it's a directed graph)
    let remaining     = execState (checks r graph) []
    case remaining of
      []          -> pure ()
      (_, _, c):_ -> throwError (NonReturningCall c)
  where
    checks root g = do
      let succs  = Graph.postSet root g
          labels = Set.toList succs <&> \ l -> (l, Graph.edgeLabel root l g)
      forM_ labels \ (s, j) -> case j of
        Call -> do
          push (Call, root, s)
          checks s (Graph.removeEdge root s g)
        Jump -> do
          checks s (Graph.removeEdge root s g)
        Ret -> do
          pop
    pop = do
      get >>= \ case
        []  -> pure ()
        _:s -> put s
    push = modify . (:)
