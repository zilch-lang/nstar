module Language.NStar.Branchchecker.Check (branchcheck) where

import Language.NStar.Branchchecker.FlowGraph
import Algebra.Graph.Labelled.AdjacencyMap ((-<), (>-))
import qualified Algebra.Graph.Labelled.AdjacencyMap as Graph
import Text.Diagnose (Diagnostic, diagnostic, (<++>), Position(..))
import Control.Monad.Except
import Control.Monad.State
import Language.NStar.Branchchecker.Errors
import Language.NStar.Typechecker.Core
import Data.Bifunctor (first, second, bimap)
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
import Language.NStar.Syntax.Core hiding (Token(..))
import Data.Foldable (foldl')
import Control.Monad.Writer (WriterT, runWriterT)

type Checker a = StateT (Maybe (Located Text), JumpGraph) (WriterT [BranchcheckerWarning] (Except BranchcheckerError)) a

branchcheck :: TypedProgram -> Either (Diagnostic s String m) (Diagnostic s String m)
branchcheck p = bimap toDiagnostic (toDiagnostic' . snd) $ runExcept (runWriterT (evalStateT (branchcheckProgram p) (Nothing, Graph.empty)))
  where toDiagnostic = (diagnostic <++>) . fromBranchcheckerError
        toDiagnostic' = foldl' (<++>) diagnostic . fmap fromBranchcheckerWarning

branchcheckProgram :: TypedProgram -> Checker ()
branchcheckProgram (TProgram (TData d :@ _) (TROData rd :@ _) (TUData ud :@ _) (TCode stts :@ _)) = do
  forM_ stts registerAllLabelsAsVertices
  forM_ stts registerEdges

  checkJumpgraphForConsistency

dummyPos :: Position
dummyPos = Position (1, 1) (1, 2) ""

registerAllLabelsAsVertices :: Located TypedStatement -> Checker ()
registerAllLabelsAsVertices (TLabel name :@ _) = do
  graph <- gets snd
  let newGraph =
        if name == ("main" :@ dummyPos)
        then graph `Graph.overlay` (("_start" :@ dummyPos)-<Call>-name)
        else graph
          -- Register the edge from "_start" to "main" if our AST contains a `main` label.
          -- Else it causes problems like not branch-checking on an empty file.
  let ngraph = newGraph `Graph.overlay` Graph.vertex name
  modify (second (const ngraph))
registerAllLabelsAsVertices _ = pure ()

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
        newGraph =
          if not (null parents)
          then foldl' (\ g p -> Graph.overlay g (label-<Ret>-p)) graph parents
          else graph `Graph.overlay` (label-<Ret>-("@_unknown" :@ dummyPos))
    modify (second (const newGraph))

    pure ()
  JMP (Name toLabel :@ _) _ -> do
    (lbl, graph) <- get
    let Just currentLabel = lbl
        newGraph = graph `Graph.overlay` (currentLabel-<Jump>-toLabel)
    modify (second (const newGraph))

    pure ()
  CALL (Name toLabel :@ _) _ -> do
    (lbl, graph) <- get
    let Just currentLabel = lbl
        newGraph = graph `Graph.overlay` (currentLabel-<Call>-toLabel)
    modify (second (const newGraph))
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
  checkNoControlFlowLeak

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

checkNoControlFlowLeak :: Checker ()
checkNoControlFlowLeak = do
  graph <- gets snd
  let vertices = Set.filter (/= ("@_unknown" :@ dummyPos)) $ Graph.vertexSet graph
  forM_ vertices \ v -> do
    let succs = Graph.postSet v graph

    when (null succs) do
      throwError (ControlFlowLeak v v)
    -- TODO: we will have to also check whether it MAY leak, for example on a conditional jump.
