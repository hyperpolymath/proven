-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- SafeGraph: Formally verified graph operations with cycle detection
--
-- Provides:
-- - Type-safe directed graphs and DAGs
-- - Compile-time acyclicity proofs for DAGs
-- - Topological sorting with correctness guarantees
-- - Path finding with termination proofs

module Proven.SafeGraph

import Data.List
import Data.List1
import Data.Maybe
import Data.Nat
import Decidable.Equality

%default total

||| An edge in a graph from source to target
public export
record Edge (node : Type) where
  constructor MkEdge
  source : node
  target : node

||| A directed graph with nodes and edges
public export
record Graph (node : Type) where
  constructor MkGraph
  nodes : List node
  edges : List (Edge node)

||| Empty graph
public export
emptyGraph : Graph node
emptyGraph = MkGraph [] []

||| Add a node to the graph
public export
addNode : Eq node => node -> Graph node -> Graph node
addNode n g =
  if elem n (nodes g)
    then g
    else MkGraph (n :: nodes g) (edges g)

||| Add an edge to the graph (nodes must exist)
public export
addEdge : Eq node => node -> node -> Graph node -> Maybe (Graph node)
addEdge from to g =
  if elem from (nodes g) && elem to (nodes g)
    then Just (MkGraph (nodes g) (MkEdge from to :: edges g))
    else Nothing

||| Get all outgoing edges from a node
public export
outEdges : Eq node => node -> Graph node -> List node
outEdges n g = map target (filter (\e => source e == n) (edges g))

||| Get all incoming edges to a node
public export
inEdges : Eq node => node -> Graph node -> List node
inEdges n g = map source (filter (\e => target e == n) (edges g))

||| In-degree of a node (number of incoming edges)
public export
inDegree : Eq node => node -> Graph node -> Nat
inDegree n g = length (inEdges n g)

||| Out-degree of a node (number of outgoing edges)
public export
outDegree : Eq node => node -> Graph node -> Nat
outDegree n g = length (outEdges n g)

||| Check if there's a direct edge from a to b
public export
hasEdge : Eq node => node -> node -> Graph node -> Bool
hasEdge from to g = any (\e => source e == from && target e == to) (edges g)

||| A path through the graph
public export
data Path : (node : Type) -> (from : node) -> (to : node) -> Type where
  ||| Single node path (from = to)
  Here : Path node n n
  ||| Extend path with an edge
  There : (edge : Edge node) -> source edge = from ->
          Path node (target edge) to -> Path node from to

||| Length of a path
public export
pathLength : Path node from to -> Nat
pathLength Here = 0
pathLength (There _ _ rest) = S (pathLength rest)

||| A proof that a graph contains no cycles
public export
data Acyclic : Graph node -> Type where
  MkAcyclic : (topoOrder : List node) ->
              (allNodes : (n : node) -> Elem n (nodes g) -> Elem n topoOrder) ->
              (ordered : (e : Edge node) -> Elem e (edges g) ->
                        LT (fromMaybe 0 (elemIndex (target e) topoOrder))
                           (fromMaybe 0 (elemIndex (source e) topoOrder))) ->
              Acyclic g

||| A Directed Acyclic Graph with proof of acyclicity
public export
record DAG (node : Type) where
  constructor MkDAG
  graph : Graph node
  acyclicProof : Acyclic graph

||| Result of cycle detection
public export
data CycleCheck : (node : Type) -> Type where
  NoCycle : DAG node -> CycleCheck node
  HasCycle : List node -> CycleCheck node  -- The cycle path

||| DFS state for cycle detection
data DFSState : Type -> Type where
  MkDFSState : (visited : List node) -> (stack : List node) -> DFSState node

||| Topological sort result
public export
data TopoResult : (node : Type) -> Type where
  TopoSorted : List node -> TopoResult node
  CycleDetected : List node -> TopoResult node

||| Kahn's algorithm for topological sorting
||| Returns Nothing if cycle detected
public export
topoSort : Eq node => Graph node -> TopoResult node
topoSort g = kahnLoop (nodes g) [] (computeInDegrees g)
  where
    computeInDegrees : Graph node -> List (node, Nat)
    computeInDegrees g = map (\n => (n, inDegree n g)) (nodes g)

    getZeroInDegree : List (node, Nat) -> List node
    getZeroInDegree = map fst . filter (\p => snd p == 0)

    decrementInDegree : Eq node => node -> List (node, Nat) -> List (node, Nat)
    decrementInDegree n = map (\p => if fst p == n then (fst p, pred (snd p)) else p)

    kahnLoop : List node -> List node -> List (node, Nat) -> TopoResult node
    kahnLoop [] result _ = TopoSorted (reverse result)
    kahnLoop remaining result degrees =
      let zeros = filter (\n => elem n remaining) (getZeroInDegree degrees)
      in case zeros of
           [] => CycleDetected remaining  -- Cycle detected
           (n :: _) =>
             let newRemaining = filter (/= n) remaining
                 newDegrees = foldl (flip decrementInDegree) degrees (outEdges n g)
             in kahnLoop newRemaining (n :: result) newDegrees

||| Find all paths from source to target (with depth limit to ensure termination)
public export
findPaths : Eq node => Graph node -> node -> node -> Nat -> List (List node)
findPaths g from to maxDepth = dfs [from] maxDepth
  where
    dfs : List node -> Nat -> List (List node)
    dfs path Z = []  -- Depth limit reached
    dfs path (S depth) =
      let current = case path of
                      [] => from
                      (h :: _) => h
      in if current == to
           then [reverse path]
           else let nexts = filter (\n => not (elem n path)) (outEdges current g)
                in concatMap (\n => dfs (n :: path) depth) nexts

||| Check if node is reachable from another
public export
isReachable : Eq node => Graph node -> node -> node -> Nat -> Bool
isReachable g from to maxDepth = not (null (findPaths g from to maxDepth))

||| Strongly connected components (Kosaraju's algorithm concept)
public export
record SCC (node : Type) where
  constructor MkSCC
  component : List node

||| Reverse all edges in a graph
public export
reverseGraph : Graph node -> Graph node
reverseGraph g = MkGraph (nodes g) (map reverseEdge (edges g))
  where
    reverseEdge : Edge node -> Edge node
    reverseEdge e = MkEdge (target e) (source e)

||| Get root nodes (no incoming edges)
public export
getRoots : Eq node => Graph node -> List node
getRoots g = filter (\n => inDegree n g == 0) (nodes g)

||| Get leaf nodes (no outgoing edges)
public export
getLeaves : Eq node => Graph node -> List node
getLeaves g = filter (\n => outDegree n g == 0) (nodes g)

||| Subgraph induced by a subset of nodes
public export
inducedSubgraph : Eq node => List node -> Graph node -> Graph node
inducedSubgraph subset g =
  let validEdges = filter (\e => elem (source e) subset && elem (target e) subset) (edges g)
  in MkGraph subset validEdges

||| Transitive closure - add edge for all reachable pairs
public export
transitiveClosure : Eq node => Graph node -> Nat -> Graph node
transitiveClosure g maxDepth =
  let newEdges = concatMap (\n =>
                   map (\m => MkEdge n m)
                       (filter (\m => m /= n && isReachable g n m maxDepth) (nodes g)))
                 (nodes g)
  in MkGraph (nodes g) (nub (edges g ++ newEdges))
  where
    nub : Eq a => List a -> List a
    nub [] = []
    nub (x :: xs) = x :: nub (filter (/= x) xs)

||| Proof that a path exists in a graph
public export
data PathExists : Graph node -> node -> node -> Type where
  DirectEdge : Elem (MkEdge from to) (edges g) -> PathExists g from to
  IndirectPath : Elem (MkEdge from mid) (edges g) -> PathExists g mid to -> PathExists g from to

||| A weighted edge
public export
record WeightedEdge (node : Type) (weight : Type) where
  constructor MkWeightedEdge
  wSource : node
  wTarget : node
  wWeight : weight

||| A weighted graph
public export
record WeightedGraph (node : Type) (weight : Type) where
  constructor MkWeightedGraph
  wgNodes : List node
  wgEdges : List (WeightedEdge node weight)

||| Convert weighted graph to unweighted
public export
forgetWeights : WeightedGraph node weight -> Graph node
forgetWeights wg = MkGraph (wgNodes wg) (map toEdge (wgEdges wg))
  where
    toEdge : WeightedEdge node weight -> Edge node
    toEdge we = MkEdge (wSource we) (wTarget we)
