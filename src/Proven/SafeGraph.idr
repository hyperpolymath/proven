-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeGraph - Safe graph data structure operations
|||
||| This module provides safe graph operations with
||| bounds checking and cycle detection.
module Proven.SafeGraph

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Graph Types
--------------------------------------------------------------------------------

||| Edge in a graph
public export
record Edge v w where
  constructor MkEdge
  source : v
  target : v
  weight : w

||| A directed graph with vertices and weighted edges
public export
record Graph v w where
  constructor MkGraph
  vertices : List v
  edges : List (Edge v w)

||| An undirected graph (edges go both ways)
public export
record UndirectedGraph v w where
  constructor MkUndirected
  graph : Graph v w

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

||| Create an empty graph
public export
empty : Graph v w
empty = MkGraph [] []

||| Create a graph from a list of vertices
public export
fromVertices : List v -> Graph v w
fromVertices vs = MkGraph vs []

||| Add a vertex to the graph
public export
addVertex : Eq v => v -> Graph v w -> Graph v w
addVertex v g =
  if any (== v) g.vertices
    then g
    else MkGraph (v :: g.vertices) g.edges

||| Add an edge to the graph
public export
addEdge : Eq v => v -> v -> w -> Graph v w -> Graph v w
addEdge src tgt weight g =
  let g' = addVertex src (addVertex tgt g)
  in MkGraph g'.vertices (MkEdge src tgt weight :: g'.edges)

||| Remove a vertex and all its edges
public export
removeVertex : Eq v => v -> Graph v w -> Graph v w
removeVertex v g =
  MkGraph (filter (/= v) g.vertices)
          (filter (\e => e.source /= v && e.target /= v) g.edges)

||| Remove an edge
public export
removeEdge : Eq v => v -> v -> Graph v w -> Graph v w
removeEdge src tgt g =
  MkGraph g.vertices (filter (\e => not (e.source == src && e.target == tgt)) g.edges)

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

||| Check if vertex exists in graph
public export
hasVertex : Eq v => v -> Graph v w -> Bool
hasVertex v g = any (== v) g.vertices

||| Check if edge exists
public export
hasEdge : Eq v => v -> v -> Graph v w -> Bool
hasEdge src tgt g = any (\e => e.source == src && e.target == tgt) g.edges

||| Get all neighbors of a vertex (outgoing edges)
public export
neighbors : Eq v => v -> Graph v w -> List v
neighbors v g = map target (filter (\e => e.source == v) g.edges)

||| Get predecessors of a vertex (incoming edges)
public export
predecessors : Eq v => v -> Graph v w -> List v
predecessors v g = map source (filter (\e => e.target == v) g.edges)

||| Get out-degree of a vertex
public export
outDegree : Eq v => v -> Graph v w -> Nat
outDegree v g = length (filter (\e => e.source == v) g.edges)

||| Get in-degree of a vertex
public export
inDegree : Eq v => v -> Graph v w -> Nat
inDegree v g = length (filter (\e => e.target == v) g.edges)

||| Get total degree of a vertex
public export
degree : Eq v => v -> Graph v w -> Nat
degree v g = outDegree v g + inDegree v g

||| Get edge weight
public export
getWeight : Eq v => v -> v -> Graph v w -> Maybe w
getWeight src tgt g =
  case find (\e => e.source == src && e.target == tgt) g.edges of
    Nothing => Nothing
    Just e => Just e.weight

--------------------------------------------------------------------------------
-- Graph Properties
--------------------------------------------------------------------------------

||| Number of vertices
public export
vertexCount : Graph v w -> Nat
vertexCount g = length g.vertices

||| Number of edges
public export
edgeCount : Graph v w -> Nat
edgeCount g = length g.edges

||| Check if graph is empty
public export
isEmpty : Graph v w -> Bool
isEmpty g = isNil g.vertices

||| Get all edges from a vertex
public export
edgesFrom : Eq v => v -> Graph v w -> List (Edge v w)
edgesFrom v g = filter (\e => e.source == v) g.edges

||| Get all edges to a vertex
public export
edgesTo : Eq v => v -> Graph v w -> List (Edge v w)
edgesTo v g = filter (\e => e.target == v) g.edges

--------------------------------------------------------------------------------
-- Path Finding (BFS)
--------------------------------------------------------------------------------

||| Find a path between two vertices using BFS
||| Returns Nothing if no path exists
public export
findPath : Eq v => v -> v -> Graph v w -> Maybe (List v)
findPath start end g =
  if not (hasVertex start g) || not (hasVertex end g)
    then Nothing
    else bfs [(start, [start])] []
  where
    bfs : List (v, List v) -> List v -> Maybe (List v)
    bfs [] _ = Nothing
    bfs ((current, path) :: queue) visited =
      if current == end
        then Just (reverse path)
        else if any (== current) visited
          then bfs queue visited
          else let newVisited = current :: visited
                   nextNodes = filter (\n => not (any (== n) newVisited)) (neighbors current g)
                   newQueue = queue ++ map (\n => (n, n :: path)) nextNodes
               in bfs newQueue newVisited

||| Check if two vertices are connected
public export
isConnected : Eq v => v -> v -> Graph v w -> Bool
isConnected start end g =
  case findPath start end g of
    Nothing => False
    Just _ => True

--------------------------------------------------------------------------------
-- Cycle Detection
--------------------------------------------------------------------------------

||| Detect if graph has a cycle (using DFS)
public export
hasCycle : Eq v => Graph v w -> Bool
hasCycle g = any (checkFromVertex []) g.vertices
  where
    dfs : List v -> List v -> v -> Bool
    dfs visited stack current =
      if any (== current) stack
        then True  -- Back edge found, cycle exists
        else if any (== current) visited
          then False  -- Already fully explored
          else let newStack = current :: stack
                   succs = neighbors current g
               in any (dfs visited newStack) succs
    
    checkFromVertex : List v -> v -> Bool
    checkFromVertex visited v = dfs visited [] v

--------------------------------------------------------------------------------
-- Topological Sort
--------------------------------------------------------------------------------

||| Topological sort (returns Nothing if graph has cycles)
public export
topologicalSort : Eq v => Graph v w -> Maybe (List v)
topologicalSort g =
  if hasCycle g
    then Nothing
    else Just (topoSort g.vertices [] [])
  where
    topoSort : List v -> List v -> List v -> List v
    topoSort [] _ result = result
    topoSort (v :: vs) visited result =
      if any (== v) visited
        then topoSort vs visited result
        else let (newVisited, newResult) = visit v visited result
             in topoSort vs newVisited newResult
    
    visit : v -> List v -> List v -> (List v, List v)
    visit v visited result =
      if any (== v) visited
        then (visited, result)
        else let newVisited = v :: visited
                 succs = neighbors v g
                 (finalVisited, afterSuccs) = foldl (\(vis, res), s => visit s vis res) (newVisited, result) succs
             in (finalVisited, v :: afterSuccs)

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Show v => Show (Graph v w) where
  show g = "Graph(vertices=" ++ show (length g.vertices) ++
           ", edges=" ++ show (length g.edges) ++ ")"

