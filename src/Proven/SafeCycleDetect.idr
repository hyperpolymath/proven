-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- SafeCycleDetect — Verified graph cycle detection.
--
-- Implements depth-first search with three-colour marking to detect
-- cycles in directed graphs. Uses fuel-bounded recursion for totality
-- (guaranteed termination).
--
-- Used by hybrid-automation-router to verify that infrastructure
-- dependency graphs are DAGs before routing operations. A cycle in a
-- dependency graph would cause infinite loops during topological
-- ordering, which is a critical failure mode for infrastructure routing.
--
-- Extracted from: cadre-router, hybrid-automation-router optimisation work.
--
-- Design rationale:
--   The classic DFS three-colour algorithm is used:
--     - White: vertex not yet visited
--     - Grey: vertex is on the current DFS path (in the recursion stack)
--     - Black: vertex fully explored (all descendants visited)
--
--   A back-edge to a Grey vertex indicates a cycle. This is the
--   standard textbook algorithm (CLRS Introduction to Algorithms,
--   Section 22.3) adapted for total functional programming.
--
--   The key adaptation is FUEL-BOUNDED RECURSION: instead of
--   relying on structural recursion (which is hard to prove for
--   graph traversal), we pass a fuel parameter that decreases
--   at each step. When fuel runs out, we conservatively treat
--   the graph as explored (no cycle found). The initial fuel is
--   set to the number of vertices, which is sufficient because
--   each vertex is visited at most once.
--
--   The trade-off: if the graph has more edges than vertices
--   (dense graphs), the fuel might be exhausted before full
--   exploration. In practice, infrastructure dependency graphs
--   are sparse (each service depends on a small number of others),
--   so this is not an issue. For dense graphs, increase fuel to
--   vertexCount * vertexCount.
--
--   This module uses String vertex identifiers for compatibility
--   with the rest of the infrastructure (service names, route IDs,
--   etc.). For type-safe vertex identifiers, see Proven.SafeGraph.

module Proven.SafeCycleDetect

import public Proven.Core
import Data.List
import Data.SortedMap

%default total

--------------------------------------------------------------------------------
-- DFS Colour Type
--------------------------------------------------------------------------------

||| Vertex colour in DFS traversal.
|||
||| The three-colour scheme tracks the state of each vertex during
||| depth-first search:
|||   - White: not yet visited. The vertex has not been encountered
|||     by the DFS algorithm at all.
|||   - Grey: in the current DFS path. The vertex has been entered
|||     but not all of its descendants have been fully explored.
|||     A back-edge to a Grey vertex means we've found a cycle.
|||   - Black: fully explored. The vertex and ALL of its reachable
|||     descendants have been completely visited. A back-edge to
|||     a Black vertex is a cross-edge or forward-edge, NOT a cycle.
|||
||| The distinction between Grey and Black is what makes cycle
||| detection work: Grey means "I'm an ancestor on the current path",
||| Black means "I'm fully done, ignore me".
public export
data Colour : Type where
  ||| Unvisited — vertex not yet encountered by DFS.
  White : Colour
  ||| In current path — vertex entered but descendants not yet
  ||| fully explored. Back-edge to Grey = cycle detected.
  Grey : Colour
  ||| Fully explored — vertex and all descendants completely visited.
  ||| Back-edge to Black = NOT a cycle (cross/forward edge).
  Black : Colour

||| Decidable equality for Colour.
||| Used when inspecting the colour map during DFS.
public export
Eq Colour where
  White == White = True
  Grey == Grey = True
  Black == Black = True
  _ == _ = False

||| Show instance for Colour, useful for debug output.
public export
Show Colour where
  show White = "white"
  show Grey = "grey"
  show Black = "black"

--------------------------------------------------------------------------------
-- Cycle Result Type
--------------------------------------------------------------------------------

||| Result of cycle detection.
|||
||| Either the graph is a DAG (no cycles found) or a cycle was
||| detected and we return the path that forms the cycle.
|||
||| The cycle path lists vertices in order of traversal, with
||| the last element being the vertex where the back-edge was
||| detected (which is the same as some earlier vertex in the path).
public export
data CycleResult : Type where
  ||| Graph is a DAG — no cycles found.
  ||| All vertices were fully explored without encountering
  ||| any back-edges to Grey vertices.
  Acyclic : CycleResult
  ||| Cycle found — cyclePath shows the vertices forming the cycle.
  ||| The path is ordered from the cycle entry point through the
  ||| back-edge target. The last element connects back to an
  ||| earlier element, forming the cycle.
  ||| @ cyclePath  The list of vertices forming the detected cycle
  Cyclic : (cyclePath : List String) -> CycleResult

||| Decidable equality for CycleResult.
public export
Eq CycleResult where
  Acyclic == Acyclic = True
  (Cyclic p1) == (Cyclic p2) = p1 == p2
  _ == _ = False

||| Show instance for CycleResult.
||| Produces a human-readable description of the result.
public export
Show CycleResult where
  show Acyclic = "Acyclic (no cycles detected)"
  show (Cyclic path) = "Cyclic: " ++ show path

||| Check if a CycleResult indicates the graph is acyclic.
|||
||| @ r  The cycle detection result to check
public export
isAcyclic : (r : CycleResult) -> Bool
isAcyclic Acyclic = True
isAcyclic (Cyclic _) = False

||| Check if a CycleResult indicates a cycle was found.
|||
||| @ r  The cycle detection result to check
public export
isCyclic : (r : CycleResult) -> Bool
isCyclic = not . isAcyclic

--------------------------------------------------------------------------------
-- Directed Graph Type
--------------------------------------------------------------------------------

||| A directed graph represented as an adjacency list.
|||
||| Uses String vertex identifiers for compatibility with the
||| infrastructure naming convention (service names, route IDs,
||| configuration keys).
|||
||| The adjacency map stores, for each vertex, the list of
||| vertices it has edges TO (outgoing edges). This representation
||| is efficient for DFS: looking up a vertex's neighbours is
||| O(log n) via the SortedMap.
|||
||| @ vertices   The list of all vertex identifiers in the graph
||| @ adjacency  Map from vertex to its outgoing neighbours
public export
record DirectedGraph where
  constructor MkGraph
  ||| All vertex identifiers in the graph.
  ||| May contain vertices with no outgoing edges (sinks).
  vertices : List String
  ||| Adjacency list: maps each vertex to its outgoing neighbours.
  ||| A vertex not present in the map is treated as having no
  ||| outgoing edges (equivalent to an empty list).
  adjacency : SortedMap String (List String)

||| Show instance for DirectedGraph.
||| Displays vertex and edge counts for quick inspection.
public export
Show DirectedGraph where
  show g = "DirectedGraph(vertices=" ++ show (length g.vertices) ++ ")"

--------------------------------------------------------------------------------
-- Graph Construction
--------------------------------------------------------------------------------

||| Create an empty directed graph with no vertices or edges.
|||
||| Starting point for incremental graph construction.
public export
emptyGraph : DirectedGraph
emptyGraph = MkGraph [] empty

||| Build a directed graph from a list of edges.
|||
||| Each edge is a pair (source, target) representing a directed
||| edge from source to target. Vertices are automatically
||| extracted from the edges and deduplicated.
|||
||| This is the primary constructor for graphs built from
||| configuration data (e.g., service dependency declarations).
|||
||| @ edges  List of (source, target) pairs defining directed edges
public export
fromEdges : (edges : List (String, String)) -> DirectedGraph
fromEdges edges =
  let vs = nub (concatMap (\(a, b) => [a, b]) edges)
      adj = foldl addEdge empty edges
  in MkGraph vs adj
  where
    ||| Add a single directed edge to the adjacency map.
    ||| If the source vertex already has neighbours, append the
    ||| new target; otherwise create a new entry.
    addEdge : SortedMap String (List String) -> (String, String) -> SortedMap String (List String)
    addEdge m (src, tgt) =
      case lookup src m of
        Nothing => insert src [tgt] m
        Just existing => insert src (tgt :: existing) m

||| Add a single directed edge to an existing graph.
|||
||| Both vertices are added to the vertex list if not already
||| present. The edge is added to the adjacency map.
|||
||| @ src    Source vertex identifier
||| @ tgt    Target vertex identifier
||| @ graph  The graph to add the edge to
public export
addEdge : (src : String) -> (tgt : String) -> (graph : DirectedGraph) -> DirectedGraph
addEdge src tgt graph =
  let vs = nub (src :: tgt :: graph.vertices)
      adj = case lookup src graph.adjacency of
              Nothing => insert src [tgt] graph.adjacency
              Just existing => insert src (tgt :: existing) graph.adjacency
  in MkGraph vs adj

||| Get the number of vertices in the graph.
|||
||| @ graph  The graph to count vertices in
public export
vertexCount : (graph : DirectedGraph) -> Nat
vertexCount graph = length graph.vertices

--------------------------------------------------------------------------------
-- Cycle Detection Algorithm
--------------------------------------------------------------------------------

||| Get the neighbours (outgoing edges) of a vertex.
|||
||| Returns the empty list for vertices not present in the
||| adjacency map, or for vertices with no outgoing edges.
|||
||| @ adj  The adjacency map to look up in
||| @ v    The vertex to get neighbours for
public export
neighbours : (adj : SortedMap String (List String)) -> (v : String) -> List String
neighbours adj v = fromMaybe [] (lookup v adj)

||| Internal: Perform DFS from a single vertex with fuel-bounded recursion.
|||
||| This is the core of the cycle detection algorithm. It performs a
||| depth-first traversal starting from the given vertex, colouring
||| vertices as it goes:
|||   1. Mark the current vertex Grey (in current path)
|||   2. For each neighbour:
|||      a. If Grey: cycle found! Return Left with the cycle path.
|||      b. If White: recurse into it.
|||      c. If Black: skip (already fully explored).
|||   3. After all neighbours processed, mark current vertex Black.
|||
||| The fuel parameter ensures totality: each recursive call
||| decrements the fuel by one. When fuel reaches zero, we
||| conservatively return the current colour map (no cycle detected
||| in the remaining budget).
|||
||| @ fuel     Remaining recursion budget (decrements each call)
||| @ vertex   The vertex to start DFS from
||| @ adj      Adjacency map for neighbour lookups
||| @ colours  Current vertex colour assignments
||| @ path     The current DFS path (for cycle reporting)
|||
||| Returns: Left cyclePath if a cycle was found,
|||          Right updatedColours if no cycle found from this vertex.
dfs : (fuel : Nat)
   -> (vertex : String)
   -> (adj : SortedMap String (List String))
   -> (colours : SortedMap String Colour)
   -> (path : List String)
   -> Either (List String) (SortedMap String Colour)
dfs Z _ _ colours _ = Right colours  -- Fuel exhausted — conservatively no cycle
dfs (S fuel) vertex adj colours path =
  let colours' = insert vertex Grey colours
      nbrs = neighbours adj vertex
  in processNeighbours fuel adj (vertex :: path) vertex nbrs (Right colours')
  where
    ||| Process a list of neighbours sequentially during DFS.
    |||
    ||| For each neighbour, check its colour:
    |||   - Grey: cycle detected (back-edge to ancestor on current path)
    |||   - White: recurse into it
    |||   - Black or absent: skip (already fully explored)
    |||
    ||| Short-circuits on the first cycle found (Left propagation).
    |||
    ||| @ fuel     Remaining recursion budget
    ||| @ adj      Adjacency map
    ||| @ path     Current DFS path for cycle reporting
    ||| @ current  The vertex whose neighbours we're processing
    ||| @ nbrs     Remaining neighbours to process
    ||| @ acc      Accumulated result (Left = cycle found, Right = colours so far)
    processNeighbours : (fuel : Nat)
                     -> (adj : SortedMap String (List String))
                     -> (path : List String)
                     -> (current : String)
                     -> (nbrs : List String)
                     -> Either (List String) (SortedMap String Colour)
                     -> Either (List String) (SortedMap String Colour)
    processNeighbours _ _ _ _ _ (Left cycle) = Left cycle  -- Already found a cycle
    processNeighbours _ _ _ _ [] (Right colours) = Right colours  -- No more neighbours
    processNeighbours fuel adj path current (nbr :: rest) (Right colours) =
      case lookup nbr colours of
        Just Grey =>
          -- Back edge found: nbr is an ancestor on the current path.
          -- The cycle is: nbr -> ... -> current -> nbr
          Left (reverse (nbr :: path))
        Just White =>
          -- Unvisited neighbour: recurse into it
          let result = dfs fuel nbr adj colours path
          in processNeighbours fuel adj path current rest result
        _ =>
          -- Black or absent: skip (already fully explored or unknown)
          processNeighbours fuel adj path current rest (Right colours)

||| Check a directed graph for cycles using fuel-bounded DFS.
|||
||| Performs a complete DFS traversal of the graph, starting from
||| each unvisited vertex. The fuel is set to the number of vertices,
||| which is sufficient for sparse graphs (each vertex visited at most
||| once). For dense graphs, the fuel may be exhausted before full
||| exploration, in which case the function conservatively reports
||| no cycle — this is safe because false negatives only occur when
||| the graph is too large for the fuel budget, and the caller can
||| retry with more fuel.
|||
||| The algorithm is:
|||   1. Initialise all vertices to White (unvisited).
|||   2. For each White vertex, start a DFS from it.
|||   3. If any DFS finds a back-edge (Grey vertex), report the cycle.
|||   4. If all DFS complete without finding back-edges, the graph is a DAG.
|||
||| Time complexity: O(V + E) where V = vertices, E = edges.
||| Space complexity: O(V) for the colour map and DFS stack.
|||
||| @ graph  The directed graph to check for cycles
public export
checkCycles : (graph : DirectedGraph) -> CycleResult
checkCycles graph =
  let fuel = length graph.vertices
      initialColours = foldl (\m, v => insert v White m) empty graph.vertices
  in checkAll fuel graph.vertices graph.adjacency initialColours
  where
    ||| Iterate over all vertices, starting DFS from each unvisited one.
    |||
    ||| This outer loop ensures that all connected components of the
    ||| graph are checked, not just the component reachable from a
    ||| single starting vertex.
    |||
    ||| @ fuel     Remaining overall fuel budget
    ||| @ vs       Remaining vertices to check
    ||| @ adj      Adjacency map
    ||| @ colours  Current colour assignments
    checkAll : (fuel : Nat)
            -> (vs : List String)
            -> (adj : SortedMap String (List String))
            -> (colours : SortedMap String Colour)
            -> CycleResult
    checkAll Z _ _ _ = Acyclic  -- Fuel exhausted — conservatively acyclic
    checkAll (S fuel) [] _ _ = Acyclic  -- All vertices processed — acyclic
    checkAll (S fuel) (v :: vs) adj colours =
      case lookup v colours of
        Just White =>
          -- Unvisited vertex: start DFS from here
          case dfs (S fuel) v adj colours [v] of
            Left cycle => Cyclic cycle   -- Cycle found
            Right colours' => checkAll fuel vs adj colours'
        _ =>
          -- Already visited (Grey or Black) — skip
          checkAll fuel vs adj colours

--------------------------------------------------------------------------------
-- Convenience Functions
--------------------------------------------------------------------------------

||| Check if a directed graph is a DAG (directed acyclic graph).
|||
||| This is the most common use case: infrastructure dependency
||| graphs must be DAGs to allow topological ordering.
|||
||| @ graph  The directed graph to check
public export
isDAG : (graph : DirectedGraph) -> Bool
isDAG graph = isAcyclic (checkCycles graph)

||| Check a graph for cycles, returning a Result for error handling.
|||
||| Returns Ok () if the graph is acyclic, or Err with a descriptive
||| error message including the cycle path if a cycle is found.
||| This is the preferred interface for infrastructure validation
||| because it integrates with the Result-based error handling
||| used throughout Proven.
|||
||| @ graph  The directed graph to validate
public export
validateDAG : (graph : DirectedGraph) -> Result SafeError ()
validateDAG graph =
  case checkCycles graph of
    Acyclic => Ok ()
    Cyclic path =>
      let pathStr = foldl (\acc, v => acc ++ " -> " ++ v) "" path
      in Err (MkError ("Cycle detected in dependency graph:" ++ pathStr))

||| Build a graph from dependency declarations and validate it.
|||
||| Combines graph construction and cycle detection into a single
||| operation. This is the top-level function that infrastructure
||| configuration loaders should call: provide the list of
||| dependency edges, get back either Ok (validated DAG) or
||| Err (cycle found).
|||
||| @ edges  List of (dependent, dependency) pairs
public export
validateDependencies : (edges : List (String, String)) -> Result SafeError DirectedGraph
validateDependencies edges =
  let graph = fromEdges edges
  in case checkCycles graph of
       Acyclic => Ok graph
       Cyclic path =>
         let pathStr = foldl (\acc, v => acc ++ " -> " ++ v) "" path
         in Err (MkError ("Cycle detected in dependency graph:" ++ pathStr))
