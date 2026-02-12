-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeGraph operations
|||
||| This module exports graph validation and statistics helpers to the C ABI
||| via Idris2's RefC backend. All functions are proven total.
|||
||| Return conventions:
||| - Count → Int (vertices, edges, degree)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
||| - Statistics → Double (density, clustering coefficient)
|||
||| CRITICAL: Graph operations must validate bounds and detect cycles
|||           to prevent infinite loops and memory exhaustion.
|||
||| Graph Types:
||| - Directed graph: Edges have direction (src → tgt)
||| - Undirected graph: Edges bidirectional (src ↔ tgt)
||| - Weighted graph: Edges have numeric weights
|||
||| Graph Properties:
||| - Degree: Number of edges incident to vertex
|||   - In-degree: Incoming edges (predecessors)
|||   - Out-degree: Outgoing edges (successors)
|||   - Total degree: In + out (directed) or 2×edges (undirected)
||| - Density: Ratio of actual edges to possible edges
|||   - Sparse: density < 0.5
|||   - Dense: density ≥ 0.5
||| - Complete: All vertices connected (density = 1.0)
|||
||| Algorithms:
||| - BFS: Breadth-first search for shortest paths
||| - DFS: Depth-first search for cycle detection
||| - Topological sort: Linear ordering (DAGs only)
|||
||| NOTE: Polymorphic graph data managed in Zig.
|||       These helpers validate operations and calculate statistics.
module Proven.FFI.SafeGraph

import Proven.SafeGraph
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- Graph Size and Bounds
--------------------------------------------------------------------------------

export
proven_idris_graph_vertex_count : Int -> Int
proven_idris_graph_vertex_count count = count

export
proven_idris_graph_edge_count : Int -> Int
proven_idris_graph_edge_count count = count

export
proven_idris_graph_is_empty : Int -> Int
proven_idris_graph_is_empty vertexCount =
  encodeBool (vertexCount == 0)

export
proven_idris_graph_max_edges_directed : Int -> Int
proven_idris_graph_max_edges_directed vertexCount =
  -- n × (n - 1) for directed graph (no self-loops)
  vertexCount * (vertexCount - 1)

export
proven_idris_graph_max_edges_undirected : Int -> Int
proven_idris_graph_max_edges_undirected vertexCount =
  -- n × (n - 1) / 2 for undirected graph
  (vertexCount * (vertexCount - 1)) `div` 2

export
proven_idris_graph_is_valid_vertex_index : Int -> Int -> Int
proven_idris_graph_is_valid_vertex_index index vertexCount =
  encodeBool (index >= 0 && index < vertexCount)

export
proven_idris_graph_is_valid_edge_count : Int -> Int -> Int
proven_idris_graph_is_valid_edge_count edgeCount maxEdges =
  encodeBool (edgeCount >= 0 && edgeCount <= maxEdges)

--------------------------------------------------------------------------------
-- Degree Operations
--------------------------------------------------------------------------------

export
proven_idris_graph_out_degree : Int -> Int
proven_idris_graph_out_degree count = count

export
proven_idris_graph_in_degree : Int -> Int
proven_idris_graph_in_degree count = count

export
proven_idris_graph_total_degree : Int -> Int -> Int
proven_idris_graph_total_degree inDegree outDegree = inDegree + outDegree

export
proven_idris_graph_average_degree : Int -> Int -> Double
proven_idris_graph_average_degree totalDegree vertexCount =
  if vertexCount == 0 then 0.0
  else cast totalDegree / cast vertexCount

export
proven_idris_graph_max_degree : Int -> Int
proven_idris_graph_max_degree vertexCount =
  if vertexCount <= 0 then 0 else vertexCount - 1

export
proven_idris_graph_is_sink : Int -> Int
proven_idris_graph_is_sink outDegree =
  encodeBool (outDegree == 0)

export
proven_idris_graph_is_source : Int -> Int
proven_idris_graph_is_source inDegree =
  encodeBool (inDegree == 0)

export
proven_idris_graph_is_isolated : Int -> Int -> Int
proven_idris_graph_is_isolated inDegree outDegree =
  encodeBool (inDegree == 0 && outDegree == 0)

--------------------------------------------------------------------------------
-- Graph Density
--------------------------------------------------------------------------------

export
proven_idris_graph_density_directed : Int -> Int -> Double
proven_idris_graph_density_directed edgeCount vertexCount =
  if vertexCount <= 1 then 0.0
  else let maxEdges = vertexCount * (vertexCount - 1)
       in cast edgeCount / cast maxEdges

export
proven_idris_graph_density_undirected : Int -> Int -> Double
proven_idris_graph_density_undirected edgeCount vertexCount =
  if vertexCount <= 1 then 0.0
  else let maxEdges = (vertexCount * (vertexCount - 1)) `div` 2
       in cast edgeCount / cast maxEdges

export
proven_idris_graph_is_sparse : Double -> Int
proven_idris_graph_is_sparse density =
  encodeBool (density < 0.5)

export
proven_idris_graph_is_dense : Double -> Int
proven_idris_graph_is_dense density =
  encodeBool (density >= 0.5)

export
proven_idris_graph_is_complete : Int -> Int -> Int
proven_idris_graph_is_complete edgeCount maxEdges =
  encodeBool (edgeCount == maxEdges)

--------------------------------------------------------------------------------
-- Path Finding Helpers
--------------------------------------------------------------------------------

export
proven_idris_graph_max_path_length : Int -> Int
proven_idris_graph_max_path_length vertexCount =
  if vertexCount <= 0 then 0 else vertexCount - 1

export
proven_idris_graph_is_valid_path_length : Int -> Int -> Int
proven_idris_graph_is_valid_path_length pathLength vertexCount =
  encodeBool (pathLength >= 0 && pathLength < vertexCount)

export
proven_idris_graph_bfs_depth_limit : Int -> Int
proven_idris_graph_bfs_depth_limit vertexCount =
  -- BFS depth limit = diameter ≤ n-1
  if vertexCount <= 0 then 0 else vertexCount - 1

export
proven_idris_graph_dfs_depth_limit : Int -> Int
proven_idris_graph_dfs_depth_limit vertexCount =
  -- DFS depth limit for cycle detection
  vertexCount

export
proven_idris_graph_path_exists : Int -> Int
proven_idris_graph_path_exists hasPath = hasPath

export
proven_idris_graph_path_length : Int -> Int
proven_idris_graph_path_length numEdges = numEdges

--------------------------------------------------------------------------------
-- Cycle Detection
--------------------------------------------------------------------------------

export
proven_idris_graph_has_cycle : Int -> Int
proven_idris_graph_has_cycle hasCycle = hasCycle

export
proven_idris_graph_is_dag : Int -> Int
proven_idris_graph_is_dag hasCycle =
  encodeBool (hasCycle == 0)

export
proven_idris_graph_can_topological_sort : Int -> Int
proven_idris_graph_can_topological_sort hasCycle =
  -- Can only topologically sort DAGs (acyclic)
  encodeBool (hasCycle == 0)

export
proven_idris_graph_back_edge_detected : Int -> Int
proven_idris_graph_back_edge_detected stackContains = stackContains

--------------------------------------------------------------------------------
-- Connectivity
--------------------------------------------------------------------------------

export
proven_idris_graph_is_connected : Int -> Int -> Int
proven_idris_graph_is_connected vertexCount reachableCount =
  encodeBool (vertexCount == reachableCount)

export
proven_idris_graph_connected_components : Int -> Int
proven_idris_graph_connected_components count = count

export
proven_idris_graph_is_strongly_connected : Int -> Int
proven_idris_graph_is_strongly_connected isConnected = isConnected

export
proven_idris_graph_is_weakly_connected : Int -> Int
proven_idris_graph_is_weakly_connected isConnected = isConnected

--------------------------------------------------------------------------------
-- Special Graphs
--------------------------------------------------------------------------------

export
proven_idris_graph_is_tree : Int -> Int -> Int -> Int
proven_idris_graph_is_tree vertexCount edgeCount hasCycle =
  -- Tree: n vertices, n-1 edges, acyclic, connected
  encodeBool (vertexCount > 0 && edgeCount == vertexCount - 1 && hasCycle == 0)

export
proven_idris_graph_is_forest : Int -> Int
proven_idris_graph_is_forest hasCycle =
  -- Forest: acyclic (possibly disconnected)
  encodeBool (hasCycle == 0)

export
proven_idris_graph_is_bipartite : Int -> Int
proven_idris_graph_is_bipartite isBipartite = isBipartite

export
proven_idris_graph_is_planar : Int -> Int -> Int
proven_idris_graph_is_planar vertexCount edgeCount =
  -- Planar: e ≤ 3v - 6 (for v ≥ 3)
  if vertexCount < 3 then 1
  else encodeBool (edgeCount <= 3 * vertexCount - 6)

--------------------------------------------------------------------------------
-- Edge Weight Helpers
--------------------------------------------------------------------------------

export
proven_idris_graph_has_negative_weight : Int -> Int
proven_idris_graph_has_negative_weight weight =
  encodeBool (weight < 0)

export
proven_idris_graph_total_weight : Int -> Int
proven_idris_graph_total_weight sum = sum

export
proven_idris_graph_average_weight : Int -> Int -> Double
proven_idris_graph_average_weight totalWeight edgeCount =
  if edgeCount == 0 then 0.0
  else cast totalWeight / cast edgeCount

export
proven_idris_graph_min_weight : Int -> Int
proven_idris_graph_min_weight weight = weight

export
proven_idris_graph_max_weight : Int -> Int
proven_idris_graph_max_weight weight = weight

--------------------------------------------------------------------------------
-- Graph Traversal Limits
--------------------------------------------------------------------------------

export
proven_idris_graph_max_visits : Int -> Int
proven_idris_graph_max_visits vertexCount = vertexCount

export
proven_idris_graph_max_queue_size : Int -> Int
proven_idris_graph_max_queue_size vertexCount = vertexCount

export
proven_idris_graph_max_stack_depth : Int -> Int
proven_idris_graph_max_stack_depth vertexCount = vertexCount

export
proven_idris_graph_visit_limit_exceeded : Int -> Int -> Int
proven_idris_graph_visit_limit_exceeded visitCount vertexCount =
  encodeBool (visitCount > vertexCount)

--------------------------------------------------------------------------------
-- Graph Statistics
--------------------------------------------------------------------------------

export
proven_idris_graph_clustering_coefficient : Int -> Int -> Int -> Double
proven_idris_graph_clustering_coefficient triangles connectedTriples =
  if connectedTriples == 0 then 0.0
  else cast triangles / cast connectedTriples

export
proven_idris_graph_diameter : Int -> Int
proven_idris_graph_diameter maxDistance = maxDistance

export
proven_idris_graph_radius : Int -> Int
proven_idris_graph_radius minEccentricity = minEccentricity

export
proven_idris_graph_girth : Int -> Int
proven_idris_graph_girth shortestCycleLength = shortestCycleLength

--------------------------------------------------------------------------------
-- Memory Estimation
--------------------------------------------------------------------------------

export
proven_idris_graph_vertex_memory : Int -> Int -> Int
proven_idris_graph_vertex_memory vertexCount bytesPerVertex =
  vertexCount * bytesPerVertex

export
proven_idris_graph_edge_memory : Int -> Int -> Int
proven_idris_graph_edge_memory edgeCount bytesPerEdge =
  edgeCount * bytesPerEdge

export
proven_idris_graph_total_memory : Int -> Int -> Int -> Int -> Int
proven_idris_graph_total_memory vertexCount edgeCount bytesPerVertex bytesPerEdge =
  proven_idris_graph_vertex_memory vertexCount bytesPerVertex +
  proven_idris_graph_edge_memory edgeCount bytesPerEdge

export
proven_idris_graph_recommend_adjacency_list : Int -> Int -> Int
proven_idris_graph_recommend_adjacency_list vertexCount edgeCount =
  -- Adjacency list better for sparse graphs
  let density = if vertexCount <= 1 then 0.0
                 else cast edgeCount / cast (vertexCount * vertexCount)
  in encodeBool (density < 0.5)

export
proven_idris_graph_recommend_adjacency_matrix : Int -> Int -> Int
proven_idris_graph_recommend_adjacency_matrix vertexCount edgeCount =
  -- Adjacency matrix better for dense graphs
  let density = if vertexCount <= 1 then 0.0
                 else cast edgeCount / cast (vertexCount * vertexCount)
  in encodeBool (density >= 0.5)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

export
proven_idris_graph_min_vertices : Int
proven_idris_graph_min_vertices = 0

export
proven_idris_graph_max_reasonable_vertices : Int
proven_idris_graph_max_reasonable_vertices = 1000000

export
proven_idris_graph_max_reasonable_edges : Int
proven_idris_graph_max_reasonable_edges = 10000000

export
proven_idris_graph_sparse_threshold : Double
proven_idris_graph_sparse_threshold = 0.5

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_graph_friendly_error : String -> String
proven_idris_graph_friendly_error errorMsg =
  if isInfixOf "cycle" (toLower errorMsg)
    then "Graph cycle detected (cannot topologically sort)"
  else if isInfixOf "vertex" (toLower errorMsg) && isInfixOf "not found" (toLower errorMsg)
    then "Vertex not found in graph"
  else if isInfixOf "edge" (toLower errorMsg) && isInfixOf "not found" (toLower errorMsg)
    then "Edge not found in graph"
  else if isInfixOf "path" (toLower errorMsg)
    then "No path exists between vertices"
  else if isInfixOf "disconnected" (toLower errorMsg)
    then "Graph is disconnected (not all vertices reachable)"
  else if isInfixOf "limit" (toLower errorMsg) || isInfixOf "overflow" (toLower errorMsg)
    then "Graph size limit exceeded"
  else
    "Graph operation error"

export
proven_idris_graph_type_description : Double -> String
proven_idris_graph_type_description density =
  if density == 0.0 then "Empty graph (no edges)"
  else if density < 0.1 then "Very sparse graph"
  else if density < 0.5 then "Sparse graph"
  else if density < 0.9 then "Dense graph"
  else if density < 1.0 then "Very dense graph"
  else "Complete graph (fully connected)"
