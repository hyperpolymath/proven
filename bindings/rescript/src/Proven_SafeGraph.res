// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeGraph - Graph operations that cannot crash.
 *
 * Provides safe directed graph implementation with bounded vertex and edge
 * counts. All operations return Result types to handle edge cases like
 * invalid vertices or capacity limits safely.
 *
 * Key properties:
 * - Fixed maximum capacity for vertices and edges
 * - Supports weighted directed edges
 * - Provides cycle detection
 * - Supports basic graph algorithms (BFS, DFS, topological sort)
 */

// ============================================================================
// Error types
// ============================================================================

/** Error types for graph operations */
type graphError =
  | VertexNotFound
  | EdgeExists
  | MaxVerticesReached
  | MaxEdgesReached
  | CycleDetected
  | InvalidCapacity

/** Convert error to human-readable string */
let errorToString = (error: graphError): string => {
  switch error {
  | VertexNotFound => "Vertex not found"
  | EdgeExists => "Edge already exists"
  | MaxVerticesReached => "Maximum vertices reached"
  | MaxEdgesReached => "Maximum edges reached"
  | CycleDetected => "Cycle detected in graph"
  | InvalidCapacity => "Invalid capacity specified"
  }
}

// ============================================================================
// Graph Types
// ============================================================================

/** A directed edge with weight */
type edge = {
  source: int,
  target: int,
  weight: float,
}

/** A directed graph with bounded capacity */
type directedGraph<'v> = {
  mutable vertices: array<option<'v>>,
  mutable edges: array<option<edge>>,
  mutable vertexCount: int,
  mutable edgeCount: int,
  maxVertices: int,
  maxEdges: int,
}

// ============================================================================
// Graph Creation
// ============================================================================

/** Create a new directed graph with given capacity limits */
let make = (maxVertices: int, maxEdges: int): result<directedGraph<'v>, graphError> => {
  if maxVertices <= 0 || maxEdges <= 0 {
    Error(InvalidCapacity)
  } else {
    Ok({
      vertices: Belt.Array.make(maxVertices, None),
      edges: Belt.Array.make(maxEdges, None),
      vertexCount: 0,
      edgeCount: 0,
      maxVertices,
      maxEdges,
    })
  }
}

/** Get the number of vertices in the graph */
let vertexCount = (graph: directedGraph<'v>): int => {
  graph.vertexCount
}

/** Get the number of edges in the graph */
let edgeCount = (graph: directedGraph<'v>): int => {
  graph.edgeCount
}

/** Check if the graph has no vertices */
let isEmpty = (graph: directedGraph<'v>): bool => {
  graph.vertexCount == 0
}

// ============================================================================
// Vertex Operations
// ============================================================================

/** Add a vertex to the graph, returns the vertex index */
let addVertex = (graph: directedGraph<'v>, value: 'v): result<int, graphError> => {
  if graph.vertexCount >= graph.maxVertices {
    Error(MaxVerticesReached)
  } else {
    let index = graph.vertexCount
    Belt.Array.setUnsafe(graph.vertices, index, Some(value))
    graph.vertexCount = graph.vertexCount + 1
    Ok(index)
  }
}

/** Get the value of a vertex by index */
let getVertex = (graph: directedGraph<'v>, index: int): option<'v> => {
  if index < 0 || index >= graph.vertexCount {
    None
  } else {
    Belt.Array.getUnsafe(graph.vertices, index)
  }
}

/** Find a vertex by value, returns the index */
let findVertex = (graph: directedGraph<'v>, value: 'v): option<int> => {
  let found = ref(None)
  let i = ref(0)
  while found.contents->Belt.Option.isNone && i.contents < graph.vertexCount {
    switch Belt.Array.getUnsafe(graph.vertices, i.contents) {
    | Some(v) =>
      if v == value {
        found := Some(i.contents)
      }
    | None => ()
    }
    i := i.contents + 1
  }
  found.contents
}

/** Check if a vertex index is valid */
let hasVertex = (graph: directedGraph<'v>, index: int): bool => {
  index >= 0 && index < graph.vertexCount
}

// ============================================================================
// Edge Operations
// ============================================================================

/** Add a directed edge from source to target with weight */
let addEdge = (
  graph: directedGraph<'v>,
  source: int,
  target: int,
  weight: float,
): result<unit, graphError> => {
  if source < 0 || source >= graph.vertexCount || target < 0 || target >= graph.vertexCount {
    Error(VertexNotFound)
  } else if graph.edgeCount >= graph.maxEdges {
    Error(MaxEdgesReached)
  } else {
    Belt.Array.setUnsafe(
      graph.edges,
      graph.edgeCount,
      Some({
        source,
        target,
        weight,
      }),
    )
    graph.edgeCount = graph.edgeCount + 1
    Ok()
  }
}

/** Add a directed edge with default weight of 1.0 */
let addEdgeUnweighted = (
  graph: directedGraph<'v>,
  source: int,
  target: int,
): result<unit, graphError> => {
  addEdge(graph, source, target, 1.0)
}

/** Check if an edge exists between two vertices */
let hasEdge = (graph: directedGraph<'v>, source: int, target: int): bool => {
  let found = ref(false)
  let i = ref(0)
  while !found.contents && i.contents < graph.edgeCount {
    switch Belt.Array.getUnsafe(graph.edges, i.contents) {
    | Some(edge) =>
      if edge.source == source && edge.target == target {
        found := true
      }
    | None => ()
    }
    i := i.contents + 1
  }
  found.contents
}

/** Get the weight of an edge between two vertices */
let getEdgeWeight = (graph: directedGraph<'v>, source: int, target: int): option<float> => {
  let found = ref(None)
  let i = ref(0)
  while found.contents->Belt.Option.isNone && i.contents < graph.edgeCount {
    switch Belt.Array.getUnsafe(graph.edges, i.contents) {
    | Some(edge) =>
      if edge.source == source && edge.target == target {
        found := Some(edge.weight)
      }
    | None => ()
    }
    i := i.contents + 1
  }
  found.contents
}

/** Remove an edge between two vertices */
let removeEdge = (graph: directedGraph<'v>, source: int, target: int): bool => {
  let removed = ref(false)
  let i = ref(0)
  while !removed.contents && i.contents < graph.edgeCount {
    switch Belt.Array.getUnsafe(graph.edges, i.contents) {
    | Some(edge) =>
      if edge.source == source && edge.target == target {
        // Shift remaining edges down
        for j in i.contents to graph.edgeCount - 2 {
          Belt.Array.setUnsafe(graph.edges, j, Belt.Array.getUnsafe(graph.edges, j + 1))
        }
        Belt.Array.setUnsafe(graph.edges, graph.edgeCount - 1, None)
        graph.edgeCount = graph.edgeCount - 1
        removed := true
      }
    | None => ()
    }
    i := i.contents + 1
  }
  removed.contents
}

// ============================================================================
// Neighbor and Degree Operations
// ============================================================================

/** Get all outgoing neighbors (successors) of a vertex */
let getNeighbors = (graph: directedGraph<'v>, vertex: int): array<int> => {
  let result = []
  for i in 0 to graph.edgeCount - 1 {
    switch Belt.Array.getUnsafe(graph.edges, i) {
    | Some(edge) =>
      if edge.source == vertex {
        let _ = Js.Array2.push(result, edge.target)
      }
    | None => ()
    }
  }
  result
}

/** Get all incoming neighbors (predecessors) of a vertex */
let getPredecessors = (graph: directedGraph<'v>, vertex: int): array<int> => {
  let result = []
  for i in 0 to graph.edgeCount - 1 {
    switch Belt.Array.getUnsafe(graph.edges, i) {
    | Some(edge) =>
      if edge.target == vertex {
        let _ = Js.Array2.push(result, edge.source)
      }
    | None => ()
    }
  }
  result
}

/** Get the out-degree of a vertex (number of outgoing edges) */
let outDegree = (graph: directedGraph<'v>, vertex: int): int => {
  let count = ref(0)
  for i in 0 to graph.edgeCount - 1 {
    switch Belt.Array.getUnsafe(graph.edges, i) {
    | Some(edge) =>
      if edge.source == vertex {
        count := count.contents + 1
      }
    | None => ()
    }
  }
  count.contents
}

/** Get the in-degree of a vertex (number of incoming edges) */
let inDegree = (graph: directedGraph<'v>, vertex: int): int => {
  let count = ref(0)
  for i in 0 to graph.edgeCount - 1 {
    switch Belt.Array.getUnsafe(graph.edges, i) {
    | Some(edge) =>
      if edge.target == vertex {
        count := count.contents + 1
      }
    | None => ()
    }
  }
  count.contents
}

/** Get the total degree of a vertex (in + out) */
let degree = (graph: directedGraph<'v>, vertex: int): int => {
  inDegree(graph, vertex) + outDegree(graph, vertex)
}

// ============================================================================
// Cycle Detection
// ============================================================================

/** Internal: DFS helper for cycle detection */
let rec hasCycleFromVertex = (
  graph: directedGraph<'v>,
  vertex: int,
  visited: array<bool>,
  stack: array<bool>,
): bool => {
  if Belt.Array.getUnsafe(stack, vertex) {
    true // Back edge found, cycle exists
  } else if Belt.Array.getUnsafe(visited, vertex) {
    false // Already fully explored
  } else {
    Belt.Array.setUnsafe(visited, vertex, true)
    Belt.Array.setUnsafe(stack, vertex, true)

    let neighbors = getNeighbors(graph, vertex)
    let hasCycle = ref(false)
    let i = ref(0)

    while !hasCycle.contents && i.contents < Belt.Array.length(neighbors) {
      let neighbor = Belt.Array.getUnsafe(neighbors, i.contents)
      if hasCycleFromVertex(graph, neighbor, visited, stack) {
        hasCycle := true
      }
      i := i.contents + 1
    }

    Belt.Array.setUnsafe(stack, vertex, false)
    hasCycle.contents
  }
}

/** Check if the graph contains any cycles */
let hasCycle = (graph: directedGraph<'v>): bool => {
  let visited = Belt.Array.make(graph.maxVertices, false)
  let stack = Belt.Array.make(graph.maxVertices, false)

  let hasCycleResult = ref(false)
  let i = ref(0)

  while !hasCycleResult.contents && i.contents < graph.vertexCount {
    if !Belt.Array.getUnsafe(visited, i.contents) {
      if hasCycleFromVertex(graph, i.contents, visited, stack) {
        hasCycleResult := true
      }
    }
    i := i.contents + 1
  }

  hasCycleResult.contents
}

// ============================================================================
// Graph Traversal
// ============================================================================

/** Depth-first search from a starting vertex, returns visited vertices in order */
let dfs = (graph: directedGraph<'v>, start: int): result<array<int>, graphError> => {
  if start < 0 || start >= graph.vertexCount {
    Error(VertexNotFound)
  } else {
    let visited = Belt.Array.make(graph.maxVertices, false)
    let result = []

    let rec dfsHelper = (vertex: int): unit => {
      if !Belt.Array.getUnsafe(visited, vertex) {
        Belt.Array.setUnsafe(visited, vertex, true)
        let _ = Js.Array2.push(result, vertex)

        let neighbors = getNeighbors(graph, vertex)
        Belt.Array.forEach(neighbors, neighbor => {
          dfsHelper(neighbor)
        })
      }
    }

    dfsHelper(start)
    Ok(result)
  }
}

/** Breadth-first search from a starting vertex, returns visited vertices in order */
let bfs = (graph: directedGraph<'v>, start: int): result<array<int>, graphError> => {
  if start < 0 || start >= graph.vertexCount {
    Error(VertexNotFound)
  } else {
    let visited = Belt.Array.make(graph.maxVertices, false)
    let result = []
    let queue = [start]

    Belt.Array.setUnsafe(visited, start, true)

    while Belt.Array.length(queue) > 0 {
      switch Js.Array2.shift(queue) {
      | None => ()
      | Some(current) =>
        let _ = Js.Array2.push(result, current)

        let neighbors = getNeighbors(graph, current)
        Belt.Array.forEach(neighbors, neighbor => {
          if !Belt.Array.getUnsafe(visited, neighbor) {
            Belt.Array.setUnsafe(visited, neighbor, true)
            let _ = Js.Array2.push(queue, neighbor)
          }
        })
      }
    }

    Ok(result)
  }
}

// ============================================================================
// Topological Sort
// ============================================================================

/** Perform topological sort on a DAG (directed acyclic graph)
 *
 * Returns Error(CycleDetected) if the graph contains a cycle.
 */
let topologicalSort = (graph: directedGraph<'v>): result<array<int>, graphError> => {
  if hasCycle(graph) {
    Error(CycleDetected)
  } else {
    let visited = Belt.Array.make(graph.maxVertices, false)
    let result = []

    let rec visit = (vertex: int): unit => {
      if !Belt.Array.getUnsafe(visited, vertex) {
        Belt.Array.setUnsafe(visited, vertex, true)

        let neighbors = getNeighbors(graph, vertex)
        Belt.Array.forEach(neighbors, neighbor => {
          visit(neighbor)
        })

        let _ = Js.Array2.unshift(result, vertex)
      }
    }

    for i in 0 to graph.vertexCount - 1 {
      if !Belt.Array.getUnsafe(visited, i) {
        visit(i)
      }
    }

    Ok(result)
  }
}

// ============================================================================
// Path Finding
// ============================================================================

/** Check if a path exists between two vertices */
let hasPath = (graph: directedGraph<'v>, source: int, target: int): result<bool, graphError> => {
  if source < 0 || source >= graph.vertexCount || target < 0 || target >= graph.vertexCount {
    Error(VertexNotFound)
  } else if source == target {
    Ok(true)
  } else {
    switch bfs(graph, source) {
    | Error(e) => Error(e)
    | Ok(visited) => Ok(Js.Array2.includes(visited, target))
    }
  }
}

/** Get all edges as an array */
let getAllEdges = (graph: directedGraph<'v>): array<edge> => {
  let result = []
  for i in 0 to graph.edgeCount - 1 {
    switch Belt.Array.getUnsafe(graph.edges, i) {
    | Some(edge) => {
        let _ = Js.Array2.push(result, edge)
      }
    | None => ()
    }
  }
  result
}

/** Get all vertex values as an array */
let getAllVertices = (graph: directedGraph<'v>): array<'v> => {
  let result = []
  for i in 0 to graph.vertexCount - 1 {
    switch Belt.Array.getUnsafe(graph.vertices, i) {
    | Some(v) => {
        let _ = Js.Array2.push(result, v)
      }
    | None => ()
    }
  }
  result
}

/** Clear all vertices and edges from the graph */
let clear = (graph: directedGraph<'v>): unit => {
  for i in 0 to graph.maxVertices - 1 {
    Belt.Array.setUnsafe(graph.vertices, i, None)
  }
  for i in 0 to graph.maxEdges - 1 {
    Belt.Array.setUnsafe(graph.edges, i, None)
  }
  graph.vertexCount = 0
  graph.edgeCount = 0
}

// ============================================================================
// FFI bindings to Zig (for WASM integration)
// ============================================================================

/** Status codes from Zig FFI */
type ffiStatus = {
  status: int,
  value: int,
}

/** Status with bool value from Zig FFI */
type ffiStatusBool = {
  status: int,
  value: bool,
}

/** FFI binding to Zig graph operations */
@module("proven") external ffiGraphAddVertex: (int, int) => ffiStatus = "proven_graph_add_vertex"
@module("proven") external ffiGraphFindVertex: (int, int) => ffiStatus = "proven_graph_find_vertex"
@module("proven") external ffiGraphAddEdge: (int, int, int, float) => int = "proven_graph_add_edge"
@module("proven") external ffiGraphHasEdge: (int, int, int) => bool = "proven_graph_has_edge"
@module("proven") external ffiGraphOutDegree: (int, int) => int = "proven_graph_out_degree"
@module("proven") external ffiGraphInDegree: (int, int) => int = "proven_graph_in_degree"
@module("proven") external ffiGraphHasCycle: int => bool = "proven_graph_has_cycle"
@module("proven") external ffiGraphVertexCount: int => int = "proven_graph_vertex_count"
@module("proven") external ffiGraphEdgeCount: int => int = "proven_graph_edge_count"
