// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

// Edge represents a directed edge in a graph.
type Edge[T comparable] struct {
	From   T
	To     T
	Weight float64
}

// DirectedGraph is a directed graph with cycle detection.
type DirectedGraph[T comparable] struct {
	adjacency map[T][]Edge[T]
	nodes     map[T]bool
}

// NewDirectedGraph creates an empty directed graph.
func NewDirectedGraph[T comparable]() *DirectedGraph[T] {
	return &DirectedGraph[T]{
		adjacency: make(map[T][]Edge[T]),
		nodes:     make(map[T]bool),
	}
}

// AddNode adds a node to the graph.
func (g *DirectedGraph[T]) AddNode(node T) {
	if !g.nodes[node] {
		g.nodes[node] = true
		if g.adjacency[node] == nil {
			g.adjacency[node] = make([]Edge[T], 0)
		}
	}
}

// AddEdge adds a directed edge.
func (g *DirectedGraph[T]) AddEdge(from, to T) bool {
	return g.AddWeightedEdge(from, to, 1.0)
}

// AddWeightedEdge adds a directed edge with weight.
func (g *DirectedGraph[T]) AddWeightedEdge(from, to T, weight float64) bool {
	g.AddNode(from)
	g.AddNode(to)

	// Check for duplicate edge
	for _, e := range g.adjacency[from] {
		if e.To == to {
			return false
		}
	}

	g.adjacency[from] = append(g.adjacency[from], Edge[T]{
		From:   from,
		To:     to,
		Weight: weight,
	})
	return true
}

// RemoveEdge removes an edge.
func (g *DirectedGraph[T]) RemoveEdge(from, to T) bool {
	edges := g.adjacency[from]
	for i, e := range edges {
		if e.To == to {
			g.adjacency[from] = append(edges[:i], edges[i+1:]...)
			return true
		}
	}
	return false
}

// RemoveNode removes a node and all its edges.
func (g *DirectedGraph[T]) RemoveNode(node T) bool {
	if !g.nodes[node] {
		return false
	}

	delete(g.nodes, node)
	delete(g.adjacency, node)

	// Remove edges pointing to this node
	for from := range g.adjacency {
		edges := g.adjacency[from]
		newEdges := make([]Edge[T], 0, len(edges))
		for _, e := range edges {
			if e.To != node {
				newEdges = append(newEdges, e)
			}
		}
		g.adjacency[from] = newEdges
	}

	return true
}

// HasNode checks if a node exists.
func (g *DirectedGraph[T]) HasNode(node T) bool {
	return g.nodes[node]
}

// HasEdge checks if an edge exists.
func (g *DirectedGraph[T]) HasEdge(from, to T) bool {
	for _, e := range g.adjacency[from] {
		if e.To == to {
			return true
		}
	}
	return false
}

// Neighbors returns the neighbors of a node.
func (g *DirectedGraph[T]) Neighbors(node T) []T {
	neighbors := make([]T, 0)
	for _, e := range g.adjacency[node] {
		neighbors = append(neighbors, e.To)
	}
	return neighbors
}

// OutEdges returns outgoing edges from a node.
func (g *DirectedGraph[T]) OutEdges(node T) []Edge[T] {
	edges := g.adjacency[node]
	result := make([]Edge[T], len(edges))
	copy(result, edges)
	return result
}

// InEdges returns incoming edges to a node.
func (g *DirectedGraph[T]) InEdges(node T) []Edge[T] {
	result := make([]Edge[T], 0)
	for from, edges := range g.adjacency {
		for _, e := range edges {
			if e.To == node {
				result = append(result, Edge[T]{From: from, To: node, Weight: e.Weight})
			}
		}
	}
	return result
}

// Nodes returns all nodes.
func (g *DirectedGraph[T]) Nodes() []T {
	result := make([]T, 0, len(g.nodes))
	for node := range g.nodes {
		result = append(result, node)
	}
	return result
}

// Edges returns all edges.
func (g *DirectedGraph[T]) Edges() []Edge[T] {
	result := make([]Edge[T], 0)
	for _, edges := range g.adjacency {
		result = append(result, edges...)
	}
	return result
}

// NodeCount returns the number of nodes.
func (g *DirectedGraph[T]) NodeCount() int {
	return len(g.nodes)
}

// EdgeCount returns the number of edges.
func (g *DirectedGraph[T]) EdgeCount() int {
	count := 0
	for _, edges := range g.adjacency {
		count += len(edges)
	}
	return count
}

// HasCycle checks if the graph contains a cycle.
func (g *DirectedGraph[T]) HasCycle() bool {
	visited := make(map[T]bool)
	recStack := make(map[T]bool)

	for node := range g.nodes {
		if g.hasCycleFrom(node, visited, recStack) {
			return true
		}
	}
	return false
}

func (g *DirectedGraph[T]) hasCycleFrom(node T, visited, recStack map[T]bool) bool {
	if recStack[node] {
		return true
	}
	if visited[node] {
		return false
	}

	visited[node] = true
	recStack[node] = true

	for _, e := range g.adjacency[node] {
		if g.hasCycleFrom(e.To, visited, recStack) {
			return true
		}
	}

	recStack[node] = false
	return false
}

// TopologicalSort returns nodes in topological order.
// Returns nil if the graph has a cycle.
func (g *DirectedGraph[T]) TopologicalSort() []T {
	if g.HasCycle() {
		return nil
	}

	visited := make(map[T]bool)
	result := make([]T, 0, len(g.nodes))

	var visit func(T)
	visit = func(node T) {
		if visited[node] {
			return
		}
		visited[node] = true

		for _, e := range g.adjacency[node] {
			visit(e.To)
		}

		result = append(result, node)
	}

	for node := range g.nodes {
		visit(node)
	}

	// Reverse
	for i, j := 0, len(result)-1; i < j; i, j = i+1, j-1 {
		result[i], result[j] = result[j], result[i]
	}

	return result
}

// BFS performs breadth-first search from a start node.
func (g *DirectedGraph[T]) BFS(start T) []T {
	if !g.nodes[start] {
		return nil
	}

	visited := make(map[T]bool)
	result := make([]T, 0)
	queue := []T{start}

	for len(queue) > 0 {
		node := queue[0]
		queue = queue[1:]

		if visited[node] {
			continue
		}
		visited[node] = true
		result = append(result, node)

		for _, e := range g.adjacency[node] {
			if !visited[e.To] {
				queue = append(queue, e.To)
			}
		}
	}

	return result
}

// DFS performs depth-first search from a start node.
func (g *DirectedGraph[T]) DFS(start T) []T {
	if !g.nodes[start] {
		return nil
	}

	visited := make(map[T]bool)
	result := make([]T, 0)

	var dfs func(T)
	dfs = func(node T) {
		if visited[node] {
			return
		}
		visited[node] = true
		result = append(result, node)

		for _, e := range g.adjacency[node] {
			dfs(e.To)
		}
	}

	dfs(start)
	return result
}

// ShortestPath finds the shortest path using BFS (unweighted).
func (g *DirectedGraph[T]) ShortestPath(start, end T) []T {
	if !g.nodes[start] || !g.nodes[end] {
		return nil
	}

	if start == end {
		return []T{start}
	}

	visited := make(map[T]bool)
	parent := make(map[T]T)
	queue := []T{start}
	visited[start] = true

	for len(queue) > 0 {
		node := queue[0]
		queue = queue[1:]

		for _, e := range g.adjacency[node] {
			if !visited[e.To] {
				visited[e.To] = true
				parent[e.To] = node
				queue = append(queue, e.To)

				if e.To == end {
					// Reconstruct path
					path := []T{end}
					current := end
					for current != start {
						current = parent[current]
						path = append([]T{current}, path...)
					}
					return path
				}
			}
		}
	}

	return nil // No path found
}

// Reachable returns all nodes reachable from a start node.
func (g *DirectedGraph[T]) Reachable(start T) []T {
	return g.BFS(start)
}

// InDegree returns the number of incoming edges to a node.
func (g *DirectedGraph[T]) InDegree(node T) int {
	count := 0
	for _, edges := range g.adjacency {
		for _, e := range edges {
			if e.To == node {
				count++
			}
		}
	}
	return count
}

// OutDegree returns the number of outgoing edges from a node.
func (g *DirectedGraph[T]) OutDegree(node T) int {
	return len(g.adjacency[node])
}

// Clear removes all nodes and edges.
func (g *DirectedGraph[T]) Clear() {
	g.adjacency = make(map[T][]Edge[T])
	g.nodes = make(map[T]bool)
}

// Transpose returns a new graph with all edges reversed.
func (g *DirectedGraph[T]) Transpose() *DirectedGraph[T] {
	transposed := NewDirectedGraph[T]()

	for node := range g.nodes {
		transposed.AddNode(node)
	}

	for _, edges := range g.adjacency {
		for _, e := range edges {
			transposed.AddWeightedEdge(e.To, e.From, e.Weight)
		}
	}

	return transposed
}
