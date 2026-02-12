// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Edge in a graph.
public struct Edge<T: Hashable>: Equatable where T: Equatable {
    public let from: T
    public let to: T
    public let weight: Double?

    public init(from: T, to: T, weight: Double? = nil) {
        self.from = from
        self.to = to
        self.weight = weight
    }
}

/// Directed graph.
public class DirectedGraph<T: Hashable> {
    private var adjacency: [T: Set<T>] = [:]
    private var weights: [String: Double] = [:]
    private let lock = NSLock()

    public init() {}

    /// Add a node.
    public func addNode(_ node: T) {
        lock.lock()
        defer { lock.unlock() }

        if adjacency[node] == nil {
            adjacency[node] = Set()
        }
    }

    /// Add an edge.
    public func addEdge(from: T, to: T, weight: Double? = nil) {
        lock.lock()
        defer { lock.unlock() }

        if adjacency[from] == nil {
            adjacency[from] = Set()
        }
        if adjacency[to] == nil {
            adjacency[to] = Set()
        }

        adjacency[from]!.insert(to)

        if let w = weight {
            weights["\(from)->\(to)"] = w
        }
    }

    /// Remove an edge.
    @discardableResult
    public func removeEdge(from: T, to: T) -> Bool {
        lock.lock()
        defer { lock.unlock() }

        guard var neighbors = adjacency[from] else { return false }
        let removed = neighbors.remove(to) != nil
        adjacency[from] = neighbors

        if removed {
            weights.removeValue(forKey: "\(from)->\(to)")
        }
        return removed
    }

    /// Remove a node and all its edges.
    @discardableResult
    public func removeNode(_ node: T) -> Bool {
        lock.lock()
        defer { lock.unlock() }

        guard adjacency[node] != nil else { return false }

        // Remove outgoing edges
        if let neighbors = adjacency[node] {
            for neighbor in neighbors {
                weights.removeValue(forKey: "\(node)->\(neighbor)")
            }
        }
        adjacency.removeValue(forKey: node)

        // Remove incoming edges
        for (key, var neighbors) in adjacency {
            if neighbors.remove(node) != nil {
                adjacency[key] = neighbors
                weights.removeValue(forKey: "\(key)->\(node)")
            }
        }

        return true
    }

    /// Check if node exists.
    public func hasNode(_ node: T) -> Bool {
        lock.lock()
        defer { lock.unlock() }
        return adjacency[node] != nil
    }

    /// Check if edge exists.
    public func hasEdge(from: T, to: T) -> Bool {
        lock.lock()
        defer { lock.unlock() }
        return adjacency[from]?.contains(to) ?? false
    }

    /// Get edge weight.
    public func getWeight(from: T, to: T) -> Double? {
        lock.lock()
        defer { lock.unlock() }
        return weights["\(from)->\(to)"]
    }

    /// Get neighbors of a node.
    public func neighbors(_ node: T) -> [T] {
        lock.lock()
        defer { lock.unlock() }
        return Array(adjacency[node] ?? Set())
    }

    /// Get all nodes.
    public func nodes() -> [T] {
        lock.lock()
        defer { lock.unlock() }
        return Array(adjacency.keys)
    }

    /// Get all edges.
    public func edges() -> [Edge<T>] {
        lock.lock()
        defer { lock.unlock() }

        var result: [Edge<T>] = []
        for (from, neighbors) in adjacency {
            for to in neighbors {
                let weight = weights["\(from)->\(to)"]
                result.append(Edge(from: from, to: to, weight: weight))
            }
        }
        return result
    }

    /// Get node count.
    public var nodeCount: Int {
        lock.lock()
        defer { lock.unlock() }
        return adjacency.count
    }

    /// Get edge count.
    public var edgeCount: Int {
        lock.lock()
        defer { lock.unlock() }
        return adjacency.values.reduce(0) { $0 + $1.count }
    }

    /// Breadth-first search from a starting node.
    public func bfs(from start: T) -> [T] {
        lock.lock()
        defer { lock.unlock() }

        guard adjacency[start] != nil else { return [] }

        var visited: Set<T> = []
        var result: [T] = []
        var queue: [T] = [start]

        while !queue.isEmpty {
            let current = queue.removeFirst()
            if visited.contains(current) { continue }

            visited.insert(current)
            result.append(current)

            for neighbor in adjacency[current] ?? Set() {
                if !visited.contains(neighbor) {
                    queue.append(neighbor)
                }
            }
        }

        return result
    }

    /// Depth-first search from a starting node.
    public func dfs(from start: T) -> [T] {
        lock.lock()
        defer { lock.unlock() }

        guard adjacency[start] != nil else { return [] }

        var visited: Set<T> = []
        var result: [T] = []

        func visit(_ node: T) {
            if visited.contains(node) { return }
            visited.insert(node)
            result.append(node)

            for neighbor in adjacency[node] ?? Set() {
                visit(neighbor)
            }
        }

        visit(start)
        return result
    }

    /// Check if there's a path from source to target.
    public func hasPath(from source: T, to target: T) -> Bool {
        lock.lock()
        defer { lock.unlock() }

        guard adjacency[source] != nil && adjacency[target] != nil else { return false }

        var visited: Set<T> = []
        var queue: [T] = [source]

        while !queue.isEmpty {
            let current = queue.removeFirst()
            if current == target { return true }
            if visited.contains(current) { continue }

            visited.insert(current)

            for neighbor in adjacency[current] ?? Set() {
                if !visited.contains(neighbor) {
                    queue.append(neighbor)
                }
            }
        }

        return false
    }

    /// Find shortest path using BFS (unweighted).
    public func shortestPath(from source: T, to target: T) -> [T]? {
        lock.lock()
        defer { lock.unlock() }

        guard adjacency[source] != nil && adjacency[target] != nil else { return nil }

        var visited: Set<T> = []
        var parent: [T: T] = [:]
        var queue: [T] = [source]
        visited.insert(source)

        while !queue.isEmpty {
            let current = queue.removeFirst()

            if current == target {
                // Reconstruct path
                var path: [T] = []
                var node: T? = target
                while let n = node {
                    path.insert(n, at: 0)
                    node = parent[n]
                }
                return path
            }

            for neighbor in adjacency[current] ?? Set() {
                if !visited.contains(neighbor) {
                    visited.insert(neighbor)
                    parent[neighbor] = current
                    queue.append(neighbor)
                }
            }
        }

        return nil
    }

    /// Detect if the graph has a cycle.
    public func hasCycle() -> Bool {
        lock.lock()
        defer { lock.unlock() }

        var white = Set(adjacency.keys) // unvisited
        var gray: Set<T> = [] // in progress
        var black: Set<T> = [] // completed

        func hasCycleFrom(_ node: T) -> Bool {
            white.remove(node)
            gray.insert(node)

            for neighbor in adjacency[node] ?? Set() {
                if gray.contains(neighbor) { return true } // back edge
                if white.contains(neighbor) && hasCycleFrom(neighbor) { return true }
            }

            gray.remove(node)
            black.insert(node)
            return false
        }

        for node in Array(white) {
            if hasCycleFrom(node) { return true }
        }

        return false
    }

    /// Topological sort (only valid for DAGs).
    public func topologicalSort() -> [T]? {
        if hasCycle() { return nil }

        lock.lock()
        defer { lock.unlock() }

        var visited: Set<T> = []
        var result: [T] = []

        func visit(_ node: T) {
            if visited.contains(node) { return }
            visited.insert(node)

            for neighbor in adjacency[node] ?? Set() {
                visit(neighbor)
            }

            result.insert(node, at: 0)
        }

        for node in adjacency.keys {
            visit(node)
        }

        return result
    }

    /// Get in-degree of a node.
    public func inDegree(_ node: T) -> Int {
        lock.lock()
        defer { lock.unlock() }

        var count = 0
        for neighbors in adjacency.values {
            if neighbors.contains(node) {
                count += 1
            }
        }
        return count
    }

    /// Get out-degree of a node.
    public func outDegree(_ node: T) -> Int {
        lock.lock()
        defer { lock.unlock() }
        return adjacency[node]?.count ?? 0
    }

    /// Clear the graph.
    public func clear() {
        lock.lock()
        defer { lock.unlock() }
        adjacency.removeAll()
        weights.removeAll()
    }
}

/// Undirected graph.
public class UndirectedGraph<T: Hashable> {
    private let directed: DirectedGraph<T>

    public init() {
        self.directed = DirectedGraph<T>()
    }

    /// Add a node.
    public func addNode(_ node: T) {
        directed.addNode(node)
    }

    /// Add an edge (bidirectional).
    public func addEdge(a: T, b: T, weight: Double? = nil) {
        directed.addEdge(from: a, to: b, weight: weight)
        directed.addEdge(from: b, to: a, weight: weight)
    }

    /// Remove an edge (bidirectional).
    @discardableResult
    public func removeEdge(a: T, b: T) -> Bool {
        let r1 = directed.removeEdge(from: a, to: b)
        let r2 = directed.removeEdge(from: b, to: a)
        return r1 || r2
    }

    /// Check if edge exists.
    public func hasEdge(a: T, b: T) -> Bool {
        directed.hasEdge(from: a, to: b)
    }

    /// Get neighbors.
    public func neighbors(_ node: T) -> [T] {
        directed.neighbors(node)
    }

    /// Get all nodes.
    public func nodes() -> [T] {
        directed.nodes()
    }

    /// BFS from start node.
    public func bfs(from start: T) -> [T] {
        directed.bfs(from: start)
    }

    /// DFS from start node.
    public func dfs(from start: T) -> [T] {
        directed.dfs(from: start)
    }

    /// Check if connected.
    public func isConnected() -> Bool {
        let allNodes = nodes()
        if allNodes.isEmpty { return true }

        let visited = bfs(from: allNodes[0])
        return visited.count == allNodes.count
    }
}
