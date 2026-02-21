// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

/**
 * Edge in a graph.
 */
data class Edge<T>(
    val from: T,
    val to: T,
    val weight: Double? = null
)

/**
 * Directed graph.
 */
class DirectedGraph<T> {
    private val lock = ReentrantLock()
    private val adjacency = mutableMapOf<T, MutableSet<T>>()
    private val weights = mutableMapOf<Pair<T, T>, Double>()

    /**
     * Add a node.
     */
    fun addNode(node: T) = lock.withLock {
        adjacency.getOrPut(node) { mutableSetOf() }
    }

    /**
     * Add an edge.
     */
    fun addEdge(from: T, to: T, weight: Double? = null) = lock.withLock {
        adjacency.getOrPut(from) { mutableSetOf() }.add(to)
        adjacency.getOrPut(to) { mutableSetOf() }
        if (weight != null) {
            weights[from to to] = weight
        }
    }

    /**
     * Remove an edge.
     */
    fun removeEdge(from: T, to: T): Boolean = lock.withLock {
        val removed = adjacency[from]?.remove(to) ?: false
        if (removed) weights.remove(from to to)
        removed
    }

    /**
     * Remove a node and all its edges.
     */
    fun removeNode(node: T): Boolean = lock.withLock {
        if (adjacency[node] == null) return false

        // Remove outgoing edges
        adjacency[node]?.forEach { neighbor ->
            weights.remove(node to neighbor)
        }
        adjacency.remove(node)

        // Remove incoming edges
        adjacency.forEach { (key, neighbors) ->
            if (neighbors.remove(node)) {
                weights.remove(key to node)
            }
        }
        true
    }

    /**
     * Check if node exists.
     */
    fun hasNode(node: T): Boolean = lock.withLock { adjacency.containsKey(node) }

    /**
     * Check if edge exists.
     */
    fun hasEdge(from: T, to: T): Boolean = lock.withLock {
        adjacency[from]?.contains(to) ?: false
    }

    /**
     * Get edge weight.
     */
    fun getWeight(from: T, to: T): Double? = lock.withLock { weights[from to to] }

    /**
     * Get neighbors of a node.
     */
    fun neighbors(node: T): List<T> = lock.withLock {
        adjacency[node]?.toList() ?: emptyList()
    }

    /**
     * Get all nodes.
     */
    fun nodes(): List<T> = lock.withLock { adjacency.keys.toList() }

    /**
     * Get all edges.
     */
    fun edges(): List<Edge<T>> = lock.withLock {
        adjacency.flatMap { (from, neighbors) ->
            neighbors.map { to -> Edge(from, to, weights[from to to]) }
        }
    }

    /**
     * Get node count.
     */
    val nodeCount: Int get() = lock.withLock { adjacency.size }

    /**
     * Get edge count.
     */
    val edgeCount: Int get() = lock.withLock { adjacency.values.sumOf { it.size } }

    /**
     * Breadth-first search.
     */
    fun bfs(start: T): List<T> = lock.withLock {
        if (adjacency[start] == null) return emptyList()

        val visited = mutableSetOf<T>()
        val result = mutableListOf<T>()
        val queue = ArrayDeque<T>()
        queue.add(start)

        while (queue.isNotEmpty()) {
            val current = queue.removeFirst()
            if (current in visited) continue

            visited.add(current)
            result.add(current)

            adjacency[current]?.forEach { neighbor ->
                if (neighbor !in visited) queue.add(neighbor)
            }
        }

        result
    }

    /**
     * Depth-first search.
     */
    fun dfs(start: T): List<T> = lock.withLock {
        if (adjacency[start] == null) return emptyList()

        val visited = mutableSetOf<T>()
        val result = mutableListOf<T>()

        fun visit(node: T) {
            if (node in visited) return
            visited.add(node)
            result.add(node)
            adjacency[node]?.forEach { visit(it) }
        }

        visit(start)
        result
    }

    /**
     * Check if there's a path.
     */
    fun hasPath(from: T, to: T): Boolean = lock.withLock {
        if (adjacency[from] == null || adjacency[to] == null) return false

        val visited = mutableSetOf<T>()
        val queue = ArrayDeque<T>()
        queue.add(from)

        while (queue.isNotEmpty()) {
            val current = queue.removeFirst()
            if (current == to) return true
            if (current in visited) continue

            visited.add(current)
            adjacency[current]?.forEach { neighbor ->
                if (neighbor !in visited) queue.add(neighbor)
            }
        }

        false
    }

    /**
     * Find shortest path (BFS for unweighted).
     */
    fun shortestPath(from: T, to: T): List<T>? = lock.withLock {
        if (adjacency[from] == null || adjacency[to] == null) return null

        val visited = mutableSetOf<T>()
        val parent = mutableMapOf<T, T>()
        val queue = ArrayDeque<T>()
        queue.add(from)
        visited.add(from)

        while (queue.isNotEmpty()) {
            val current = queue.removeFirst()

            if (current == to) {
                val path = mutableListOf<T>()
                var node: T? = to
                while (node != null) {
                    path.add(0, node)
                    node = parent[node]
                }
                return path
            }

            adjacency[current]?.forEach { neighbor ->
                if (neighbor !in visited) {
                    visited.add(neighbor)
                    parent[neighbor] = current
                    queue.add(neighbor)
                }
            }
        }

        null
    }

    /**
     * Detect if graph has a cycle.
     */
    fun hasCycle(): Boolean = lock.withLock {
        val white = adjacency.keys.toMutableSet()
        val gray = mutableSetOf<T>()
        val black = mutableSetOf<T>()

        fun hasCycleFrom(node: T): Boolean {
            white.remove(node)
            gray.add(node)

            for (neighbor in adjacency[node] ?: emptySet()) {
                if (neighbor in gray) return true
                if (neighbor in white && hasCycleFrom(neighbor)) return true
            }

            gray.remove(node)
            black.add(node)
            return false
        }

        for (node in white.toList()) {
            if (hasCycleFrom(node)) return true
        }
        false
    }

    /**
     * Topological sort (only valid for DAGs).
     */
    fun topologicalSort(): List<T>? = lock.withLock {
        if (hasCycle()) return null

        val visited = mutableSetOf<T>()
        val result = mutableListOf<T>()

        fun visit(node: T) {
            if (node in visited) return
            visited.add(node)
            adjacency[node]?.forEach { visit(it) }
            result.add(0, node)
        }

        adjacency.keys.forEach { visit(it) }
        result
    }

    /**
     * Get in-degree of a node.
     */
    fun inDegree(node: T): Int = lock.withLock {
        adjacency.values.count { node in it }
    }

    /**
     * Get out-degree of a node.
     */
    fun outDegree(node: T): Int = lock.withLock {
        adjacency[node]?.size ?: 0
    }

    /**
     * Clear the graph.
     */
    fun clear() = lock.withLock {
        adjacency.clear()
        weights.clear()
    }
}

/**
 * Undirected graph.
 */
class UndirectedGraph<T> {
    private val directed = DirectedGraph<T>()

    fun addNode(node: T) = directed.addNode(node)

    fun addEdge(a: T, b: T, weight: Double? = null) {
        directed.addEdge(a, b, weight)
        directed.addEdge(b, a, weight)
    }

    fun removeEdge(a: T, b: T): Boolean {
        val r1 = directed.removeEdge(a, b)
        val r2 = directed.removeEdge(b, a)
        return r1 || r2
    }

    fun hasEdge(a: T, b: T): Boolean = directed.hasEdge(a, b)
    fun neighbors(node: T): List<T> = directed.neighbors(node)
    fun nodes(): List<T> = directed.nodes()
    fun bfs(start: T): List<T> = directed.bfs(start)
    fun dfs(start: T): List<T> = directed.dfs(start)

    fun isConnected(): Boolean {
        val allNodes = nodes()
        if (allNodes.isEmpty()) return true
        val visited = bfs(allNodes[0])
        return visited.size == allNodes.size
    }
}
