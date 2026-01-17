// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * Edge in a graph.
 */
export interface Edge<T, W = number> {
  from: T;
  to: T;
  weight?: W;
}

/**
 * DirectedGraph is a directed graph with optional edge weights.
 */
export class DirectedGraph<T> {
  private readonly adjacency: Map<string, Set<string>> = new Map();
  private readonly weights: Map<string, number> = new Map();
  private readonly nodeData: Map<string, T> = new Map();
  private readonly keyFn: (node: T) => string;

  /**
   * Create a directed graph.
   * @param keyFn Function to get a unique key from a node (default: JSON.stringify)
   */
  constructor(keyFn: (node: T) => string = (n) => JSON.stringify(n)) {
    this.keyFn = keyFn;
  }

  /**
   * Create a graph for string nodes.
   */
  static forStrings(): DirectedGraph<string> {
    return new DirectedGraph<string>((n) => n);
  }

  /**
   * Create a graph for number nodes.
   */
  static forNumbers(): DirectedGraph<number> {
    return new DirectedGraph<number>((n) => n.toString());
  }

  /**
   * Add a node.
   */
  addNode(node: T): void {
    const key = this.keyFn(node);
    if (!this.adjacency.has(key)) {
      this.adjacency.set(key, new Set());
      this.nodeData.set(key, node);
    }
  }

  /**
   * Add an edge.
   */
  addEdge(from: T, to: T, weight?: number): void {
    this.addNode(from);
    this.addNode(to);

    const fromKey = this.keyFn(from);
    const toKey = this.keyFn(to);

    this.adjacency.get(fromKey)!.add(toKey);

    if (weight !== undefined) {
      this.weights.set(`${fromKey}->${toKey}`, weight);
    }
  }

  /**
   * Remove an edge.
   */
  removeEdge(from: T, to: T): boolean {
    const fromKey = this.keyFn(from);
    const toKey = this.keyFn(to);

    const neighbors = this.adjacency.get(fromKey);
    if (!neighbors) return false;

    const deleted = neighbors.delete(toKey);
    if (deleted) {
      this.weights.delete(`${fromKey}->${toKey}`);
    }
    return deleted;
  }

  /**
   * Remove a node and all its edges.
   */
  removeNode(node: T): boolean {
    const key = this.keyFn(node);
    if (!this.adjacency.has(key)) return false;

    // Remove outgoing edges
    const neighbors = this.adjacency.get(key)!;
    for (const neighbor of neighbors) {
      this.weights.delete(`${key}->${neighbor}`);
    }
    this.adjacency.delete(key);
    this.nodeData.delete(key);

    // Remove incoming edges
    for (const [nodeKey, nodeNeighbors] of this.adjacency) {
      if (nodeNeighbors.delete(key)) {
        this.weights.delete(`${nodeKey}->${key}`);
      }
    }

    return true;
  }

  /**
   * Check if node exists.
   */
  hasNode(node: T): boolean {
    return this.adjacency.has(this.keyFn(node));
  }

  /**
   * Check if edge exists.
   */
  hasEdge(from: T, to: T): boolean {
    const fromKey = this.keyFn(from);
    const toKey = this.keyFn(to);
    return this.adjacency.get(fromKey)?.has(toKey) ?? false;
  }

  /**
   * Get edge weight.
   */
  getWeight(from: T, to: T): number | undefined {
    const fromKey = this.keyFn(from);
    const toKey = this.keyFn(to);
    return this.weights.get(`${fromKey}->${toKey}`);
  }

  /**
   * Get neighbors of a node.
   */
  neighbors(node: T): T[] {
    const key = this.keyFn(node);
    const neighborKeys = this.adjacency.get(key);
    if (!neighborKeys) return [];

    return Array.from(neighborKeys)
      .map((k) => this.nodeData.get(k))
      .filter((n): n is T => n !== undefined);
  }

  /**
   * Get all nodes.
   */
  nodes(): T[] {
    return Array.from(this.nodeData.values());
  }

  /**
   * Get all edges.
   */
  edges(): Edge<T>[] {
    const result: Edge<T>[] = [];
    for (const [fromKey, neighbors] of this.adjacency) {
      const from = this.nodeData.get(fromKey)!;
      for (const toKey of neighbors) {
        const to = this.nodeData.get(toKey)!;
        const weight = this.weights.get(`${fromKey}->${toKey}`);
        result.push({ from, to, weight });
      }
    }
    return result;
  }

  /**
   * Get node count.
   */
  get nodeCount(): number {
    return this.adjacency.size;
  }

  /**
   * Get edge count.
   */
  get edgeCount(): number {
    let count = 0;
    for (const neighbors of this.adjacency.values()) {
      count += neighbors.size;
    }
    return count;
  }

  /**
   * Breadth-first search from a starting node.
   */
  bfs(start: T): T[] {
    const startKey = this.keyFn(start);
    if (!this.adjacency.has(startKey)) return [];

    const visited = new Set<string>();
    const result: T[] = [];
    const queue: string[] = [startKey];

    while (queue.length > 0) {
      const current = queue.shift()!;
      if (visited.has(current)) continue;

      visited.add(current);
      result.push(this.nodeData.get(current)!);

      const neighbors = this.adjacency.get(current) ?? new Set();
      for (const neighbor of neighbors) {
        if (!visited.has(neighbor)) {
          queue.push(neighbor);
        }
      }
    }

    return result;
  }

  /**
   * Depth-first search from a starting node.
   */
  dfs(start: T): T[] {
    const startKey = this.keyFn(start);
    if (!this.adjacency.has(startKey)) return [];

    const visited = new Set<string>();
    const result: T[] = [];

    const visit = (key: string): void => {
      if (visited.has(key)) return;
      visited.add(key);
      result.push(this.nodeData.get(key)!);

      const neighbors = this.adjacency.get(key) ?? new Set();
      for (const neighbor of neighbors) {
        visit(neighbor);
      }
    };

    visit(startKey);
    return result;
  }

  /**
   * Check if there's a path from source to target.
   */
  hasPath(source: T, target: T): boolean {
    const sourceKey = this.keyFn(source);
    const targetKey = this.keyFn(target);

    if (!this.adjacency.has(sourceKey) || !this.adjacency.has(targetKey)) {
      return false;
    }

    const visited = new Set<string>();
    const queue: string[] = [sourceKey];

    while (queue.length > 0) {
      const current = queue.shift()!;
      if (current === targetKey) return true;
      if (visited.has(current)) continue;

      visited.add(current);
      const neighbors = this.adjacency.get(current) ?? new Set();
      for (const neighbor of neighbors) {
        if (!visited.has(neighbor)) {
          queue.push(neighbor);
        }
      }
    }

    return false;
  }

  /**
   * Find shortest path using BFS (unweighted).
   */
  shortestPath(source: T, target: T): Result<T[]> {
    const sourceKey = this.keyFn(source);
    const targetKey = this.keyFn(target);

    if (!this.adjacency.has(sourceKey)) {
      return { ok: false, error: 'Source node not found' };
    }
    if (!this.adjacency.has(targetKey)) {
      return { ok: false, error: 'Target node not found' };
    }

    const visited = new Set<string>();
    const parent = new Map<string, string>();
    const queue: string[] = [sourceKey];
    visited.add(sourceKey);

    while (queue.length > 0) {
      const current = queue.shift()!;

      if (current === targetKey) {
        // Reconstruct path
        const path: T[] = [];
        let node: string | undefined = targetKey;
        while (node !== undefined) {
          path.unshift(this.nodeData.get(node)!);
          node = parent.get(node);
        }
        return { ok: true, value: path };
      }

      const neighbors = this.adjacency.get(current) ?? new Set();
      for (const neighbor of neighbors) {
        if (!visited.has(neighbor)) {
          visited.add(neighbor);
          parent.set(neighbor, current);
          queue.push(neighbor);
        }
      }
    }

    return { ok: false, error: 'No path exists' };
  }

  /**
   * Detect if the graph has a cycle.
   */
  hasCycle(): boolean {
    const white = new Set<string>(this.adjacency.keys()); // unvisited
    const gray = new Set<string>(); // in progress
    const black = new Set<string>(); // completed

    const hasCycleFrom = (key: string): boolean => {
      white.delete(key);
      gray.add(key);

      const neighbors = this.adjacency.get(key) ?? new Set();
      for (const neighbor of neighbors) {
        if (gray.has(neighbor)) return true; // back edge
        if (white.has(neighbor) && hasCycleFrom(neighbor)) return true;
      }

      gray.delete(key);
      black.add(key);
      return false;
    };

    for (const key of Array.from(white)) {
      if (hasCycleFrom(key)) return true;
    }

    return false;
  }

  /**
   * Topological sort (only valid for DAGs).
   */
  topologicalSort(): Result<T[]> {
    if (this.hasCycle()) {
      return { ok: false, error: 'Graph has a cycle, topological sort not possible' };
    }

    const visited = new Set<string>();
    const result: string[] = [];

    const visit = (key: string): void => {
      if (visited.has(key)) return;
      visited.add(key);

      const neighbors = this.adjacency.get(key) ?? new Set();
      for (const neighbor of neighbors) {
        visit(neighbor);
      }

      result.unshift(key);
    };

    for (const key of this.adjacency.keys()) {
      visit(key);
    }

    return {
      ok: true,
      value: result.map((k) => this.nodeData.get(k)!),
    };
  }

  /**
   * Get in-degree of a node.
   */
  inDegree(node: T): number {
    const key = this.keyFn(node);
    let count = 0;
    for (const neighbors of this.adjacency.values()) {
      if (neighbors.has(key)) count++;
    }
    return count;
  }

  /**
   * Get out-degree of a node.
   */
  outDegree(node: T): number {
    const key = this.keyFn(node);
    return this.adjacency.get(key)?.size ?? 0;
  }

  /**
   * Clear the graph.
   */
  clear(): void {
    this.adjacency.clear();
    this.weights.clear();
    this.nodeData.clear();
  }
}

/**
 * UndirectedGraph is an undirected graph.
 */
export class UndirectedGraph<T> {
  private readonly directed: DirectedGraph<T>;
  private readonly keyFn: (node: T) => string;

  constructor(keyFn: (node: T) => string = (n) => JSON.stringify(n)) {
    this.keyFn = keyFn;
    this.directed = new DirectedGraph(keyFn);
  }

  /**
   * Create a graph for string nodes.
   */
  static forStrings(): UndirectedGraph<string> {
    return new UndirectedGraph<string>((n) => n);
  }

  /**
   * Add a node.
   */
  addNode(node: T): void {
    this.directed.addNode(node);
  }

  /**
   * Add an edge (bidirectional).
   */
  addEdge(a: T, b: T, weight?: number): void {
    this.directed.addEdge(a, b, weight);
    this.directed.addEdge(b, a, weight);
  }

  /**
   * Remove an edge (bidirectional).
   */
  removeEdge(a: T, b: T): boolean {
    const removed1 = this.directed.removeEdge(a, b);
    const removed2 = this.directed.removeEdge(b, a);
    return removed1 || removed2;
  }

  /**
   * Check if edge exists.
   */
  hasEdge(a: T, b: T): boolean {
    return this.directed.hasEdge(a, b);
  }

  /**
   * Get neighbors.
   */
  neighbors(node: T): T[] {
    return this.directed.neighbors(node);
  }

  /**
   * Get all nodes.
   */
  nodes(): T[] {
    return this.directed.nodes();
  }

  /**
   * BFS from start node.
   */
  bfs(start: T): T[] {
    return this.directed.bfs(start);
  }

  /**
   * DFS from start node.
   */
  dfs(start: T): T[] {
    return this.directed.dfs(start);
  }

  /**
   * Check if connected.
   */
  isConnected(): boolean {
    const allNodes = this.nodes();
    if (allNodes.length === 0) return true;

    const visited = this.bfs(allNodes[0]);
    return visited.length === allNodes.length;
  }
}

export const SafeGraph = {
  DirectedGraph,
  UndirectedGraph,
};
