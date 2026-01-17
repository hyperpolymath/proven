// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeGraph - Directed graphs with cycle detection.
 *
 * Provides safe graph operations with traversal algorithms.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Edge in a graph.
 *
 * @template W Weight type
 */
export class Edge {
  /** @type {string} */
  #from;
  /** @type {string} */
  #to;
  /** @type {W | undefined} */
  #weight;

  /**
   * Create an edge.
   *
   * @param {string} from - Source node
   * @param {string} to - Target node
   * @param {W} [weight] - Optional weight
   */
  constructor(from, to, weight) {
    this.#from = from;
    this.#to = to;
    this.#weight = weight;
  }

  get from() {
    return this.#from;
  }
  get to() {
    return this.#to;
  }
  get weight() {
    return this.#weight;
  }

  /**
   * Check equality.
   *
   * @param {Edge<W>} other - Other edge
   * @returns {boolean}
   */
  equals(other) {
    return this.#from === other.from && this.#to === other.to;
  }
}

/**
 * Directed graph.
 *
 * @template N Node data type
 * @template W Edge weight type
 */
export class DirectedGraph {
  /** @type {Map<string, N | undefined>} */
  #nodes;
  /** @type {Map<string, Map<string, W | undefined>>} */
  #edges;

  /**
   * Create a directed graph.
   */
  constructor() {
    this.#nodes = new Map();
    this.#edges = new Map();
  }

  /**
   * Get number of nodes.
   *
   * @returns {number}
   */
  get nodeCount() {
    return this.#nodes.size;
  }

  /**
   * Get number of edges.
   *
   * @returns {number}
   */
  get edgeCount() {
    let count = 0;
    for (const targets of this.#edges.values()) {
      count += targets.size;
    }
    return count;
  }

  /**
   * Add a node.
   *
   * @param {string} id - Node ID
   * @param {N} [data] - Node data
   * @returns {boolean} True if node was added (not already present)
   */
  addNode(id, data) {
    if (this.#nodes.has(id)) {
      return false;
    }
    this.#nodes.set(id, data);
    this.#edges.set(id, new Map());
    return true;
  }

  /**
   * Remove a node and all its edges.
   *
   * @param {string} id - Node ID
   * @returns {boolean} True if node was removed
   */
  removeNode(id) {
    if (!this.#nodes.has(id)) {
      return false;
    }

    // Remove all edges from this node
    this.#edges.delete(id);

    // Remove all edges to this node
    for (const targets of this.#edges.values()) {
      targets.delete(id);
    }

    this.#nodes.delete(id);
    return true;
  }

  /**
   * Check if node exists.
   *
   * @param {string} id - Node ID
   * @returns {boolean}
   */
  hasNode(id) {
    return this.#nodes.has(id);
  }

  /**
   * Get node data.
   *
   * @param {string} id - Node ID
   * @returns {N | undefined}
   */
  getNode(id) {
    return this.#nodes.get(id);
  }

  /**
   * Get all node IDs.
   *
   * @returns {string[]}
   */
  nodes() {
    return Array.from(this.#nodes.keys());
  }

  /**
   * Add an edge.
   *
   * @param {string} from - Source node
   * @param {string} to - Target node
   * @param {W} [weight] - Optional weight
   * @returns {{ ok: true, value: void } | { ok: false, error: string }}
   */
  addEdge(from, to, weight) {
    if (!this.#nodes.has(from)) {
      return err(`Source node '${from}' does not exist`);
    }
    if (!this.#nodes.has(to)) {
      return err(`Target node '${to}' does not exist`);
    }

    this.#edges.get(from).set(to, weight);
    return ok(undefined);
  }

  /**
   * Remove an edge.
   *
   * @param {string} from - Source node
   * @param {string} to - Target node
   * @returns {boolean} True if edge was removed
   */
  removeEdge(from, to) {
    const targets = this.#edges.get(from);
    if (!targets) {
      return false;
    }
    return targets.delete(to);
  }

  /**
   * Check if edge exists.
   *
   * @param {string} from - Source node
   * @param {string} to - Target node
   * @returns {boolean}
   */
  hasEdge(from, to) {
    const targets = this.#edges.get(from);
    return targets ? targets.has(to) : false;
  }

  /**
   * Get edge weight.
   *
   * @param {string} from - Source node
   * @param {string} to - Target node
   * @returns {W | undefined}
   */
  getEdgeWeight(from, to) {
    const targets = this.#edges.get(from);
    return targets ? targets.get(to) : undefined;
  }

  /**
   * Get all edges.
   *
   * @returns {Edge<W>[]}
   */
  edges() {
    const result = [];
    for (const [from, targets] of this.#edges) {
      for (const [to, weight] of targets) {
        result.push(new Edge(from, to, weight));
      }
    }
    return result;
  }

  /**
   * Get outgoing neighbors of a node.
   *
   * @param {string} id - Node ID
   * @returns {string[]}
   */
  neighbors(id) {
    const targets = this.#edges.get(id);
    return targets ? Array.from(targets.keys()) : [];
  }

  /**
   * Get incoming neighbors of a node.
   *
   * @param {string} id - Node ID
   * @returns {string[]}
   */
  incomingNeighbors(id) {
    const result = [];
    for (const [from, targets] of this.#edges) {
      if (targets.has(id)) {
        result.push(from);
      }
    }
    return result;
  }

  /**
   * Get out-degree of a node.
   *
   * @param {string} id - Node ID
   * @returns {number}
   */
  outDegree(id) {
    const targets = this.#edges.get(id);
    return targets ? targets.size : 0;
  }

  /**
   * Get in-degree of a node.
   *
   * @param {string} id - Node ID
   * @returns {number}
   */
  inDegree(id) {
    let count = 0;
    for (const targets of this.#edges.values()) {
      if (targets.has(id)) {
        count++;
      }
    }
    return count;
  }

  /**
   * Check if graph contains a cycle.
   *
   * @returns {boolean}
   */
  hasCycle() {
    const visited = new Set();
    const recursionStack = new Set();

    const hasCycleDfs = (node) => {
      visited.add(node);
      recursionStack.add(node);

      for (const neighbor of this.neighbors(node)) {
        if (!visited.has(neighbor)) {
          if (hasCycleDfs(neighbor)) {
            return true;
          }
        } else if (recursionStack.has(neighbor)) {
          return true;
        }
      }

      recursionStack.delete(node);
      return false;
    };

    for (const node of this.#nodes.keys()) {
      if (!visited.has(node)) {
        if (hasCycleDfs(node)) {
          return true;
        }
      }
    }

    return false;
  }

  /**
   * Find a cycle if one exists.
   *
   * @returns {string[] | undefined}
   */
  findCycle() {
    const visited = new Set();
    const recursionStack = new Set();
    const parent = new Map();

    const findCycleDfs = (node) => {
      visited.add(node);
      recursionStack.add(node);

      for (const neighbor of this.neighbors(node)) {
        if (!visited.has(neighbor)) {
          parent.set(neighbor, node);
          const result = findCycleDfs(neighbor);
          if (result) {
            return result;
          }
        } else if (recursionStack.has(neighbor)) {
          // Found cycle - reconstruct path
          const cycle = [neighbor];
          let current = node;
          while (current !== neighbor) {
            cycle.push(current);
            current = parent.get(current);
          }
          cycle.push(neighbor);
          return cycle.reverse();
        }
      }

      recursionStack.delete(node);
      return undefined;
    };

    for (const node of this.#nodes.keys()) {
      if (!visited.has(node)) {
        const cycle = findCycleDfs(node);
        if (cycle) {
          return cycle;
        }
      }
    }

    return undefined;
  }

  /**
   * Topological sort (fails if cycle exists).
   *
   * @returns {{ ok: true, value: string[] } | { ok: false, error: string }}
   */
  topologicalSort() {
    if (this.hasCycle()) {
      return err('Graph contains a cycle');
    }

    const visited = new Set();
    const result = [];

    const visit = (node) => {
      if (visited.has(node)) {
        return;
      }
      visited.add(node);

      for (const neighbor of this.neighbors(node)) {
        visit(neighbor);
      }

      result.unshift(node);
    };

    for (const node of this.#nodes.keys()) {
      visit(node);
    }

    return ok(result);
  }

  /**
   * Breadth-first search.
   *
   * @param {string} start - Start node
   * @returns {string[]}
   */
  bfs(start) {
    if (!this.#nodes.has(start)) {
      return [];
    }

    const visited = new Set();
    const queue = [start];
    const result = [];

    while (queue.length > 0) {
      const node = queue.shift();
      if (visited.has(node)) {
        continue;
      }

      visited.add(node);
      result.push(node);

      for (const neighbor of this.neighbors(node)) {
        if (!visited.has(neighbor)) {
          queue.push(neighbor);
        }
      }
    }

    return result;
  }

  /**
   * Depth-first search.
   *
   * @param {string} start - Start node
   * @returns {string[]}
   */
  dfs(start) {
    if (!this.#nodes.has(start)) {
      return [];
    }

    const visited = new Set();
    const result = [];

    const visit = (node) => {
      if (visited.has(node)) {
        return;
      }
      visited.add(node);
      result.push(node);

      for (const neighbor of this.neighbors(node)) {
        visit(neighbor);
      }
    };

    visit(start);
    return result;
  }

  /**
   * Find shortest path (unweighted).
   *
   * @param {string} from - Start node
   * @param {string} to - End node
   * @returns {string[] | undefined}
   */
  shortestPath(from, to) {
    if (!this.#nodes.has(from) || !this.#nodes.has(to)) {
      return undefined;
    }

    if (from === to) {
      return [from];
    }

    const visited = new Set();
    const queue = [[from]];

    while (queue.length > 0) {
      const path = queue.shift();
      const node = path[path.length - 1];

      if (visited.has(node)) {
        continue;
      }
      visited.add(node);

      for (const neighbor of this.neighbors(node)) {
        const newPath = [...path, neighbor];

        if (neighbor === to) {
          return newPath;
        }

        if (!visited.has(neighbor)) {
          queue.push(newPath);
        }
      }
    }

    return undefined;
  }

  /**
   * Check if path exists between two nodes.
   *
   * @param {string} from - Start node
   * @param {string} to - End node
   * @returns {boolean}
   */
  hasPath(from, to) {
    return this.shortestPath(from, to) !== undefined;
  }

  /**
   * Get all nodes reachable from a start node.
   *
   * @param {string} start - Start node
   * @returns {Set<string>}
   */
  reachableFrom(start) {
    return new Set(this.bfs(start));
  }

  /**
   * Clear the graph.
   */
  clear() {
    this.#nodes.clear();
    this.#edges.clear();
  }

  /**
   * Create a copy of the graph.
   *
   * @returns {DirectedGraph<N, W>}
   */
  clone() {
    const copy = new DirectedGraph();

    for (const [id, data] of this.#nodes) {
      copy.addNode(id, data);
    }

    for (const [from, targets] of this.#edges) {
      for (const [to, weight] of targets) {
        copy.addEdge(from, to, weight);
      }
    }

    return copy;
  }
}

/**
 * Safe graph utilities.
 */
export class SafeGraph {
  /**
   * Create an empty directed graph.
   *
   * @template N, W
   * @returns {DirectedGraph<N, W>}
   */
  static directed() {
    return new DirectedGraph();
  }

  /**
   * Create a graph from nodes and edges.
   *
   * @template N, W
   * @param {string[]} nodes - Node IDs
   * @param {Array<[string, string, W?]>} edges - Edges as [from, to, weight?]
   * @returns {{ ok: true, value: DirectedGraph<N, W> } | { ok: false, error: string }}
   */
  static create(nodes, edges) {
    const graph = new DirectedGraph();

    for (const node of nodes) {
      graph.addNode(node);
    }

    for (const [from, to, weight] of edges) {
      const result = graph.addEdge(from, to, weight);
      if (!result.ok) {
        return result;
      }
    }

    return ok(graph);
  }

  /**
   * Create a DAG (validates no cycles).
   *
   * @template N, W
   * @param {string[]} nodes - Node IDs
   * @param {Array<[string, string, W?]>} edges - Edges
   * @returns {{ ok: true, value: DirectedGraph<N, W> } | { ok: false, error: string }}
   */
  static dag(nodes, edges) {
    const result = SafeGraph.create(nodes, edges);
    if (!result.ok) {
      return result;
    }

    if (result.value.hasCycle()) {
      return err('Graph contains a cycle (not a DAG)');
    }

    return result;
  }
}
