// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeGraph - Directed graph operations.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Edge representation.
 */
export class Edge {
  /** @type {number} */
  from;
  /** @type {number} */
  to;

  /**
   * @param {number} from - Source node.
   * @param {number} to - Destination node.
   */
  constructor(from, to) {
    this.from = from;
    this.to = to;
  }
}

/**
 * Directed graph backed by the libproven FFI.
 * Nodes are identified by integer IDs (0-based).
 */
export class DirectedGraph {
  /** @type {Deno.PointerObject|null} */
  #ptr = null;

  /**
   * Create a directed graph.
   *
   * @param {number} nodeCount - Number of nodes.
   * @returns {{ ok: true, value: DirectedGraph } | { ok: false, error: string }}
   */
  static create(nodeCount) {
    const symbols = getLib();
    const ptr = symbols.proven_graph_create(nodeCount);
    if (ptr === null) return err('Failed to create graph');
    const graph = new DirectedGraph();
    graph.#ptr = ptr;
    return ok(graph);
  }

  /**
   * Add a directed edge.
   *
   * @param {number} from - Source node.
   * @param {number} to - Destination node.
   * @returns {{ ok: true, value: undefined } | { ok: false, error: string }}
   */
  addEdge(from, to) {
    if (this.#ptr === null) return err('Graph is closed');
    const symbols = getLib();
    const status = symbols.proven_graph_add_edge(this.#ptr, from, to);
    if (status !== ProvenStatus.OK) return err(statusToError(status));
    return ok(undefined);
  }

  /**
   * Check if an edge exists.
   *
   * @param {number} from - Source node.
   * @param {number} to - Destination node.
   * @returns {boolean}
   */
  hasEdge(from, to) {
    if (this.#ptr === null) return false;
    const symbols = getLib();
    return symbols.proven_graph_has_edge(this.#ptr, from, to);
  }

  /**
   * Close and free the native graph.
   */
  close() {
    if (this.#ptr !== null) {
      const symbols = getLib();
      symbols.proven_graph_free(this.#ptr);
      this.#ptr = null;
    }
  }
}

/**
 * SafeGraph namespace.
 */
export class SafeGraph {
  /**
   * Create a directed graph.
   *
   * @param {number} nodeCount
   * @returns {{ ok: true, value: DirectedGraph } | { ok: false, error: string }}
   */
  static directed(nodeCount) {
    return DirectedGraph.create(nodeCount);
  }
}
