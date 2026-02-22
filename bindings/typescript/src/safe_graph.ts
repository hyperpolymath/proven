// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeGraph - Typed wrapper for directed graph operations.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import {
  DirectedGraph as JsDirectedGraph,
  Edge as JsEdge,
  SafeGraph as JsSafeGraph,
} from '../../javascript/src/safe_graph.js';

/** Result type returned by graph operations. */
export type Result<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Edge representation.
 * Mirrors the FFI Edge class.
 */
export interface Edge {
  readonly from: number;
  readonly to: number;
}

/**
 * Typed wrapper around the FFI-backed DirectedGraph.
 *
 * Every method calls through to the JavaScript FFI wrapper, which in turn
 * calls the formally verified Idris 2 code via the Zig FFI bridge.
 * Nodes are identified by integer IDs (0-based).
 */
export class DirectedGraph {
  /** The underlying JS FFI directed graph instance. */
  readonly #inner: InstanceType<typeof JsDirectedGraph>;

  /** Private constructor -- use DirectedGraph.create() instead. */
  private constructor(inner: InstanceType<typeof JsDirectedGraph>) {
    this.#inner = inner;
  }

  /**
   * Create a directed graph via FFI.
   * Delegates to proven_graph_create.
   *
   * @param nodeCount - Number of nodes in the graph.
   * @returns Result with the DirectedGraph instance, or error.
   */
  static create(nodeCount: number): Result<DirectedGraph> {
    const result = JsSafeGraph.directed(nodeCount) as Result<InstanceType<typeof JsDirectedGraph>>;
    if (!result.ok) return result;
    return { ok: true, value: new DirectedGraph(result.value) };
  }

  /**
   * Add a directed edge.
   * Delegates to proven_graph_add_edge via FFI.
   *
   * @param from - Source node ID.
   * @param to - Destination node ID.
   * @returns Result indicating success or an error string.
   */
  addEdge(from: number, to: number): Result<undefined> {
    return this.#inner.addEdge(from, to) as Result<undefined>;
  }

  /**
   * Check if an edge exists.
   * Delegates to proven_graph_has_edge via FFI.
   *
   * @param from - Source node ID.
   * @param to - Destination node ID.
   * @returns True if the edge exists.
   */
  hasEdge(from: number, to: number): boolean {
    return this.#inner.hasEdge(from, to);
  }

  /**
   * Close and free the native graph.
   * Delegates to proven_graph_free via FFI.
   */
  close(): void {
    this.#inner.close();
  }
}

/**
 * SafeGraph namespace.
 * All operations delegate to the JavaScript FFI binding.
 */
export class SafeGraph {
  /**
   * Create a directed graph.
   * Delegates to proven_graph_create via FFI.
   *
   * @param nodeCount - Number of nodes.
   * @returns Result with the DirectedGraph instance, or error.
   */
  static directed(nodeCount: number): Result<DirectedGraph> {
    return DirectedGraph.create(nodeCount);
  }
}
