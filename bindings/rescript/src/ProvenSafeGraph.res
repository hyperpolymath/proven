// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeGraph - Typed wrapper for graph data structures and algorithms.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * The JS FFI layer provides DirectedGraph and UndirectedGraph classes
 * backed by native graph implementations.
 */

/** Opaque handle to a JavaScript DirectedGraph instance. */
type directedGraph

/** Opaque handle to a JavaScript UndirectedGraph instance. */
type undirectedGraph

/** JavaScript bindings to the SafeGraph FFI wrapper. */
module SafeGraphJs = {
  @module("../../javascript/src/safe_graph.js") @scope("DirectedGraph")
  external forStrings: unit => directedGraph = "forStrings"

  @module("../../javascript/src/safe_graph.js") @scope("DirectedGraph")
  external forNumbers: unit => directedGraph = "forNumbers"

  @module("../../javascript/src/safe_graph.js") @scope("UndirectedGraph")
  external undirectedForStrings: unit => undirectedGraph = "forStrings"
}

/**
 * Create a new directed graph for string nodes.
 *
 * @returns A new DirectedGraph handle.
 */
let directedForStrings = SafeGraphJs.forStrings

/**
 * Create a new directed graph for number nodes.
 *
 * @returns A new DirectedGraph handle.
 */
let directedForNumbers = SafeGraphJs.forNumbers

/**
 * Create a new undirected graph for string nodes.
 *
 * @returns A new UndirectedGraph handle.
 */
let undirectedForStrings = SafeGraphJs.undirectedForStrings
