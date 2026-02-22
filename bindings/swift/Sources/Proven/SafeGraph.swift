// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Directed graph delegated to libproven FFI.
///
/// Graph construction and edge queries are performed by the formally
/// verified Idris 2 core via the Zig FFI bridge. This class manages the
/// lifecycle of the C-allocated graph.

import CProven

/// Directed graph backed by libproven. Nodes are identified by size_t indices.
public final class SafeGraph {
    private let graph: UnsafeMutablePointer<ProvenGraph>

    /// Create a directed graph with the given number of nodes.
    /// Returns nil if allocation fails.
    public init?(nodeCount: Int) {
        guard let ptr = proven_graph_create(nodeCount) else {
            return nil
        }
        self.graph = ptr
    }

    deinit {
        proven_graph_free(graph)
    }

    /// Add a directed edge from one node to another.
    @discardableResult
    public func addEdge(from: Int, to: Int) -> Result<Void, ProvenError> {
        let status = proven_graph_add_edge(graph, from, to)
        if let error = ProvenError.fromStatus(status) {
            return .failure(error)
        }
        return .success(())
    }

    /// Check if a directed edge exists between two nodes.
    public func hasEdge(from: Int, to: Int) -> Bool {
        proven_graph_has_edge(graph, from, to)
    }
}
