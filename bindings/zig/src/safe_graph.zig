// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeGraph - FFI bindings to libproven graph operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for graph operations.
pub const GraphError = error{
    CreationFailed,
    OutOfBounds,
    ProvenError,
};

/// Directed graph backed by libproven.
pub const Graph = struct {
    ptr: *c.ProvenGraph,

    /// Add directed edge from -> to via libproven.
    pub fn addEdge(self: Graph, from: usize, to: usize) GraphError!void {
        const status = c.proven_graph_add_edge(self.ptr, from, to);
        return switch (status) {
            c.PROVEN_OK => {},
            c.PROVEN_ERR_OUT_OF_BOUNDS => error.OutOfBounds,
            else => error.ProvenError,
        };
    }

    /// Check if directed edge exists via libproven.
    pub fn hasEdge(self: Graph, from: usize, to: usize) bool {
        return c.proven_graph_has_edge(self.ptr, from, to);
    }

    /// Free graph via libproven.
    pub fn deinit(self: Graph) void {
        c.proven_graph_free(self.ptr);
    }
};

/// Create a directed graph via libproven.
/// node_count: Number of nodes (max 10,000).
pub fn create(node_count: usize) GraphError!Graph {
    const ptr = c.proven_graph_create(node_count);
    if (ptr == null) return error.CreationFailed;
    return Graph{ .ptr = ptr.? };
}

test "create and add edges" {
    const g = try create(5);
    defer g.deinit();
    try g.addEdge(0, 1);
    try g.addEdge(1, 2);
    try std.testing.expect(g.hasEdge(0, 1));
    try std.testing.expect(!g.hasEdge(2, 0));
}
