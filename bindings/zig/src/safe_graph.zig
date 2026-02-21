// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe graph data structure operations.

const std = @import("std");

pub const GraphError = error{
    VertexNotFound,
    EdgeExists,
    MaxVerticesReached,
    MaxEdgesReached,
    CycleDetected,
};

/// Directed graph with fixed capacity.
pub fn DirectedGraph(comptime V: type, comptime max_vertices: usize, comptime max_edges: usize) type {
    return struct {
        const Self = @This();

        const Edge = struct {
            source: usize,
            target: usize,
            weight: i64,
        };

        vertices: [max_vertices]?V = [_]?V{null} ** max_vertices,
        edges: [max_edges]?Edge = [_]?Edge{null} ** max_edges,
        vertex_count: usize = 0,
        edge_count: usize = 0,

        pub fn init() Self {
            return .{};
        }

        /// Add a vertex.
        pub fn addVertex(self: *Self, value: V) GraphError!usize {
            if (self.vertex_count >= max_vertices) return error.MaxVerticesReached;
            const idx = self.vertex_count;
            self.vertices[idx] = value;
            self.vertex_count += 1;
            return idx;
        }

        /// Find vertex index by value.
        pub fn findVertex(self: *const Self, value: V) ?usize {
            for (self.vertices, 0..) |v_opt, i| {
                if (v_opt) |v| {
                    if (std.meta.eql(v, value)) return i;
                }
            }
            return null;
        }

        /// Add an edge.
        pub fn addEdge(self: *Self, source: usize, target: usize, weight: i64) GraphError!void {
            if (source >= self.vertex_count or target >= self.vertex_count) {
                return error.VertexNotFound;
            }
            if (self.edge_count >= max_edges) return error.MaxEdgesReached;

            self.edges[self.edge_count] = .{ .source = source, .target = target, .weight = weight };
            self.edge_count += 1;
        }

        /// Check if edge exists.
        pub fn hasEdge(self: *const Self, source: usize, target: usize) bool {
            for (self.edges[0..self.edge_count]) |e_opt| {
                if (e_opt) |e| {
                    if (e.source == source and e.target == target) return true;
                }
            }
            return false;
        }

        /// Get neighbors of a vertex.
        pub fn neighbors(self: *const Self, vertex: usize, buf: []usize) []usize {
            var count: usize = 0;
            for (self.edges[0..self.edge_count]) |e_opt| {
                if (e_opt) |e| {
                    if (e.source == vertex and count < buf.len) {
                        buf[count] = e.target;
                        count += 1;
                    }
                }
            }
            return buf[0..count];
        }

        /// Get out-degree of a vertex.
        pub fn outDegree(self: *const Self, vertex: usize) usize {
            var count: usize = 0;
            for (self.edges[0..self.edge_count]) |e_opt| {
                if (e_opt) |e| {
                    if (e.source == vertex) count += 1;
                }
            }
            return count;
        }

        /// Get in-degree of a vertex.
        pub fn inDegree(self: *const Self, vertex: usize) usize {
            var count: usize = 0;
            for (self.edges[0..self.edge_count]) |e_opt| {
                if (e_opt) |e| {
                    if (e.target == vertex) count += 1;
                }
            }
            return count;
        }

        /// Check for cycles using DFS.
        pub fn hasCycle(self: *const Self) bool {
            var visited = [_]bool{false} ** max_vertices;
            var stack = [_]bool{false} ** max_vertices;

            var i: usize = 0;
            while (i < self.vertex_count) : (i += 1) {
                if (self.hasCycleFromVertex(i, &visited, &stack)) {
                    return true;
                }
            }
            return false;
        }

        fn hasCycleFromVertex(self: *const Self, v: usize, visited: *[max_vertices]bool, stack: *[max_vertices]bool) bool {
            if (stack[v]) return true;
            if (visited[v]) return false;

            visited[v] = true;
            stack[v] = true;

            for (self.edges[0..self.edge_count]) |e_opt| {
                if (e_opt) |e| {
                    if (e.source == v) {
                        if (self.hasCycleFromVertex(e.target, visited, stack)) {
                            return true;
                        }
                    }
                }
            }

            stack[v] = false;
            return false;
        }
    };
}

test "DirectedGraph" {
    var graph = DirectedGraph([]const u8, 10, 20).init();

    const a = try graph.addVertex("A");
    const b = try graph.addVertex("B");
    const c = try graph.addVertex("C");

    try graph.addEdge(a, b, 1);
    try graph.addEdge(b, c, 1);

    try std.testing.expect(graph.hasEdge(a, b));
    try std.testing.expect(!graph.hasEdge(a, c));
    try std.testing.expect(!graph.hasCycle());

    // Add cycle
    try graph.addEdge(c, a, 1);
    try std.testing.expect(graph.hasCycle());
}
