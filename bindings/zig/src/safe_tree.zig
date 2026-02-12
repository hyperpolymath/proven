// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe tree data structure with depth limits that cannot crash.
//!
//! Provides bounded tree operations with configurable maximum depth
//! and node count. Supports common tree traversals, path operations,
//! and structural queries while preventing unbounded recursion.

const std = @import("std");

/// Error types for tree operations.
pub const TreeError = error{
    MaxDepthExceeded,
    MaxNodesReached,
    NodeNotFound,
    InvalidParent,
    CycleDetected,
    RootAlreadyExists,
    CannotRemoveRoot,
    OutOfMemory,
};

/// Default maximum tree depth.
pub const DEFAULT_MAX_DEPTH = 64;

/// Default maximum nodes per tree.
pub const DEFAULT_MAX_NODES = 1024;

/// Node identifier type.
pub const NodeId = usize;

/// Sentinel value for no parent (root node).
pub const NO_PARENT: NodeId = std.math.maxInt(NodeId);

/// A tree node with value and parent reference.
pub fn TreeNode(comptime V: type) type {
    return struct {
        value: V,
        parent: NodeId,
        depth: usize,
        child_count: usize,
    };
}

/// A safe tree data structure with bounded depth and node count.
pub fn Tree(comptime V: type, comptime max_nodes: usize, comptime max_depth: usize) type {
    return struct {
        const Self = @This();
        const Node = TreeNode(V);

        nodes: [max_nodes]?Node = [_]?Node{null} ** max_nodes,
        node_count: usize = 0,
        root: ?NodeId = null,

        /// Initialize an empty tree.
        pub fn init() Self {
            return .{};
        }

        /// Get the number of nodes in the tree.
        pub fn len(self: *const Self) usize {
            return self.node_count;
        }

        /// Check if tree is empty.
        pub fn isEmpty(self: *const Self) bool {
            return self.node_count == 0;
        }

        /// Get the root node ID.
        pub fn getRoot(self: *const Self) ?NodeId {
            return self.root;
        }

        /// Set the root node (creates new tree or replaces root).
        pub fn setRoot(self: *Self, value: V) TreeError!NodeId {
            if (self.root != null) return error.RootAlreadyExists;
            if (self.node_count >= max_nodes) return error.MaxNodesReached;

            const id = self.node_count;
            self.nodes[id] = .{
                .value = value,
                .parent = NO_PARENT,
                .depth = 0,
                .child_count = 0,
            };
            self.node_count += 1;
            self.root = id;
            return id;
        }

        /// Add a child node to a parent.
        pub fn addChild(self: *Self, parent_id: NodeId, value: V) TreeError!NodeId {
            if (parent_id >= self.node_count) return error.InvalidParent;
            if (self.nodes[parent_id] == null) return error.InvalidParent;
            if (self.node_count >= max_nodes) return error.MaxNodesReached;

            const parent = &self.nodes[parent_id].?;
            const new_depth = parent.depth + 1;

            if (new_depth > max_depth) return error.MaxDepthExceeded;

            const id = self.node_count;
            self.nodes[id] = .{
                .value = value,
                .parent = parent_id,
                .depth = new_depth,
                .child_count = 0,
            };
            self.node_count += 1;

            // Update parent's child count
            self.nodes[parent_id].?.child_count += 1;

            return id;
        }

        /// Get a node's value.
        pub fn getValue(self: *const Self, id: NodeId) ?V {
            if (id >= self.node_count) return null;
            if (self.nodes[id]) |node| {
                return node.value;
            }
            return null;
        }

        /// Set a node's value.
        pub fn setValue(self: *Self, id: NodeId, value: V) TreeError!void {
            if (id >= self.node_count) return error.NodeNotFound;
            if (self.nodes[id]) |*node| {
                node.value = value;
            } else {
                return error.NodeNotFound;
            }
        }

        /// Get a node's parent ID.
        pub fn getParent(self: *const Self, id: NodeId) ?NodeId {
            if (id >= self.node_count) return null;
            if (self.nodes[id]) |node| {
                if (node.parent == NO_PARENT) return null;
                return node.parent;
            }
            return null;
        }

        /// Get a node's depth.
        pub fn getDepth(self: *const Self, id: NodeId) ?usize {
            if (id >= self.node_count) return null;
            if (self.nodes[id]) |node| {
                return node.depth;
            }
            return null;
        }

        /// Check if node is the root.
        pub fn isRoot(self: *const Self, id: NodeId) bool {
            if (self.root) |root_id| {
                return id == root_id;
            }
            return false;
        }

        /// Check if node is a leaf (no children).
        pub fn isLeaf(self: *const Self, id: NodeId) bool {
            if (id >= self.node_count) return false;
            if (self.nodes[id]) |node| {
                return node.child_count == 0;
            }
            return false;
        }

        /// Get children of a node.
        pub fn getChildren(self: *const Self, parent_id: NodeId, buf: []NodeId) []NodeId {
            var count: usize = 0;
            for (self.nodes[0..self.node_count], 0..) |node_opt, id| {
                if (node_opt) |node| {
                    if (node.parent == parent_id and count < buf.len) {
                        buf[count] = id;
                        count += 1;
                    }
                }
            }
            return buf[0..count];
        }

        /// Get number of children for a node.
        pub fn getChildCount(self: *const Self, id: NodeId) usize {
            if (id >= self.node_count) return 0;
            if (self.nodes[id]) |node| {
                return node.child_count;
            }
            return 0;
        }

        /// Get path from root to node.
        pub fn getPath(self: *const Self, id: NodeId, buf: []NodeId) []NodeId {
            if (id >= self.node_count) return buf[0..0];
            if (self.nodes[id] == null) return buf[0..0];

            // Walk up to root, collecting IDs
            var path_len: usize = 0;
            var current = id;

            while (path_len < buf.len) {
                buf[path_len] = current;
                path_len += 1;

                if (self.nodes[current]) |node| {
                    if (node.parent == NO_PARENT) break;
                    current = node.parent;
                } else {
                    break;
                }
            }

            // Reverse the path (currently leaf-to-root, want root-to-leaf)
            var i: usize = 0;
            var j: usize = path_len - 1;
            while (i < j) {
                const tmp = buf[i];
                buf[i] = buf[j];
                buf[j] = tmp;
                i += 1;
                j -= 1;
            }

            return buf[0..path_len];
        }

        /// Check if node A is an ancestor of node B.
        pub fn isAncestor(self: *const Self, ancestor_id: NodeId, descendant_id: NodeId) bool {
            if (ancestor_id >= self.node_count or descendant_id >= self.node_count) {
                return false;
            }

            var current = descendant_id;
            var iterations: usize = 0;

            while (iterations < max_depth + 1) : (iterations += 1) {
                if (self.nodes[current]) |node| {
                    if (node.parent == ancestor_id) return true;
                    if (node.parent == NO_PARENT) return false;
                    current = node.parent;
                } else {
                    return false;
                }
            }

            return false;
        }

        /// Get the lowest common ancestor of two nodes.
        pub fn lowestCommonAncestor(self: *const Self, id_a: NodeId, id_b: NodeId) ?NodeId {
            if (id_a >= self.node_count or id_b >= self.node_count) {
                return null;
            }

            // Get depths
            const depth_a = self.getDepth(id_a) orelse return null;
            const depth_b = self.getDepth(id_b) orelse return null;

            var a = id_a;
            var b = id_b;

            // Level the depths
            var da = depth_a;
            var db = depth_b;

            while (da > db) : (da -= 1) {
                a = self.getParent(a) orelse return null;
            }
            while (db > da) : (db -= 1) {
                b = self.getParent(b) orelse return null;
            }

            // Walk up together
            var iterations: usize = 0;
            while (a != b and iterations < max_depth + 1) : (iterations += 1) {
                a = self.getParent(a) orelse return null;
                b = self.getParent(b) orelse return null;
            }

            if (a == b) return a;
            return null;
        }

        /// Get maximum depth in the tree.
        pub fn getMaxDepth(self: *const Self) usize {
            var max: usize = 0;
            for (self.nodes[0..self.node_count]) |node_opt| {
                if (node_opt) |node| {
                    if (node.depth > max) max = node.depth;
                }
            }
            return max;
        }

        /// Count leaf nodes.
        pub fn countLeaves(self: *const Self) usize {
            var count: usize = 0;
            for (self.nodes[0..self.node_count]) |node_opt| {
                if (node_opt) |node| {
                    if (node.child_count == 0) count += 1;
                }
            }
            return count;
        }

        /// Perform pre-order traversal, writing node IDs to buffer.
        pub fn preorderTraversal(self: *const Self, buf: []NodeId) []NodeId {
            if (self.root == null) return buf[0..0];

            var count: usize = 0;
            var stack: [max_depth + 1]NodeId = undefined;
            var stack_len: usize = 1;
            stack[0] = self.root.?;

            while (stack_len > 0 and count < buf.len) {
                stack_len -= 1;
                const current = stack[stack_len];
                buf[count] = current;
                count += 1;

                // Add children (in reverse order for correct traversal)
                var children: [max_nodes]NodeId = undefined;
                const child_ids = self.getChildren(current, &children);

                var i = child_ids.len;
                while (i > 0 and stack_len < max_depth + 1) {
                    i -= 1;
                    stack[stack_len] = child_ids[i];
                    stack_len += 1;
                }
            }

            return buf[0..count];
        }

        /// Clear the tree.
        pub fn clear(self: *Self) void {
            for (&self.nodes) |*node| {
                node.* = null;
            }
            self.node_count = 0;
            self.root = null;
        }
    };
}

/// Default tree type with standard limits.
pub const DefaultTree = Tree([]const u8, DEFAULT_MAX_NODES, DEFAULT_MAX_DEPTH);

test "basic tree operations" {
    var tree = Tree(i32, 16, 8).init();

    const root = try tree.setRoot(1);
    try std.testing.expectEqual(@as(usize, 1), tree.len());
    try std.testing.expect(tree.isRoot(root));

    const child1 = try tree.addChild(root, 2);
    const child2 = try tree.addChild(root, 3);
    try std.testing.expectEqual(@as(usize, 3), tree.len());

    try std.testing.expectEqual(@as(i32, 1), tree.getValue(root).?);
    try std.testing.expectEqual(@as(i32, 2), tree.getValue(child1).?);
    try std.testing.expectEqual(@as(i32, 3), tree.getValue(child2).?);
}

test "tree depth limits" {
    var tree = Tree(u8, 16, 3).init();

    const n0 = try tree.setRoot(0);
    const n1 = try tree.addChild(n0, 1);
    const n2 = try tree.addChild(n1, 2);
    const n3 = try tree.addChild(n2, 3);

    try std.testing.expectError(error.MaxDepthExceeded, tree.addChild(n3, 4));
}

test "tree path" {
    var tree = Tree(u8, 16, 8).init();

    const root = try tree.setRoot(0);
    const child = try tree.addChild(root, 1);
    const grandchild = try tree.addChild(child, 2);

    var path_buf: [8]NodeId = undefined;
    const path = tree.getPath(grandchild, &path_buf);

    try std.testing.expectEqual(@as(usize, 3), path.len);
    try std.testing.expectEqual(root, path[0]);
    try std.testing.expectEqual(child, path[1]);
    try std.testing.expectEqual(grandchild, path[2]);
}

test "ancestor check" {
    var tree = Tree(u8, 16, 8).init();

    const root = try tree.setRoot(0);
    const child = try tree.addChild(root, 1);
    const grandchild = try tree.addChild(child, 2);

    try std.testing.expect(tree.isAncestor(root, grandchild));
    try std.testing.expect(tree.isAncestor(child, grandchild));
    try std.testing.expect(!tree.isAncestor(grandchild, root));
}

test "lowest common ancestor" {
    var tree = Tree(u8, 16, 8).init();

    const root = try tree.setRoot(0);
    const left = try tree.addChild(root, 1);
    const right = try tree.addChild(root, 2);
    const left_child = try tree.addChild(left, 3);
    const right_child = try tree.addChild(right, 4);

    try std.testing.expectEqual(root, tree.lowestCommonAncestor(left_child, right_child).?);
    try std.testing.expectEqual(left, tree.lowestCommonAncestor(left, left_child).?);
}

test "leaf and depth" {
    var tree = Tree(u8, 16, 8).init();

    const root = try tree.setRoot(0);
    const child1 = try tree.addChild(root, 1);
    _ = try tree.addChild(root, 2);
    _ = try tree.addChild(child1, 3);

    try std.testing.expect(!tree.isLeaf(root));
    try std.testing.expect(!tree.isLeaf(child1));
    try std.testing.expectEqual(@as(usize, 2), tree.countLeaves());
    try std.testing.expectEqual(@as(usize, 2), tree.getMaxDepth());
}

test "preorder traversal" {
    var tree = Tree(i32, 16, 8).init();

    const root = try tree.setRoot(1);
    const left = try tree.addChild(root, 2);
    _ = try tree.addChild(root, 3); // right child
    _ = try tree.addChild(left, 4);
    _ = try tree.addChild(left, 5);

    var buf: [8]NodeId = undefined;
    const order = tree.preorderTraversal(&buf);

    try std.testing.expectEqual(@as(usize, 5), order.len);
    try std.testing.expectEqual(root, order[0]);
    // Children order depends on internal structure (left, right are used above)
}
