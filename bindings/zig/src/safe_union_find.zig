// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe Union-Find (disjoint set) data structure that cannot crash.
//!
//! Provides efficient operations for tracking disjoint sets with path compression
//! and union by rank optimizations. All operations are bounded and cannot cause
//! out-of-bounds access or infinite loops.

const std = @import("std");

/// Error types for Union-Find operations.
pub const UnionFindError = error{
    /// Element index is out of bounds.
    ElementOutOfBounds,
    /// Maximum capacity has been reached.
    MaxCapacityReached,
    /// Invalid operation attempted.
    InvalidOperation,
};

/// Union-Find data structure with fixed capacity.
/// Uses path compression and union by rank for near-constant time operations.
pub fn UnionFind(comptime max_elements: usize) type {
    return struct {
        const Self = @This();

        /// Parent array - parent[i] is the parent of element i.
        /// If parent[i] == i, then i is a root.
        parent: [max_elements]usize = undefined,

        /// Rank array for union by rank optimization.
        rank: [max_elements]usize = undefined,

        /// Number of elements currently in the structure.
        element_count: usize = 0,

        /// Number of disjoint sets.
        set_count: usize = 0,

        /// Initialize an empty Union-Find structure.
        pub fn init() Self {
            return .{};
        }

        /// Initialize with a specific number of elements.
        /// Each element starts in its own singleton set.
        pub fn initWithElements(num_elements: usize) UnionFindError!Self {
            if (num_elements > max_elements) return error.MaxCapacityReached;

            var self = Self{};
            var element_index: usize = 0;
            while (element_index < num_elements) : (element_index += 1) {
                self.parent[element_index] = element_index;
                self.rank[element_index] = 0;
            }
            self.element_count = num_elements;
            self.set_count = num_elements;
            return self;
        }

        /// Add a new element as a singleton set.
        /// Returns the index of the new element.
        pub fn makeSet(self: *Self) UnionFindError!usize {
            if (self.element_count >= max_elements) return error.MaxCapacityReached;

            const new_element_index = self.element_count;
            self.parent[new_element_index] = new_element_index;
            self.rank[new_element_index] = 0;
            self.element_count += 1;
            self.set_count += 1;
            return new_element_index;
        }

        /// Find the representative (root) of the set containing element x.
        /// Uses path compression for amortized near-constant time.
        pub fn find(self: *Self, element_index: usize) UnionFindError!usize {
            if (element_index >= self.element_count) return error.ElementOutOfBounds;

            // Find root with path compression
            var current_element = element_index;
            while (self.parent[current_element] != current_element) {
                // Path compression: make every node point to its grandparent
                self.parent[current_element] = self.parent[self.parent[current_element]];
                current_element = self.parent[current_element];
            }
            return current_element;
        }

        /// Find without modifying the structure (const version).
        pub fn findConst(self: *const Self, element_index: usize) UnionFindError!usize {
            if (element_index >= self.element_count) return error.ElementOutOfBounds;

            var current_element = element_index;
            while (self.parent[current_element] != current_element) {
                current_element = self.parent[current_element];
            }
            return current_element;
        }

        /// Union the sets containing elements x and y.
        /// Uses union by rank to keep trees balanced.
        /// Returns true if a merge occurred, false if already in same set.
        pub fn unite(self: *Self, element_x: usize, element_y: usize) UnionFindError!bool {
            const root_x = try self.find(element_x);
            const root_y = try self.find(element_y);

            if (root_x == root_y) return false;

            // Union by rank
            if (self.rank[root_x] < self.rank[root_y]) {
                self.parent[root_x] = root_y;
            } else if (self.rank[root_x] > self.rank[root_y]) {
                self.parent[root_y] = root_x;
            } else {
                self.parent[root_y] = root_x;
                self.rank[root_x] += 1;
            }

            self.set_count -= 1;
            return true;
        }

        /// Check if two elements are in the same set.
        pub fn connected(self: *Self, element_x: usize, element_y: usize) UnionFindError!bool {
            const root_x = try self.find(element_x);
            const root_y = try self.find(element_y);
            return root_x == root_y;
        }

        /// Check if two elements are in the same set (const version).
        pub fn connectedConst(self: *const Self, element_x: usize, element_y: usize) UnionFindError!bool {
            const root_x = try self.findConst(element_x);
            const root_y = try self.findConst(element_y);
            return root_x == root_y;
        }

        /// Get the number of elements in the set containing x.
        pub fn setSize(self: *Self, element_index: usize) UnionFindError!usize {
            const root = try self.find(element_index);
            var size_count: usize = 0;
            var scan_index: usize = 0;
            while (scan_index < self.element_count) : (scan_index += 1) {
                if ((self.findConst(scan_index) catch continue) == root) {
                    size_count += 1;
                }
            }
            return size_count;
        }

        /// Get all elements in the set containing x.
        /// Returns the number of elements written to the buffer.
        pub fn getSetMembers(self: *Self, element_index: usize, output_buffer: []usize) UnionFindError!usize {
            const root = try self.find(element_index);
            var write_index: usize = 0;
            var scan_index: usize = 0;
            while (scan_index < self.element_count and write_index < output_buffer.len) : (scan_index += 1) {
                if ((self.findConst(scan_index) catch continue) == root) {
                    output_buffer[write_index] = scan_index;
                    write_index += 1;
                }
            }
            return write_index;
        }

        /// Get all roots (set representatives).
        /// Returns the number of roots written to the buffer.
        pub fn getRoots(self: *const Self, output_buffer: []usize) usize {
            var write_index: usize = 0;
            var scan_index: usize = 0;
            while (scan_index < self.element_count and write_index < output_buffer.len) : (scan_index += 1) {
                if (self.parent[scan_index] == scan_index) {
                    output_buffer[write_index] = scan_index;
                    write_index += 1;
                }
            }
            return write_index;
        }

        /// Get the number of disjoint sets.
        pub fn countSets(self: *const Self) usize {
            return self.set_count;
        }

        /// Get the total number of elements.
        pub fn count(self: *const Self) usize {
            return self.element_count;
        }

        /// Check if the structure is empty.
        pub fn isEmpty(self: *const Self) bool {
            return self.element_count == 0;
        }

        /// Reset to empty state.
        pub fn clear(self: *Self) void {
            self.element_count = 0;
            self.set_count = 0;
        }
    };
}

/// Weighted Union-Find with associated values.
/// Tracks relative weights/distances between elements.
pub fn WeightedUnionFind(comptime max_elements: usize, comptime WeightType: type) type {
    return struct {
        const Self = @This();

        parent: [max_elements]usize = undefined,
        rank: [max_elements]usize = undefined,
        weight: [max_elements]WeightType = undefined,
        element_count: usize = 0,
        set_count: usize = 0,

        pub fn init() Self {
            return .{};
        }

        /// Add a new element with initial weight of zero.
        pub fn makeSet(self: *Self) UnionFindError!usize {
            if (self.element_count >= max_elements) return error.MaxCapacityReached;

            const new_element_index = self.element_count;
            self.parent[new_element_index] = new_element_index;
            self.rank[new_element_index] = 0;
            self.weight[new_element_index] = 0;
            self.element_count += 1;
            self.set_count += 1;
            return new_element_index;
        }

        /// Find root and return the weight from element to root.
        pub fn findWithWeight(self: *Self, element_index: usize) UnionFindError!struct { root: usize, weight: WeightType } {
            if (element_index >= self.element_count) return error.ElementOutOfBounds;

            if (self.parent[element_index] == element_index) {
                return .{ .root = element_index, .weight = 0 };
            }

            const parent_result = try self.findWithWeight(self.parent[element_index]);
            self.parent[element_index] = parent_result.root;
            self.weight[element_index] += parent_result.weight;

            return .{ .root = parent_result.root, .weight = self.weight[element_index] };
        }

        /// Union with weight: weight[y] - weight[x] = w after union.
        pub fn uniteWithWeight(self: *Self, element_x: usize, element_y: usize, weight_value: WeightType) UnionFindError!bool {
            const result_x = try self.findWithWeight(element_x);
            const result_y = try self.findWithWeight(element_y);

            if (result_x.root == result_y.root) return false;

            // weight[y] - weight[x] = w
            // weight[root_y] = weight[x] + w - weight[y]
            const new_weight = result_x.weight + weight_value - result_y.weight;

            if (self.rank[result_x.root] < self.rank[result_y.root]) {
                self.parent[result_x.root] = result_y.root;
                self.weight[result_x.root] = -new_weight;
            } else if (self.rank[result_x.root] > self.rank[result_y.root]) {
                self.parent[result_y.root] = result_x.root;
                self.weight[result_y.root] = new_weight;
            } else {
                self.parent[result_y.root] = result_x.root;
                self.weight[result_y.root] = new_weight;
                self.rank[result_x.root] += 1;
            }

            self.set_count -= 1;
            return true;
        }

        /// Get the weight difference between two elements (if in same set).
        pub fn getDifference(self: *Self, element_x: usize, element_y: usize) UnionFindError!?WeightType {
            const result_x = try self.findWithWeight(element_x);
            const result_y = try self.findWithWeight(element_y);

            if (result_x.root != result_y.root) return null;

            return result_y.weight - result_x.weight;
        }
    };
}

test "UnionFind basic operations" {
    var union_find_structure = try UnionFind(10).initWithElements(5);

    try std.testing.expectEqual(@as(usize, 5), union_find_structure.countSets());

    _ = try union_find_structure.unite(0, 1);
    try std.testing.expectEqual(@as(usize, 4), union_find_structure.countSets());

    _ = try union_find_structure.unite(2, 3);
    try std.testing.expectEqual(@as(usize, 3), union_find_structure.countSets());

    _ = try union_find_structure.unite(0, 2);
    try std.testing.expectEqual(@as(usize, 2), union_find_structure.countSets());

    try std.testing.expect(try union_find_structure.connected(0, 3));
    try std.testing.expect(try union_find_structure.connected(1, 2));
    try std.testing.expect(!try union_find_structure.connected(0, 4));
}

test "UnionFind makeSet" {
    var union_find_structure = UnionFind(10).init();

    const element_a = try union_find_structure.makeSet();
    const element_b = try union_find_structure.makeSet();
    const element_c = try union_find_structure.makeSet();

    try std.testing.expectEqual(@as(usize, 0), element_a);
    try std.testing.expectEqual(@as(usize, 1), element_b);
    try std.testing.expectEqual(@as(usize, 2), element_c);
    try std.testing.expectEqual(@as(usize, 3), union_find_structure.countSets());

    _ = try union_find_structure.unite(element_a, element_b);
    try std.testing.expectEqual(@as(usize, 2), union_find_structure.countSets());
}

test "UnionFind bounds checking" {
    var union_find_structure = try UnionFind(5).initWithElements(3);

    try std.testing.expectError(error.ElementOutOfBounds, union_find_structure.find(5));
    try std.testing.expectError(error.ElementOutOfBounds, union_find_structure.unite(0, 10));
}

test "UnionFind setSize and getSetMembers" {
    var union_find_structure = try UnionFind(10).initWithElements(5);

    _ = try union_find_structure.unite(0, 1);
    _ = try union_find_structure.unite(0, 2);

    try std.testing.expectEqual(@as(usize, 3), try union_find_structure.setSize(0));
    try std.testing.expectEqual(@as(usize, 1), try union_find_structure.setSize(4));

    var members_buffer: [10]usize = undefined;
    const member_count = try union_find_structure.getSetMembers(0, &members_buffer);
    try std.testing.expectEqual(@as(usize, 3), member_count);
}

test "WeightedUnionFind" {
    var weighted_union_find = WeightedUnionFind(10, i64).init();

    _ = try weighted_union_find.makeSet(); // 0
    _ = try weighted_union_find.makeSet(); // 1
    _ = try weighted_union_find.makeSet(); // 2

    // Set weight[1] - weight[0] = 5
    _ = try weighted_union_find.uniteWithWeight(0, 1, 5);

    // Set weight[2] - weight[0] = 10
    _ = try weighted_union_find.uniteWithWeight(0, 2, 10);

    // Check weight[2] - weight[1] = 5
    const difference = try weighted_union_find.getDifference(1, 2);
    try std.testing.expectEqual(@as(?i64, 5), difference);
}
