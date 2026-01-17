// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe policy expression evaluation that cannot crash.
//!
//! Provides a type-safe policy engine for evaluating access control rules,
//! feature flags, and conditional logic with well-defined behavior.

const std = @import("std");

pub const PolicyError = error{
    InvalidPolicy,
    EvaluationFailed,
    MaxDepthExceeded,
    UnknownAttribute,
    TypeMismatch,
};

/// Policy evaluation result.
pub const PolicyResult = enum {
    allow,
    deny,
    not_applicable,

    /// Combine two results with AND semantics (deny wins).
    pub fn and_(self: PolicyResult, other: PolicyResult) PolicyResult {
        if (self == .deny or other == .deny) return .deny;
        if (self == .not_applicable or other == .not_applicable) return .not_applicable;
        return .allow;
    }

    /// Combine two results with OR semantics (allow wins).
    pub fn or_(self: PolicyResult, other: PolicyResult) PolicyResult {
        if (self == .allow or other == .allow) return .allow;
        if (self == .deny and other == .deny) return .deny;
        return .not_applicable;
    }

    /// Negate the result.
    pub fn not(self: PolicyResult) PolicyResult {
        return switch (self) {
            .allow => .deny,
            .deny => .allow,
            .not_applicable => .not_applicable,
        };
    }

    /// Convert to boolean (allow = true, deny/not_applicable = false).
    pub fn toBool(self: PolicyResult) bool {
        return self == .allow;
    }
};

/// Comparison operators for policy conditions.
pub const CompareOp = enum {
    equal,
    not_equal,
    less_than,
    less_equal,
    greater_than,
    greater_equal,
    contains,
    starts_with,
    ends_with,
    matches_any,
};

/// Value types for policy attributes.
pub const Value = union(enum) {
    boolean: bool,
    integer: i64,
    float: f64,
    string: []const u8,
    string_list: []const []const u8,

    /// Compare two values.
    pub fn compare(self: Value, other: Value, op: CompareOp) ?bool {
        switch (self) {
            .boolean => |a| {
                if (other != .boolean) return null;
                const b = other.boolean;
                return switch (op) {
                    .equal => a == b,
                    .not_equal => a != b,
                    else => null,
                };
            },
            .integer => |a| {
                if (other != .integer) return null;
                const b = other.integer;
                return switch (op) {
                    .equal => a == b,
                    .not_equal => a != b,
                    .less_than => a < b,
                    .less_equal => a <= b,
                    .greater_than => a > b,
                    .greater_equal => a >= b,
                    else => null,
                };
            },
            .float => |a| {
                if (other != .float) return null;
                const b = other.float;
                return switch (op) {
                    .equal => a == b,
                    .not_equal => a != b,
                    .less_than => a < b,
                    .less_equal => a <= b,
                    .greater_than => a > b,
                    .greater_equal => a >= b,
                    else => null,
                };
            },
            .string => |a| {
                if (other == .string) {
                    const b = other.string;
                    return switch (op) {
                        .equal => std.mem.eql(u8, a, b),
                        .not_equal => !std.mem.eql(u8, a, b),
                        .contains => std.mem.indexOf(u8, a, b) != null,
                        .starts_with => std.mem.startsWith(u8, a, b),
                        .ends_with => std.mem.endsWith(u8, a, b),
                        else => null,
                    };
                } else if (other == .string_list) {
                    const list = other.string_list;
                    return switch (op) {
                        .matches_any => blk: {
                            for (list) |item| {
                                if (std.mem.eql(u8, a, item)) break :blk true;
                            }
                            break :blk false;
                        },
                        else => null,
                    };
                }
                return null;
            },
            .string_list => return null,
        }
    }
};

/// A single policy condition.
pub const Condition = struct {
    attribute: []const u8,
    operator: CompareOp,
    value: Value,
};

/// Policy rule combining effect with conditions.
pub const Rule = struct {
    effect: PolicyResult,
    conditions: []const Condition,
    description: ?[]const u8 = null,

    /// Evaluate rule against attributes.
    pub fn evaluate(self: Rule, attributes: AttributeMap) PolicyResult {
        for (self.conditions) |condition| {
            const attr_value = attributes.get(condition.attribute) orelse return .not_applicable;
            const result = attr_value.compare(condition.value, condition.operator) orelse return .not_applicable;
            if (!result) return .not_applicable;
        }
        return self.effect;
    }
};

/// Combining algorithm for multiple rules.
pub const CombineAlgorithm = enum {
    /// First applicable rule wins.
    first_applicable,
    /// Deny overrides allow.
    deny_overrides,
    /// Allow overrides deny.
    allow_overrides,
    /// All rules must allow.
    unanimous_allow,
    /// All rules must deny.
    unanimous_deny,
};

/// Attribute map for policy evaluation.
pub const AttributeMap = struct {
    const Entry = struct {
        key: []const u8,
        value: Value,
    };

    entries: [32]?Entry = [_]?Entry{null} ** 32,
    count: usize = 0,

    /// Set an attribute value.
    pub fn set(self: *AttributeMap, key: []const u8, value: Value) void {
        // Check if key exists and update
        for (&self.entries) |*entry_opt| {
            if (entry_opt.*) |*entry| {
                if (std.mem.eql(u8, entry.key, key)) {
                    entry.value = value;
                    return;
                }
            }
        }

        // Add new entry
        if (self.count < 32) {
            self.entries[self.count] = .{ .key = key, .value = value };
            self.count += 1;
        }
    }

    /// Get an attribute value.
    pub fn get(self: *const AttributeMap, key: []const u8) ?Value {
        for (self.entries[0..self.count]) |entry_opt| {
            if (entry_opt) |entry| {
                if (std.mem.eql(u8, entry.key, key)) {
                    return entry.value;
                }
            }
        }
        return null;
    }

    /// Check if attribute exists.
    pub fn has(self: *const AttributeMap, key: []const u8) bool {
        return self.get(key) != null;
    }
};

/// Policy set containing multiple rules.
pub fn PolicySet(comptime max_rules: usize) type {
    return struct {
        const Self = @This();

        rules: [max_rules]?Rule = [_]?Rule{null} ** max_rules,
        rule_count: usize = 0,
        algorithm: CombineAlgorithm = .deny_overrides,
        default_result: PolicyResult = .deny,

        pub fn init(algorithm: CombineAlgorithm) Self {
            return .{ .algorithm = algorithm };
        }

        /// Add a rule to the policy set.
        pub fn addRule(self: *Self, rule: Rule) PolicyError!void {
            if (self.rule_count >= max_rules) return error.InvalidPolicy;
            self.rules[self.rule_count] = rule;
            self.rule_count += 1;
        }

        /// Evaluate the policy set against attributes.
        pub fn evaluate(self: *const Self, attributes: AttributeMap) PolicyResult {
            if (self.rule_count == 0) return self.default_result;

            var allow_count: usize = 0;
            var deny_count: usize = 0;
            var applicable_count: usize = 0;
            var first_result: ?PolicyResult = null;

            for (self.rules[0..self.rule_count]) |rule_opt| {
                if (rule_opt) |rule| {
                    const result = rule.evaluate(attributes);
                    if (result != .not_applicable) {
                        applicable_count += 1;
                        if (first_result == null) first_result = result;
                        if (result == .allow) allow_count += 1;
                        if (result == .deny) deny_count += 1;
                    }
                }
            }

            if (applicable_count == 0) return self.default_result;

            return switch (self.algorithm) {
                .first_applicable => first_result orelse self.default_result,
                .deny_overrides => if (deny_count > 0) .deny else .allow,
                .allow_overrides => if (allow_count > 0) .allow else .deny,
                .unanimous_allow => if (allow_count == applicable_count) .allow else .deny,
                .unanimous_deny => if (deny_count == applicable_count) .deny else .allow,
            };
        }

        /// Check if an action is allowed.
        pub fn isAllowed(self: *const Self, attributes: AttributeMap) bool {
            return self.evaluate(attributes) == .allow;
        }
    };
}

/// Simple feature flag.
pub const FeatureFlag = struct {
    name: []const u8,
    enabled: bool,
    rollout_percentage: u8 = 100,

    /// Check if feature is enabled for a user ID.
    pub fn isEnabledFor(self: FeatureFlag, user_id: u64) bool {
        if (!self.enabled) return false;
        if (self.rollout_percentage >= 100) return true;
        if (self.rollout_percentage == 0) return false;

        // Consistent hashing based on user_id
        const hash = user_id % 100;
        return hash < self.rollout_percentage;
    }
};

/// Time-based policy (e.g., business hours).
pub const TimePolicy = struct {
    start_hour: u8,
    end_hour: u8,
    days: u7 = 0b1111111, // Bitmask: Sun=1, Mon=2, Tue=4, Wed=8, Thu=16, Fri=32, Sat=64

    /// Check if current time (hour 0-23, day 0-6 Sunday=0) is within policy.
    pub fn isActiveAt(self: TimePolicy, hour: u8, day_of_week: u3) bool {
        if (hour > 23) return false;
        if (day_of_week > 6) return false;

        const day_mask: u7 = @as(u7, 1) << day_of_week;
        if (self.days & day_mask == 0) return false;

        if (self.start_hour <= self.end_hour) {
            return hour >= self.start_hour and hour < self.end_hour;
        } else {
            // Wraps around midnight
            return hour >= self.start_hour or hour < self.end_hour;
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "PolicyResult and" {
    try std.testing.expectEqual(PolicyResult.deny, PolicyResult.allow.and_(.deny));
    try std.testing.expectEqual(PolicyResult.allow, PolicyResult.allow.and_(.allow));
    try std.testing.expectEqual(PolicyResult.not_applicable, PolicyResult.allow.and_(.not_applicable));
}

test "PolicyResult or" {
    try std.testing.expectEqual(PolicyResult.allow, PolicyResult.allow.or_(.deny));
    try std.testing.expectEqual(PolicyResult.deny, PolicyResult.deny.or_(.deny));
    try std.testing.expectEqual(PolicyResult.allow, PolicyResult.not_applicable.or_(.allow));
}

test "Value compare" {
    const int_a = Value{ .integer = 10 };
    const int_b = Value{ .integer = 20 };
    try std.testing.expectEqual(true, int_a.compare(int_b, .less_than).?);
    try std.testing.expectEqual(false, int_a.compare(int_b, .equal).?);

    const str_a = Value{ .string = "hello" };
    const str_b = Value{ .string = "hello" };
    try std.testing.expectEqual(true, str_a.compare(str_b, .equal).?);
}

test "AttributeMap" {
    var attrs = AttributeMap{};
    attrs.set("role", .{ .string = "admin" });
    attrs.set("level", .{ .integer = 5 });

    try std.testing.expectEqualStrings("admin", attrs.get("role").?.string);
    try std.testing.expectEqual(@as(i64, 5), attrs.get("level").?.integer);
    try std.testing.expect(attrs.get("unknown") == null);
}

test "Rule evaluate" {
    const rule = Rule{
        .effect = .allow,
        .conditions = &[_]Condition{
            .{
                .attribute = "role",
                .operator = .equal,
                .value = .{ .string = "admin" },
            },
        },
    };

    var attrs = AttributeMap{};
    attrs.set("role", .{ .string = "admin" });
    try std.testing.expectEqual(PolicyResult.allow, rule.evaluate(attrs));

    attrs.set("role", .{ .string = "user" });
    try std.testing.expectEqual(PolicyResult.not_applicable, rule.evaluate(attrs));
}

test "PolicySet deny_overrides" {
    var policy = PolicySet(10).init(.deny_overrides);
    try policy.addRule(.{
        .effect = .allow,
        .conditions = &[_]Condition{
            .{ .attribute = "authenticated", .operator = .equal, .value = .{ .boolean = true } },
        },
    });
    try policy.addRule(.{
        .effect = .deny,
        .conditions = &[_]Condition{
            .{ .attribute = "banned", .operator = .equal, .value = .{ .boolean = true } },
        },
    });

    var attrs = AttributeMap{};
    attrs.set("authenticated", .{ .boolean = true });
    attrs.set("banned", .{ .boolean = false });
    try std.testing.expectEqual(PolicyResult.allow, policy.evaluate(attrs));

    attrs.set("banned", .{ .boolean = true });
    try std.testing.expectEqual(PolicyResult.deny, policy.evaluate(attrs));
}

test "FeatureFlag rollout" {
    const flag = FeatureFlag{ .name = "new_feature", .enabled = true, .rollout_percentage = 50 };

    // User IDs 0-49 should be enabled, 50-99 should be disabled
    try std.testing.expect(flag.isEnabledFor(25));
    try std.testing.expect(!flag.isEnabledFor(75));
}

test "TimePolicy business hours" {
    const policy = TimePolicy{
        .start_hour = 9,
        .end_hour = 17,
        .days = 0b0111110, // Mon-Fri (bits 1-5)
    };

    // Monday (1) at 10:00 - should be active
    try std.testing.expect(policy.isActiveAt(10, 1));
    // Monday (1) at 20:00 - should not be active
    try std.testing.expect(!policy.isActiveAt(20, 1));
    // Sunday (0) at 10:00 - should not be active (weekend)
    try std.testing.expect(!policy.isActiveAt(10, 0));
}
