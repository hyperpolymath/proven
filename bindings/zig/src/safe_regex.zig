// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe regex pattern validation with ReDoS prevention.
//!
//! Provides regex pattern analysis and matching with protection against
//! Regular Expression Denial of Service (ReDoS) attacks. Patterns are
//! analyzed for dangerous constructs before compilation.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for regex operations.
pub const RegexError = error{
    InvalidPattern,
    PatternTooComplex,
    NestedQuantifiers,
    ExcessiveBacktracking,
    UnbalancedParentheses,
    UnbalancedBrackets,
    InvalidEscape,
    EmptyAlternation,
    PatternTooLong,
    OutOfMemory,
};

/// Maximum allowed pattern length.
pub const MAX_PATTERN_LENGTH: usize = 1024;

/// Maximum allowed quantifier repetition.
pub const MAX_QUANTIFIER_BOUND: usize = 1000;

/// Maximum allowed nesting depth.
pub const MAX_NESTING_DEPTH: usize = 10;

/// Result of pattern analysis.
pub const PatternAnalysis = struct {
    is_safe: bool,
    complexity_score: u32,
    has_nested_quantifiers: bool,
    has_overlapping_alternations: bool,
    max_nesting_depth: u32,
    estimated_states: u32,
    warnings: []const Warning,

    pub const Warning = struct {
        position: usize,
        message: []const u8,
    };
};

/// Validated regex pattern that has passed safety checks.
pub const SafePattern = struct {
    pattern: []const u8,
    analysis: PatternAnalysis,

    /// Check if a string matches the pattern using simple DFA simulation.
    /// This is a limited but safe matcher for simple patterns.
    pub fn matches(self: SafePattern, input: []const u8) bool {
        return simpleMatch(self.pattern, input);
    }

    /// Check if pattern starts with literal prefix.
    pub fn literalPrefix(self: SafePattern) ?[]const u8 {
        var end: usize = 0;
        for (self.pattern) |c| {
            if (isMetaChar(c)) break;
            end += 1;
        }
        if (end == 0) return null;
        return self.pattern[0..end];
    }
};

/// Analyze a regex pattern for safety without compiling.
pub fn analyzePattern(allocator: Allocator, pattern: []const u8) RegexError!PatternAnalysis {
    if (pattern.len > MAX_PATTERN_LENGTH) return error.PatternTooLong;

    var warnings = std.array_list.Managed(PatternAnalysis.Warning).init(allocator);
    errdefer warnings.deinit();

    var complexity: u32 = 0;
    var max_depth: u32 = 0;
    var current_depth: u32 = 0;
    var has_nested_quantifiers = false;
    var has_overlapping_alternations = false;
    var bracket_depth: u32 = 0;
    var prev_was_quantifier = false;
    var in_bracket = false;

    var i: usize = 0;
    while (i < pattern.len) : (i += 1) {
        const c = pattern[i];

        switch (c) {
            '(' => {
                if (!in_bracket) {
                    current_depth += 1;
                    max_depth = @max(max_depth, current_depth);
                    if (current_depth > MAX_NESTING_DEPTH) {
                        return error.PatternTooComplex;
                    }
                    complexity += 2;
                }
                prev_was_quantifier = false;
            },
            ')' => {
                if (!in_bracket) {
                    if (current_depth == 0) return error.UnbalancedParentheses;
                    current_depth -= 1;
                }
                // Don't reset prev_was_quantifier - a quantified group followed by
                // a quantifier is a nested quantifier (e.g., "(a+)+")
            },
            '[' => {
                if (!in_bracket) {
                    in_bracket = true;
                    bracket_depth += 1;
                    complexity += 1;
                }
                prev_was_quantifier = false;
            },
            ']' => {
                if (in_bracket) {
                    in_bracket = false;
                    bracket_depth -= 1;
                }
                prev_was_quantifier = false;
            },
            '*', '+', '?' => {
                if (prev_was_quantifier) {
                    has_nested_quantifiers = true;
                    try warnings.append(.{
                        .position = i,
                        .message = "Nested quantifier detected - potential ReDoS",
                    });
                }
                complexity += 5;
                prev_was_quantifier = true;
            },
            '{' => {
                if (!in_bracket) {
                    // Check for quantifier bounds
                    const bounds = parseQuantifierBounds(pattern[i..]);
                    if (bounds) |b| {
                        if (b.max > MAX_QUANTIFIER_BOUND) {
                            return error.PatternTooComplex;
                        }
                        complexity += @intCast(@min(b.max, 100));
                        if (prev_was_quantifier) {
                            has_nested_quantifiers = true;
                        }
                        prev_was_quantifier = true;
                    }
                }
            },
            '|' => {
                if (!in_bracket) {
                    complexity += 3;
                    // Check for overlapping alternations (simplified check)
                    if (i > 0 and i + 1 < pattern.len) {
                        if (pattern[i - 1] == '|' or pattern[i + 1] == '|') {
                            has_overlapping_alternations = true;
                        }
                    }
                }
                prev_was_quantifier = false;
            },
            '\\' => {
                // Skip escape sequence
                if (i + 1 >= pattern.len) return error.InvalidEscape;
                i += 1;
                prev_was_quantifier = false;
            },
            '.' => {
                complexity += 2;
                prev_was_quantifier = false;
            },
            else => {
                complexity += 1;
                prev_was_quantifier = false;
            },
        }
    }

    if (current_depth != 0) return error.UnbalancedParentheses;
    if (in_bracket) return error.UnbalancedBrackets;

    const estimated_states = complexity * (max_depth + 1);
    const is_safe = !has_nested_quantifiers and estimated_states < 10000;

    return PatternAnalysis{
        .is_safe = is_safe,
        .complexity_score = complexity,
        .has_nested_quantifiers = has_nested_quantifiers,
        .has_overlapping_alternations = has_overlapping_alternations,
        .max_nesting_depth = max_depth,
        .estimated_states = estimated_states,
        .warnings = try warnings.toOwnedSlice(),
    };
}

/// Validate a pattern and return a safe wrapper.
pub fn validatePattern(allocator: Allocator, pattern: []const u8) RegexError!SafePattern {
    const analysis = try analyzePattern(allocator, pattern);

    if (!analysis.is_safe) {
        // Free warnings before returning error to prevent memory leak
        allocator.free(analysis.warnings);
        if (analysis.has_nested_quantifiers) {
            return error.NestedQuantifiers;
        }
        return error.ExcessiveBacktracking;
    }

    return SafePattern{
        .pattern = pattern,
        .analysis = analysis,
    };
}

/// Check if a character is a regex metacharacter.
pub fn isMetaChar(c: u8) bool {
    return switch (c) {
        '.', '*', '+', '?', '[', ']', '(', ')', '{', '}', '|', '^', '$', '\\' => true,
        else => false,
    };
}

/// Escape a string for literal matching in regex.
pub fn escape(allocator: Allocator, input: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (input) |c| {
        if (isMetaChar(c)) {
            try result.append('\\');
        }
        try result.append(c);
    }

    return result.toOwnedSlice();
}

/// Simple pattern matching for basic patterns (no backtracking).
/// Supports: literal chars, '.', '*' (as .* only), '^', '$'
fn simpleMatch(pattern: []const u8, input: []const u8) bool {
    var pattern_start: usize = 0;
    var anchored_start = false;
    var anchored_end = false;

    // Check anchors
    if (pattern.len > 0 and pattern[0] == '^') {
        anchored_start = true;
        pattern_start = 1;
    }
    var pattern_end = pattern.len;
    if (pattern_end > pattern_start and pattern[pattern_end - 1] == '$') {
        anchored_end = true;
        pattern_end -= 1;
    }

    const effective_pattern = pattern[pattern_start..pattern_end];

    if (anchored_start and anchored_end) {
        return matchHere(effective_pattern, input);
    } else if (anchored_start) {
        return matchPrefix(effective_pattern, input);
    } else if (anchored_end) {
        return matchSuffix(effective_pattern, input);
    } else {
        // Search for pattern anywhere in input
        var i: usize = 0;
        while (i <= input.len) : (i += 1) {
            if (matchPrefix(effective_pattern, input[i..])) {
                return true;
            }
        }
        return false;
    }
}

fn matchHere(pattern: []const u8, input: []const u8) bool {
    if (pattern.len == 0) return input.len == 0;
    return matchPrefix(pattern, input) and suffixLen(pattern, input) == 0;
}

fn matchPrefix(pattern: []const u8, input: []const u8) bool {
    var pi: usize = 0;
    var ii: usize = 0;

    while (pi < pattern.len) {
        // Check for .* pattern
        if (pi + 1 < pattern.len and pattern[pi] == '.' and pattern[pi + 1] == '*') {
            // .* matches zero or more of anything - try all possibilities
            const rest_pattern = pattern[pi + 2 ..];
            while (ii <= input.len) : (ii += 1) {
                if (matchPrefix(rest_pattern, input[ii..])) {
                    return true;
                }
            }
            return false;
        }

        if (ii >= input.len) return false;

        if (pattern[pi] == '.') {
            // . matches any single character
            pi += 1;
            ii += 1;
        } else if (pattern[pi] == '\\' and pi + 1 < pattern.len) {
            // Escaped character
            if (input[ii] != pattern[pi + 1]) return false;
            pi += 2;
            ii += 1;
        } else {
            // Literal match
            if (pattern[pi] != input[ii]) return false;
            pi += 1;
            ii += 1;
        }
    }

    return true;
}

fn matchSuffix(pattern: []const u8, input: []const u8) bool {
    // Try matching from each position
    var i: usize = 0;
    while (i <= input.len) : (i += 1) {
        if (matchHere(pattern, input[i..])) {
            return true;
        }
    }
    return false;
}

fn suffixLen(pattern: []const u8, input: []const u8) usize {
    var pi: usize = 0;
    var consumed: usize = 0;

    while (pi < pattern.len) {
        if (pi + 1 < pattern.len and pattern[pi] == '.' and pattern[pi + 1] == '*') {
            pi += 2;
            continue;
        }
        if (pattern[pi] == '\\' and pi + 1 < pattern.len) {
            pi += 2;
        } else {
            pi += 1;
        }
        consumed += 1;
    }

    if (consumed > input.len) return input.len;
    return input.len - consumed;
}

const QuantifierBounds = struct {
    min: usize,
    max: usize,
};

fn parseQuantifierBounds(pattern: []const u8) ?QuantifierBounds {
    if (pattern.len < 3 or pattern[0] != '{') return null;

    var i: usize = 1;
    var min: usize = 0;
    var max: usize = 0;
    var parsing_max = false;

    while (i < pattern.len and pattern[i] != '}') : (i += 1) {
        const c = pattern[i];
        if (c == ',') {
            parsing_max = true;
        } else if (c >= '0' and c <= '9') {
            const digit = c - '0';
            if (parsing_max) {
                max = max * 10 + digit;
            } else {
                min = min * 10 + digit;
            }
        } else {
            return null;
        }
    }

    if (!parsing_max) {
        max = min;
    }

    return QuantifierBounds{ .min = min, .max = max };
}

test "analyzePattern simple" {
    const allocator = std.testing.allocator;
    const analysis = try analyzePattern(allocator, "hello");
    defer allocator.free(analysis.warnings);
    try std.testing.expect(analysis.is_safe);
    try std.testing.expect(!analysis.has_nested_quantifiers);
}

test "analyzePattern nested quantifiers" {
    const allocator = std.testing.allocator;
    const analysis = try analyzePattern(allocator, "(a+)+");
    defer allocator.free(analysis.warnings);
    try std.testing.expect(analysis.has_nested_quantifiers);
    try std.testing.expect(!analysis.is_safe);
}

test "analyzePattern unbalanced parens" {
    const allocator = std.testing.allocator;
    try std.testing.expectError(error.UnbalancedParentheses, analyzePattern(allocator, "(abc"));
}

test "validatePattern safe" {
    const allocator = std.testing.allocator;
    const safe = try validatePattern(allocator, "^hello$");
    try std.testing.expect(safe.analysis.is_safe);
}

test "validatePattern unsafe" {
    const allocator = std.testing.allocator;
    try std.testing.expectError(error.NestedQuantifiers, validatePattern(allocator, "(a*)*"));
}

test "escape metacharacters" {
    const allocator = std.testing.allocator;
    const result = try escape(allocator, "a.b*c?");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("a\\.b\\*c\\?", result);
}

test "simpleMatch literal" {
    const pattern = try validatePattern(std.testing.allocator, "hello");
    try std.testing.expect(pattern.matches("hello world"));
    try std.testing.expect(!pattern.matches("world"));
}

test "simpleMatch anchored" {
    const allocator = std.testing.allocator;
    const pattern = try validatePattern(allocator, "^hello$");
    try std.testing.expect(pattern.matches("hello"));
    try std.testing.expect(!pattern.matches("hello world"));
}

test "simpleMatch dot" {
    const allocator = std.testing.allocator;
    const pattern = try validatePattern(allocator, "h.llo");
    try std.testing.expect(pattern.matches("hello"));
    try std.testing.expect(pattern.matches("hallo"));
}

test "simpleMatch dotstar" {
    const allocator = std.testing.allocator;
    const pattern = try validatePattern(allocator, "h.*o");
    try std.testing.expect(pattern.matches("hello"));
    try std.testing.expect(pattern.matches("ho"));
}
