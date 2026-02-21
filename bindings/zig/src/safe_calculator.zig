// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe expression parser and calculator that cannot crash.

const std = @import("std");

/// Error types for calculator operations.
pub const CalcError = error{
    InvalidExpression,
    DivisionByZero,
    ModuloByZero,
    NegativeSqrt,
    LogOfNonPositive,
    UndefinedVariable,
    ParenthesisMismatch,
    UnknownFunction,
    OutOfMemory,
    Overflow,
};

/// Binary operators
pub const BinOp = enum {
    add,
    sub,
    mul,
    div,
    mod,
    pow,
};

/// Unary operators/functions
pub const UnaryOp = enum {
    neg,
    abs,
    sqrt,
    sin,
    cos,
    tan,
    ln,
    log10,
    exp,
    floor,
    ceil,
    round,
};

/// Expression node types
pub const ExprType = enum {
    number,
    variable,
    binary,
    unary,
};

/// Expression AST node
pub const Expr = union(ExprType) {
    number: f64,
    variable: []const u8,
    binary: struct {
        op: BinOp,
        left: *const Expr,
        right: *const Expr,
    },
    unary: struct {
        op: UnaryOp,
        operand: *const Expr,
    },
};

/// Variable environment
pub const Environment = struct {
    variables: std.StringHashMap(f64),

    pub fn init(allocator: std.mem.Allocator) Environment {
        var env = Environment{
            .variables = std.StringHashMap(f64).init(allocator),
        };
        // Add constants
        env.variables.put("pi", std.math.pi) catch {};
        env.variables.put("e", std.math.e) catch {};
        env.variables.put("tau", 2 * std.math.pi) catch {};
        env.variables.put("phi", 1.618033988749895) catch {}; // Golden ratio
        return env;
    }

    pub fn deinit(self: *Environment) void {
        self.variables.deinit();
    }

    pub fn set(self: *Environment, name: []const u8, value: f64) !void {
        try self.variables.put(name, value);
    }

    pub fn get(self: Environment, name: []const u8) ?f64 {
        return self.variables.get(name);
    }
};

/// Evaluate a binary operation
fn evalBinary(op: BinOp, left: f64, right: f64) CalcError!f64 {
    return switch (op) {
        .add => left + right,
        .sub => left - right,
        .mul => left * right,
        .div => if (right == 0) error.DivisionByZero else left / right,
        .mod => if (right == 0) error.ModuloByZero else @mod(left, right),
        .pow => std.math.pow(f64, left, right),
    };
}

/// Evaluate a unary operation
fn evalUnary(op: UnaryOp, x: f64) CalcError!f64 {
    return switch (op) {
        .neg => -x,
        .abs => @abs(x),
        .sqrt => if (x < 0) error.NegativeSqrt else @sqrt(x),
        .sin => @sin(x),
        .cos => @cos(x),
        .tan => @tan(x),
        .ln => if (x <= 0) error.LogOfNonPositive else @log(x),
        .log10 => if (x <= 0) error.LogOfNonPositive else @log10(x),
        .exp => @exp(x),
        .floor => @floor(x),
        .ceil => @ceil(x),
        .round => @round(x),
    };
}

/// Evaluate an expression with the given environment
pub fn evaluate(expr: *const Expr, env: *const Environment) CalcError!f64 {
    return switch (expr.*) {
        .number => |n| n,
        .variable => |name| env.get(name) orelse error.UndefinedVariable,
        .binary => |b| {
            const left = try evaluate(b.left, env);
            const right = try evaluate(b.right, env);
            return evalBinary(b.op, left, right);
        },
        .unary => |u| {
            const operand = try evaluate(u.operand, env);
            return evalUnary(u.op, operand);
        },
    };
}

/// Token types
const TokenType = enum {
    number,
    identifier,
    plus,
    minus,
    star,
    slash,
    percent,
    caret,
    lparen,
    rparen,
    comma,
    eof,
};

/// Token
const Token = struct {
    type_: TokenType,
    text: []const u8,
    number: f64 = 0,
};

/// Tokenizer
const Tokenizer = struct {
    input: []const u8,
    pos: usize = 0,

    fn next(self: *Tokenizer) Token {
        // Skip whitespace
        while (self.pos < self.input.len and (self.input[self.pos] == ' ' or self.input[self.pos] == '\t')) {
            self.pos += 1;
        }

        if (self.pos >= self.input.len) {
            return Token{ .type_ = .eof, .text = "" };
        }

        const c = self.input[self.pos];

        // Single character tokens
        const single_char_type: ?TokenType = switch (c) {
            '+' => .plus,
            '-' => .minus,
            '*' => .star,
            '/' => .slash,
            '%' => .percent,
            '^' => .caret,
            '(' => .lparen,
            ')' => .rparen,
            ',' => .comma,
            else => null,
        };

        if (single_char_type) |t| {
            self.pos += 1;
            return Token{ .type_ = t, .text = self.input[self.pos - 1 .. self.pos] };
        }

        // Number
        if (std.ascii.isDigit(c) or c == '.') {
            const start = self.pos;
            while (self.pos < self.input.len and (std.ascii.isDigit(self.input[self.pos]) or self.input[self.pos] == '.')) {
                self.pos += 1;
            }
            const text = self.input[start..self.pos];
            const num = std.fmt.parseFloat(f64, text) catch 0;
            return Token{ .type_ = .number, .text = text, .number = num };
        }

        // Identifier
        if (std.ascii.isAlphabetic(c) or c == '_') {
            const start = self.pos;
            while (self.pos < self.input.len and (std.ascii.isAlphanumeric(self.input[self.pos]) or self.input[self.pos] == '_')) {
                self.pos += 1;
            }
            return Token{ .type_ = .identifier, .text = self.input[start..self.pos] };
        }

        // Unknown character, skip
        self.pos += 1;
        return self.next();
    }
};

/// Simple expression calculator (no AST allocation needed)
/// Evaluates expressions directly using recursive descent
pub fn calculate(input: []const u8) CalcError!f64 {
    var env = Environment.init(std.heap.page_allocator);
    defer env.deinit();

    return calculateWith(input, &env);
}

/// Calculate with custom environment
pub fn calculateWith(input: []const u8, env: *Environment) CalcError!f64 {
    var tokenizer = Tokenizer{ .input = input };
    var current = tokenizer.next();

    return parseAddSub(&tokenizer, &current, env);
}

fn parseAddSub(tokenizer: *Tokenizer, current: *Token, env: *Environment) CalcError!f64 {
    var result = try parseMulDiv(tokenizer, current, env);

    while (current.type_ == .plus or current.type_ == .minus) {
        const op = current.type_;
        current.* = tokenizer.next();
        const right = try parseMulDiv(tokenizer, current, env);

        if (op == .plus) {
            result += right;
        } else {
            result -= right;
        }
    }

    return result;
}

fn parseMulDiv(tokenizer: *Tokenizer, current: *Token, env: *Environment) CalcError!f64 {
    var result = try parsePower(tokenizer, current, env);

    while (current.type_ == .star or current.type_ == .slash or current.type_ == .percent) {
        const op = current.type_;
        current.* = tokenizer.next();
        const right = try parsePower(tokenizer, current, env);

        if (op == .star) {
            result *= right;
        } else if (op == .slash) {
            if (right == 0) return error.DivisionByZero;
            result /= right;
        } else {
            if (right == 0) return error.ModuloByZero;
            result = @mod(result, right);
        }
    }

    return result;
}

fn parsePower(tokenizer: *Tokenizer, current: *Token, env: *Environment) CalcError!f64 {
    const base = try parseUnary(tokenizer, current, env);

    if (current.type_ == .caret) {
        current.* = tokenizer.next();
        const exp = try parsePower(tokenizer, current, env); // Right associative
        return std.math.pow(f64, base, exp);
    }

    return base;
}

fn parseUnary(tokenizer: *Tokenizer, current: *Token, env: *Environment) CalcError!f64 {
    if (current.type_ == .minus) {
        current.* = tokenizer.next();
        return -(try parseUnary(tokenizer, current, env));
    }
    if (current.type_ == .plus) {
        current.* = tokenizer.next();
        return try parseUnary(tokenizer, current, env);
    }

    return try parsePrimary(tokenizer, current, env);
}

fn parsePrimary(tokenizer: *Tokenizer, current: *Token, env: *Environment) CalcError!f64 {
    if (current.type_ == .number) {
        const val = current.number;
        current.* = tokenizer.next();
        return val;
    }

    if (current.type_ == .identifier) {
        const name = current.text;
        current.* = tokenizer.next();

        // Check for function call
        if (current.type_ == .lparen) {
            current.* = tokenizer.next();
            const arg = try parseAddSub(tokenizer, current, env);

            if (current.type_ != .rparen) return error.ParenthesisMismatch;
            current.* = tokenizer.next();

            // Evaluate function
            return evalFunction(name, arg);
        }

        // Variable lookup
        return env.get(name) orelse error.UndefinedVariable;
    }

    if (current.type_ == .lparen) {
        current.* = tokenizer.next();
        const result = try parseAddSub(tokenizer, current, env);

        if (current.type_ != .rparen) return error.ParenthesisMismatch;
        current.* = tokenizer.next();

        return result;
    }

    return error.InvalidExpression;
}

fn evalFunction(name: []const u8, arg: f64) CalcError!f64 {
    if (std.mem.eql(u8, name, "sin")) return @sin(arg);
    if (std.mem.eql(u8, name, "cos")) return @cos(arg);
    if (std.mem.eql(u8, name, "tan")) return @tan(arg);
    if (std.mem.eql(u8, name, "sqrt")) {
        if (arg < 0) return error.NegativeSqrt;
        return @sqrt(arg);
    }
    if (std.mem.eql(u8, name, "abs")) return @abs(arg);
    if (std.mem.eql(u8, name, "ln") or std.mem.eql(u8, name, "log")) {
        if (arg <= 0) return error.LogOfNonPositive;
        return @log(arg);
    }
    if (std.mem.eql(u8, name, "log10")) {
        if (arg <= 0) return error.LogOfNonPositive;
        return @log10(arg);
    }
    if (std.mem.eql(u8, name, "exp")) return @exp(arg);
    if (std.mem.eql(u8, name, "floor")) return @floor(arg);
    if (std.mem.eql(u8, name, "ceil")) return @ceil(arg);
    if (std.mem.eql(u8, name, "round")) return @round(arg);

    return error.UnknownFunction;
}

test "basic arithmetic" {
    try std.testing.expectApproxEqAbs(@as(f64, 5), try calculate("2 + 3"), 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 6), try calculate("2 * 3"), 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 7), try calculate("1 + 2 * 3"), 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 9), try calculate("(1 + 2) * 3"), 0.001);
}

test "power" {
    try std.testing.expectApproxEqAbs(@as(f64, 8), try calculate("2^3"), 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 512), try calculate("2^3^2"), 0.001); // Right associative: 2^9
}

test "unary" {
    try std.testing.expectApproxEqAbs(@as(f64, -5), try calculate("-5"), 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 5), try calculate("--5"), 0.001);
}

test "functions" {
    try std.testing.expectApproxEqAbs(@as(f64, 0), try calculate("sin(0)"), 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 1), try calculate("cos(0)"), 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 2), try calculate("sqrt(4)"), 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 5), try calculate("abs(-5)"), 0.001);
}

test "constants" {
    const pi = try calculate("pi");
    try std.testing.expect(pi > 3.14 and pi < 3.15);

    const e = try calculate("e");
    try std.testing.expect(e > 2.71 and e < 2.72);
}

test "division by zero" {
    try std.testing.expectError(error.DivisionByZero, calculate("1/0"));
}

test "undefined variable" {
    try std.testing.expectError(error.UndefinedVariable, calculate("x + 1"));
}
