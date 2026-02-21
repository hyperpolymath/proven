// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Zig FFI bridge for Proven Malbolge operations.
//!
//! Implements the core Malbolge primitives with safety guarantees:
//! - Ternary arithmetic (base-3)
//! - The "crazy" operation
//! - Tritwise rotation
//! - Memory bounds checking
//!
//! These operations are called from Idris 2 proofs and exposed to
//! all language bindings via the unified ABI.

const std = @import("std");

/// A single trit (ternary digit): 0, 1, or 2
pub const Trit = enum(u2) {
    t0 = 0,
    t1 = 1,
    t2 = 2,
};

/// A 10-trit word (0 to 59048), the Malbolge word size
pub const Tryte = u17; // 17 bits can hold 0-59048

pub const TRYTE_MAX: Tryte = 59048; // 3^10 - 1
pub const MEMORY_SIZE: usize = 59049; // 3^10

/// The crazy operation lookup table.
/// crazy(a, b) is defined by this 3x3 matrix.
const crazy_table = [3][3]Trit{
    .{ .t1, .t0, .t0 }, // a=0
    .{ .t1, .t0, .t2 }, // a=1
    .{ .t2, .t2, .t1 }, // a=2
};

/// Perform the crazy operation on single trits.
/// This is formally verified to be total in Idris 2.
pub fn crazy_trit(a: Trit, b: Trit) Trit {
    return crazy_table[@intFromEnum(a)][@intFromEnum(b)];
}

/// Convert a Tryte to an array of 10 trits (LSB first).
pub fn tryte_to_trits(value: Tryte) [10]Trit {
    var trits: [10]Trit = undefined;
    var n = value;
    for (0..10) |i| {
        trits[i] = @enumFromInt(@as(u2, @intCast(n % 3)));
        n /= 3;
    }
    return trits;
}

/// Convert an array of 10 trits to a Tryte.
pub fn trits_to_tryte(trits: [10]Trit) Tryte {
    var value: Tryte = 0;
    var multiplier: Tryte = 1;
    for (trits) |t| {
        value += @intFromEnum(t) * multiplier;
        multiplier *= 3;
    }
    return value;
}

/// Perform the crazy operation on full Trytes.
/// This is the heart of Malbolge, now with safety guarantees.
pub fn crazy(a: Tryte, b: Tryte) Tryte {
    const a_trits = tryte_to_trits(a);
    const b_trits = tryte_to_trits(b);
    var result: [10]Trit = undefined;

    for (0..10) |i| {
        result[i] = crazy_trit(a_trits[i], b_trits[i]);
    }

    return trits_to_tryte(result);
}

/// Rotate a Tryte right by one trit position.
/// The most significant trit becomes the least significant.
pub fn rotate_right(value: Tryte) Tryte {
    const trits = tryte_to_trits(value);
    var rotated: [10]Trit = undefined;

    rotated[0] = trits[9]; // MST becomes LST
    for (1..10) |i| {
        rotated[i] = trits[i - 1];
    }

    return trits_to_tryte(rotated);
}

/// Safe addition with wraparound (Malbolge semantics).
pub fn add_wrap(a: Tryte, b: Tryte) Tryte {
    return @intCast((@as(u32, a) + @as(u32, b)) % (TRYTE_MAX + 1));
}

/// Safe subtraction with wraparound.
pub fn sub_wrap(a: Tryte, b: Tryte) Tryte {
    if (a >= b) {
        return a - b;
    } else {
        return @intCast(TRYTE_MAX + 1 - (b - a));
    }
}

// ============================================================================
// FFI EXPORTS - Called from Idris 2 and other language bindings
// ============================================================================

/// FFI: Perform crazy operation
export fn proven_malbolge_crazy(a: Tryte, b: Tryte) Tryte {
    return crazy(a, b);
}

/// FFI: Rotate right
export fn proven_malbolge_rotate(value: Tryte) Tryte {
    return rotate_right(value);
}

/// FFI: Safe addition
export fn proven_malbolge_add(a: Tryte, b: Tryte) Tryte {
    return add_wrap(a, b);
}

/// FFI: Convert to trits (returns packed representation)
export fn proven_malbolge_to_trits(value: Tryte, out: [*]u8) void {
    const trits = tryte_to_trits(value);
    for (0..10) |i| {
        out[i] = @intFromEnum(trits[i]);
    }
}

/// FFI: Convert from trits
export fn proven_malbolge_from_trits(trits: [*]const u8) Tryte {
    var arr: [10]Trit = undefined;
    for (0..10) |i| {
        arr[i] = @enumFromInt(@as(u2, @intCast(trits[i] % 3)));
    }
    return trits_to_tryte(arr);
}

/// FFI: Validate a Tryte value (for safety checks)
export fn proven_malbolge_validate(value: u32) bool {
    return value <= TRYTE_MAX;
}

// ============================================================================
// TESTS
// ============================================================================

test "crazy operation" {
    // Test some known values
    try std.testing.expectEqual(Trit.t1, crazy_trit(.t0, .t0));
    try std.testing.expectEqual(Trit.t0, crazy_trit(.t0, .t1));
    try std.testing.expectEqual(Trit.t1, crazy_trit(.t2, .t2));
}

test "rotate right" {
    // Rotating 3^9 (which is 1 followed by 9 zeros in ternary)
    // should give us 1 (which is 0 followed by 1)
    const val: Tryte = 19683; // 3^9 = [0,0,0,0,0,0,0,0,0,1] in ternary
    const rotated = rotate_right(val);
    // After rotation: [1,0,0,0,0,0,0,0,0,0] = 1
    try std.testing.expectEqual(@as(Tryte, 1), rotated);
}

test "tryte conversion roundtrip" {
    const original: Tryte = 12345;
    const trits = tryte_to_trits(original);
    const recovered = trits_to_tryte(trits);
    try std.testing.expectEqual(original, recovered);
}

test "wraparound addition" {
    try std.testing.expectEqual(@as(Tryte, 0), add_wrap(TRYTE_MAX, 1));
    try std.testing.expectEqual(@as(Tryte, 100), add_wrap(50, 50));
}
