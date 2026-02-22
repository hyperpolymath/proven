// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeUnit - FFI bindings to libproven unit conversion operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for unit operations.
pub const UnitError = error{
    InvalidArgument,
    OutOfBounds,
    ProvenError,
};

/// Length units (matching ProvenLengthUnit enum).
pub const LengthUnit = enum(c_int) {
    meters = c.PROVEN_LENGTH_METERS,
    kilometers = c.PROVEN_LENGTH_KILOMETERS,
    centimeters = c.PROVEN_LENGTH_CENTIMETERS,
    millimeters = c.PROVEN_LENGTH_MILLIMETERS,
    feet = c.PROVEN_LENGTH_FEET,
    inches = c.PROVEN_LENGTH_INCHES,
    miles = c.PROVEN_LENGTH_MILES,
    yards = c.PROVEN_LENGTH_YARDS,
};

/// Temperature units (matching ProvenTempUnit enum).
pub const TempUnit = enum(c_int) {
    celsius = c.PROVEN_TEMP_CELSIUS,
    fahrenheit = c.PROVEN_TEMP_FAHRENHEIT,
    kelvin = c.PROVEN_TEMP_KELVIN,
};

/// Convert length between units via libproven.
pub fn convertLength(value: f64, from: LengthUnit, to: LengthUnit) UnitError!f64 {
    const result = c.proven_unit_convert_length(value, @intFromEnum(from), @intFromEnum(to));
    return switch (result.status) {
        c.PROVEN_OK => result.value,
        c.PROVEN_ERR_INVALID_ARGUMENT => error.InvalidArgument,
        else => error.ProvenError,
    };
}

/// Convert temperature between units via libproven.
pub fn convertTemp(value: f64, from: TempUnit, to: TempUnit) UnitError!f64 {
    const result = c.proven_unit_convert_temp(value, @intFromEnum(from), @intFromEnum(to));
    return switch (result.status) {
        c.PROVEN_OK => result.value,
        c.PROVEN_ERR_OUT_OF_BOUNDS => error.OutOfBounds,
        else => error.ProvenError,
    };
}

test "convertLength" {
    const meters = try convertLength(1.0, .kilometers, .meters);
    try std.testing.expectApproxEqAbs(@as(f64, 1000.0), meters, 0.01);
}

test "convertTemp" {
    const fahrenheit = try convertTemp(100.0, .celsius, .fahrenheit);
    try std.testing.expectApproxEqAbs(@as(f64, 212.0), fahrenheit, 0.01);
}
