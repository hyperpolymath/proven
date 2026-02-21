// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe physical unit operations with dimension checking.

const std = @import("std");

pub const UnitError = error{
    DimensionMismatch,
    DivisionByZero,
};

/// Physical dimensions (SI base units).
pub const Dimension = struct {
    length: i8 = 0,     // L (meter)
    mass: i8 = 0,       // M (kilogram)
    time: i8 = 0,       // T (second)
    current: i8 = 0,    // I (ampere)
    temperature: i8 = 0, // Θ (kelvin)
    amount: i8 = 0,     // N (mole)
    luminosity: i8 = 0, // J (candela)

    pub fn eql(self: Dimension, other: Dimension) bool {
        return self.length == other.length and
            self.mass == other.mass and
            self.time == other.time and
            self.current == other.current and
            self.temperature == other.temperature and
            self.amount == other.amount and
            self.luminosity == other.luminosity;
    }

    pub fn mul(self: Dimension, other: Dimension) Dimension {
        return .{
            .length = self.length + other.length,
            .mass = self.mass + other.mass,
            .time = self.time + other.time,
            .current = self.current + other.current,
            .temperature = self.temperature + other.temperature,
            .amount = self.amount + other.amount,
            .luminosity = self.luminosity + other.luminosity,
        };
    }

    pub fn div(self: Dimension, other: Dimension) Dimension {
        return .{
            .length = self.length - other.length,
            .mass = self.mass - other.mass,
            .time = self.time - other.time,
            .current = self.current - other.current,
            .temperature = self.temperature - other.temperature,
            .amount = self.amount - other.amount,
            .luminosity = self.luminosity - other.luminosity,
        };
    }
};

// Common dimensions
pub const dimensionless = Dimension{};
pub const length_dim = Dimension{ .length = 1 };
pub const mass_dim = Dimension{ .mass = 1 };
pub const time_dim = Dimension{ .time = 1 };
pub const velocity_dim = Dimension{ .length = 1, .time = -1 };
pub const acceleration_dim = Dimension{ .length = 1, .time = -2 };
pub const force_dim = Dimension{ .length = 1, .mass = 1, .time = -2 };
pub const energy_dim = Dimension{ .length = 2, .mass = 1, .time = -2 };
pub const power_dim = Dimension{ .length = 2, .mass = 1, .time = -3 };

/// A quantity with value and dimension.
pub const Quantity = struct {
    value: f64,
    dim: Dimension,

    pub fn add(self: Quantity, other: Quantity) UnitError!Quantity {
        if (!self.dim.eql(other.dim)) return error.DimensionMismatch;
        return .{ .value = self.value + other.value, .dim = self.dim };
    }

    pub fn sub(self: Quantity, other: Quantity) UnitError!Quantity {
        if (!self.dim.eql(other.dim)) return error.DimensionMismatch;
        return .{ .value = self.value - other.value, .dim = self.dim };
    }

    pub fn mul(self: Quantity, other: Quantity) Quantity {
        return .{
            .value = self.value * other.value,
            .dim = self.dim.mul(other.dim),
        };
    }

    pub fn div(self: Quantity, other: Quantity) UnitError!Quantity {
        if (other.value == 0.0) return error.DivisionByZero;
        return .{
            .value = self.value / other.value,
            .dim = self.dim.div(other.dim),
        };
    }

    pub fn scale(self: Quantity, factor: f64) Quantity {
        return .{ .value = self.value * factor, .dim = self.dim };
    }
};

// Constructors for common units
pub fn meters(value: f64) Quantity {
    return .{ .value = value, .dim = length_dim };
}

pub fn kilometers(value: f64) Quantity {
    return .{ .value = value * 1000.0, .dim = length_dim };
}

pub fn seconds(value: f64) Quantity {
    return .{ .value = value, .dim = time_dim };
}

pub fn kilograms(value: f64) Quantity {
    return .{ .value = value, .dim = mass_dim };
}

pub fn newtons(value: f64) Quantity {
    return .{ .value = value, .dim = force_dim };
}

pub fn joules(value: f64) Quantity {
    return .{ .value = value, .dim = energy_dim };
}

test "Quantity dimension check" {
    const dist = meters(100.0);
    const time = seconds(10.0);
    const velocity = try dist.div(time);
    try std.testing.expect(velocity.dim.eql(velocity_dim));
}

test "Quantity dimension mismatch" {
    const dist = meters(100.0);
    const mass = kilograms(10.0);
    try std.testing.expectError(error.DimensionMismatch, dist.add(mass));
}
