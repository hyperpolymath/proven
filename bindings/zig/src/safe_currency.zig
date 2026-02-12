// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe currency operations with type-safe monetary values.
//! All values stored as minor units (cents) to avoid floating-point errors.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// ISO 4217 currency codes
pub const CurrencyCode = enum {
    USD, EUR, GBP, JPY, CHF, CAD, AUD, NZD, CNY, INR,
    BRL, MXN, KRW, SGD, HKD, SEK, NOK, DKK, PLN, RUB,
    ZAR, TRY, THB, MYR, IDR, PHP, VND, AED, SAR, ILS,
    CZK, HUF, RON, BGN, HRK, ISK, CLP, COP, PEN, ARS,
    BTC, ETH,

    /// Get number of decimal places for this currency
    pub fn decimals(self: CurrencyCode) u8 {
        return switch (self) {
            .JPY, .KRW, .VND => 0,
            .BTC => 8,
            .ETH => 8, // Capped at 8 for practical use
            else => 2,
        };
    }

    /// Get the currency symbol
    pub fn symbol(self: CurrencyCode) []const u8 {
        return switch (self) {
            .USD => "$",
            .EUR => "€",
            .GBP => "£",
            .JPY, .CNY => "¥",
            .CHF => "Fr",
            .CAD => "C$",
            .AUD => "A$",
            .NZD => "NZ$",
            .INR => "₹",
            .BRL => "R$",
            .KRW => "₩",
            .RUB => "₽",
            .TRY => "₺",
            .THB => "฿",
            .PHP => "₱",
            .VND => "₫",
            .ILS => "₪",
            .BTC => "₿",
            .ETH => "Ξ",
            else => "",
        };
    }

    /// Get the currency name
    pub fn name(self: CurrencyCode) []const u8 {
        return switch (self) {
            .USD => "US Dollar",
            .EUR => "Euro",
            .GBP => "British Pound",
            .JPY => "Japanese Yen",
            .CHF => "Swiss Franc",
            .CAD => "Canadian Dollar",
            .AUD => "Australian Dollar",
            .NZD => "New Zealand Dollar",
            .CNY => "Chinese Yuan",
            .INR => "Indian Rupee",
            .BRL => "Brazilian Real",
            .MXN => "Mexican Peso",
            .KRW => "South Korean Won",
            .BTC => "Bitcoin",
            .ETH => "Ethereum",
            else => @tagName(self),
        };
    }
};

/// Type-safe monetary value
pub fn Money(comptime currency: CurrencyCode) type {
    return struct {
        const Self = @This();
        const Currency = currency;

        /// Amount in minor units (cents, satoshis, etc.)
        minor_units: i64,

        /// Create from major units (dollars, euros, etc.)
        pub fn fromMajor(major: i64) Self {
            const multiplier = std.math.pow(i64, 10, currency.decimals());
            return .{ .minor_units = major * multiplier };
        }

        /// Create from minor units (cents, pence, etc.)
        pub fn fromMinor(minor: i64) Self {
            return .{ .minor_units = minor };
        }

        /// Zero amount
        pub fn zero() Self {
            return .{ .minor_units = 0 };
        }

        /// Get major units (truncated)
        pub fn getMajor(self: Self) i64 {
            const divisor = std.math.pow(i64, 10, currency.decimals());
            return @divTrunc(self.minor_units, divisor);
        }

        /// Get minor units
        pub fn getMinor(self: Self) i64 {
            return self.minor_units;
        }

        /// Add two monetary values (same currency enforced by type)
        pub fn add(self: Self, other: Self) Self {
            return .{ .minor_units = self.minor_units + other.minor_units };
        }

        /// Subtract two monetary values
        pub fn sub(self: Self, other: Self) Self {
            return .{ .minor_units = self.minor_units - other.minor_units };
        }

        /// Multiply by scalar
        pub fn mul(self: Self, scalar: i64) Self {
            return .{ .minor_units = self.minor_units * scalar };
        }

        /// Divide by scalar (truncates)
        pub fn div(self: Self, scalar: i64) ?Self {
            if (scalar == 0) return null;
            return .{ .minor_units = @divTrunc(self.minor_units, scalar) };
        }

        /// Negate
        pub fn negate(self: Self) Self {
            return .{ .minor_units = -self.minor_units };
        }

        /// Absolute value
        pub fn abs(self: Self) Self {
            return .{ .minor_units = if (self.minor_units < 0) -self.minor_units else self.minor_units };
        }

        /// Check if zero
        pub fn isZero(self: Self) bool {
            return self.minor_units == 0;
        }

        /// Check if positive
        pub fn isPositive(self: Self) bool {
            return self.minor_units > 0;
        }

        /// Check if negative
        pub fn isNegative(self: Self) bool {
            return self.minor_units < 0;
        }

        /// Compare values
        pub fn cmp(self: Self, other: Self) std.math.Order {
            return std.math.order(self.minor_units, other.minor_units);
        }

        /// Format to string buffer
        pub fn format(self: Self, buf: []u8) ![]const u8 {
            const dec = currency.decimals();
            const divisor = std.math.pow(i64, 10, dec);
            const abs_units = if (self.minor_units < 0) -self.minor_units else self.minor_units;
            const major = @divTrunc(abs_units, divisor);
            const minor = @rem(abs_units, divisor);

            if (self.minor_units < 0) {
                if (dec == 0) {
                    return std.fmt.bufPrint(buf, "-{s}{d}", .{ currency.symbol(), major });
                } else {
                    return std.fmt.bufPrint(buf, "-{s}{d}.{d:0>[1]}", .{ currency.symbol(), major, @as(usize, dec), minor });
                }
            } else {
                if (dec == 0) {
                    return std.fmt.bufPrint(buf, "{s}{d}", .{ currency.symbol(), major });
                } else {
                    return std.fmt.bufPrint(buf, "{s}{d}.{d:0>[1]}", .{ currency.symbol(), major, @as(usize, dec), minor });
                }
            }
        }
    };
}

/// Currency error types
pub const CurrencyError = error{
    InvalidAmount,
    DivisionByZero,
    Overflow,
    InvalidCurrencyCode,
    OutOfMemory,
};

/// Parse currency code from string
pub fn parseCurrencyCode(str: []const u8) ?CurrencyCode {
    const codes = std.meta.fields(CurrencyCode);
    inline for (codes) |field| {
        if (std.ascii.eqlIgnoreCase(str, field.name)) {
            return @field(CurrencyCode, field.name);
        }
    }
    return null;
}

/// Check if string is valid currency code
pub fn isValidCurrencyCode(str: []const u8) bool {
    return parseCurrencyCode(str) != null;
}

test "Money basic operations" {
    const USD = Money(.USD);
    const a = USD.fromMajor(100);
    const b = USD.fromMajor(50);

    try std.testing.expectEqual(@as(i64, 15000), a.add(b).getMinor());
    try std.testing.expectEqual(@as(i64, 5000), a.sub(b).getMinor());
    try std.testing.expectEqual(@as(i64, 20000), a.mul(2).getMinor());
}

test "Money zero decimals" {
    const JPY = Money(.JPY);
    const amount = JPY.fromMajor(1000);
    try std.testing.expectEqual(@as(i64, 1000), amount.getMinor());
    try std.testing.expectEqual(@as(i64, 1000), amount.getMajor());
}

test "parseCurrencyCode" {
    try std.testing.expect(parseCurrencyCode("USD") == .USD);
    try std.testing.expect(parseCurrencyCode("usd") == .USD);
    try std.testing.expect(parseCurrencyCode("XYZ") == null);
}
