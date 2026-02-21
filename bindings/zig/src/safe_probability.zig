// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe probability operations with value clamping.

const std = @import("std");

pub const ProbabilityError = error{
    OutOfRange,
    InvalidOdds,
};

/// Probability value constrained to [0, 1].
pub const Probability = struct {
    value: f64,

    /// Create a probability with validation.
    pub fn init(value: f64) ProbabilityError!Probability {
        if (value < 0.0 or value > 1.0) return error.OutOfRange;
        return Probability{ .value = value };
    }

    /// Create a probability, clamping to valid range.
    pub fn clamped(value: f64) Probability {
        return Probability{ .value = @max(0.0, @min(1.0, value)) };
    }

    /// Complement (1 - p).
    pub fn complement(self: Probability) Probability {
        return Probability{ .value = 1.0 - self.value };
    }

    /// Convert to percentage.
    pub fn toPercent(self: Probability) f64 {
        return self.value * 100.0;
    }

    /// Convert from percentage.
    pub fn fromPercent(percent: f64) ProbabilityError!Probability {
        return init(percent / 100.0);
    }

    /// Convert to odds (p / (1-p)).
    pub fn toOdds(self: Probability) ?f64 {
        if (self.value >= 1.0) return null;
        return self.value / (1.0 - self.value);
    }

    /// Convert from odds.
    pub fn fromOdds(odds: f64) ProbabilityError!Probability {
        if (odds < 0.0) return error.InvalidOdds;
        return init(odds / (1.0 + odds));
    }

    /// AND combination (independent events).
    pub fn and_(self: Probability, other: Probability) Probability {
        return Probability{ .value = self.value * other.value };
    }

    /// OR combination (independent events).
    pub fn or_(self: Probability, other: Probability) Probability {
        return Probability{ .value = self.value + other.value - self.value * other.value };
    }

    /// Bayes' theorem: P(A|B) = P(B|A) * P(A) / P(B).
    pub fn bayes(prior: Probability, likelihood: Probability, evidence: Probability) ?Probability {
        if (evidence.value == 0.0) return null;
        const posterior = (likelihood.value * prior.value) / evidence.value;
        if (posterior > 1.0) return null;
        return Probability{ .value = posterior };
    }
};

/// Expected value calculation.
pub fn expectedValue(outcomes: []const f64, probabilities: []const Probability) ?f64 {
    if (outcomes.len != probabilities.len) return null;
    if (outcomes.len == 0) return null;

    var sum: f64 = 0.0;
    var prob_sum: f64 = 0.0;

    for (outcomes, probabilities) |outcome, prob| {
        sum += outcome * prob.value;
        prob_sum += prob.value;
    }

    // Check probabilities sum to ~1
    if (prob_sum < 0.99 or prob_sum > 1.01) return null;

    return sum;
}

/// Binomial probability: P(k successes in n trials).
pub fn binomial(n: u32, k: u32, p: Probability) f64 {
    if (k > n) return 0.0;

    const coeff = binomialCoeff(n, k);
    const success = std.math.pow(f64, p.value, @floatFromInt(k));
    const failure = std.math.pow(f64, 1.0 - p.value, @floatFromInt(n - k));

    return @as(f64, @floatFromInt(coeff)) * success * failure;
}

fn binomialCoeff(n: u32, k: u32) u64 {
    if (k > n) return 0;
    if (k == 0 or k == n) return 1;

    var result: u64 = 1;
    var i: u32 = 0;
    while (i < k) : (i += 1) {
        result = result * (n - i) / (i + 1);
    }
    return result;
}

test "Probability" {
    const p = try Probability.init(0.5);
    try std.testing.expectApproxEqAbs(0.5, p.value, 0.001);
    try std.testing.expectApproxEqAbs(0.5, p.complement().value, 0.001);
}

test "Probability odds" {
    const p = try Probability.init(0.75);
    try std.testing.expectApproxEqAbs(3.0, p.toOdds().?, 0.001);
}
