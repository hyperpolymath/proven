// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Probability operations delegated to libproven FFI.
///
/// All computation via the formally verified Idris 2 core.

import CProven

public enum SafeProbability {
    /// Create a probability value clamped to [0, 1].
    public static func create(_ value: Double) -> Double {
        proven_probability_create(value)
    }

    /// Probability of A AND B (independent events).
    public static func and(_ a: Double, _ b: Double) -> Double {
        proven_probability_and(a, b)
    }

    /// Probability of A OR B (mutually exclusive events).
    public static func orExclusive(_ a: Double, _ b: Double) -> Double {
        proven_probability_or_exclusive(a, b)
    }

    /// Probability of NOT A.
    public static func not(_ p: Double) -> Double {
        proven_probability_not(p)
    }
}
