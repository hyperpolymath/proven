// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCrypto - Cryptographic primitives via libproven FFI.
// All computation is performed in verified Idris 2 code via the Zig FFI
// layer. This Wren class is a thin wrapper; it does NOT reimplement any logic.

// Safe cryptographic operations.
// Returns null on error instead of crashing.
class SafeCrypto {
    // Constant-time byte comparison (timing-attack safe).
    // Returns true if equal, false if different, null on error.
    foreign static constantTimeEq(a, b)
}
