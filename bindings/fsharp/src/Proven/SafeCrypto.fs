// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe cryptographic primitives via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafeCrypto =

    /// Constant-time byte comparison (timing-attack safe).
    let constantTimeEquals (a: byte array) (b: byte array) : bool option =
        FFI.proven_crypto_constant_time_eq(a, unativeint a.Length, b, unativeint b.Length)
        |> boolResultToOption

    /// Fill buffer with cryptographically secure random bytes.
    let randomBytes (length: int) : byte array option =
        let buf = Array.zeroCreate<byte> length
        let status = FFI.proven_crypto_random_bytes(buf, unativeint length)
        if status = 0 then Some buf
        else None
