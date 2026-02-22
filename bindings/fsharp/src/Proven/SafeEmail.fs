// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe email validation via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafeEmail =

    /// Validate email address (RFC 5321 simplified).
    let isValid (email: string) : bool option =
        let bytes = toUtf8 email
        FFI.proven_email_is_valid(bytes, unativeint bytes.Length) |> boolResultToOption
