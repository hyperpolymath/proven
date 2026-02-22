// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe filesystem path operations via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafePath =

    /// Check if a path contains directory traversal sequences ("..").
    let hasTraversal (path: string) : bool option =
        let bytes = toUtf8 path
        FFI.proven_path_has_traversal(bytes, unativeint bytes.Length) |> boolResultToOption

    /// Sanitize a filename by removing dangerous characters.
    let sanitizeFilename (name: string) : string option =
        let bytes = toUtf8 name
        FFI.proven_path_sanitize_filename(bytes, unativeint bytes.Length) |> stringResultToOption
