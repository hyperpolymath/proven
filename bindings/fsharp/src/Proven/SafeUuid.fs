// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe UUID generation, parsing, and validation via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafeUuid =

    /// Generate UUID v4 (random).
    let v4 () : ProvenUUID option =
        let result = FFI.proven_uuid_v4()
        if result.Status = 0 then Some result.UUID
        else None

    /// Format UUID as canonical string.
    let toString (uuid: ProvenUUID) : string option =
        FFI.proven_uuid_to_string(uuid) |> stringResultToOption

    /// Parse UUID from string.
    let parse (s: string) : ProvenUUID option =
        let bytes = toUtf8 s
        let result = FFI.proven_uuid_parse(bytes, unativeint bytes.Length)
        if result.Status = 0 then Some result.UUID
        else None

    /// Check if UUID is nil (all zeros).
    let isNil (uuid: ProvenUUID) : bool =
        FFI.proven_uuid_is_nil(uuid)

    /// Get UUID version.
    let version (uuid: ProvenUUID) : byte =
        FFI.proven_uuid_version(uuid)
