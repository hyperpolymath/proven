// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe hexadecimal encoding and decoding via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

open System.Runtime.InteropServices

module SafeHex =

    /// Encode bytes to hex string.
    /// uppercase: if true, produces uppercase hex characters.
    let encode (data: byte array) (uppercase: bool) : string option =
        FFI.proven_hex_encode(data, unativeint data.Length, uppercase) |> stringResultToOption

    /// Decode hex string to byte array.
    /// Returns None on invalid hex input (odd length, non-hex characters).
    let decode (hex: string) : byte array option =
        let bytes = toUtf8 hex
        let result = FFI.proven_hex_decode(bytes, unativeint bytes.Length)
        if result.Status = 0 && result.Data <> nativeint 0 then
            let output = Array.zeroCreate<byte>(int result.Length)
            Marshal.Copy(result.Data, output, 0, int result.Length)
            FFI.proven_hex_free(result.Data)
            Some output
        else
            if result.Data <> nativeint 0 then
                FFI.proven_hex_free(result.Data)
            None
