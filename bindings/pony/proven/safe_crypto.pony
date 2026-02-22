// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCrypto - Cryptographic primitives via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This class is a thin wrapper; it does NOT reimplement any logic.

primitive SafeCrypto
  """
  Formally verified cryptographic operations.

  Example:
  ```pony
  match SafeCrypto.constant_time_eq("secret", "secret")
  | let v: Bool => env.out.print("Equal: " + v.string())
  | None => env.out.print("Error")
  end
  ```
  """

  fun constant_time_eq(a: String, b: String): (Bool | None) =>
    """
    Constant-time byte comparison (timing-attack safe).
    Returns false if lengths differ.
    """
    let r = _LibProven.crypto_constant_time_eq(
      a.cpointer(), a.size(),
      b.cpointer(), b.size())
    if r.status == ProvenOk() then r.value else None end

  fun random_bytes(n: USize): (Array[U8] iso^ | None) =>
    """
    Generate cryptographically secure random bytes.
    Returns an isolated array of n random bytes, or None on error.
    """
    let buf = recover iso Array[U8].init(0, n) end
    let status = _LibProven.crypto_random_bytes(buf.cpointer(), n)
    if status == ProvenOk() then
      consume buf
    else
      None
    end

  fun hex_encode(data: String, uppercase: Bool = false): (String | None) =>
    """Encode bytes to hexadecimal string."""
    let r = _LibProven.hex_encode(data.cpointer(), data.size(), uppercase)
    _LibProven._extract_string(r)
