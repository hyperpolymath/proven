// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCrypto.pike - Cryptographic primitives for Pike.
//
// All operations delegate to libproven via LibProven. Returns UNDEFINED
// on error.
//
// Usage:
//   import Proven;
//   LibProven.init();
//   int eq = SafeCrypto.constant_time_eq("secret", "secret");
//   string hex = SafeCrypto.random_hex(16);
//   LibProven.deinit();

//! @class SafeCrypto
//! Timing-safe comparison and secure random generation.
//!
//! Provides constant-time byte comparison (for preventing timing attacks),
//! cryptographic random byte generation, hex encoding/decoding, and
//! CRC32 checksums.

protected LibProven lib = LibProven();

//! @decl int(0..1)|zero constant_time_eq(string a, string b)
//! Constant-time byte comparison (timing-attack safe).
//! @returns
//!   @expr{1@} if equal, @expr{0@} if not, @expr{UNDEFINED@} on error.
int(0..1)|zero constant_time_eq(string a, string b)
{
    return lib->call_bool("crypto_constant_time_eq", ({a, b}));
}

//! @decl string|zero random_hex(int nbytes)
//! Generate cryptographically secure random bytes as hex.
//! @param nbytes
//!   Number of random bytes (1-1024).
//! @returns
//!   Hex-encoded random bytes (2*nbytes chars), or @expr{UNDEFINED@} on error.
string|zero random_hex(int nbytes)
{
    return lib->call_string("crypto_random_hex", ({(string)nbytes}));
}

//! @decl string|zero hex_encode(string data)
//! Hex-encode a string.
//! @returns
//!   Lowercase hex string, or @expr{UNDEFINED@} on error.
string|zero hex_encode(string data)
{
    return lib->call_string("hex_encode", ({data}));
}

//! @decl string|zero hex_decode(string hex_str)
//! Hex-decode a string.
//! @returns
//!   Decoded bytes, or @expr{UNDEFINED@} on error.
string|zero hex_decode(string hex_str)
{
    return lib->call_string("hex_decode", ({hex_str}));
}

//! @decl int|zero crc32(string data)
//! Compute CRC32 checksum.
//! @returns
//!   CRC32 value, or @expr{UNDEFINED@} on error.
int|zero crc32(string data)
{
    return lib->call_int("checksum_crc32", ({data}));
}
