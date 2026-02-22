# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven::SafeCrypto - Cryptographic primitives and hex encoding.
#
# Thin wrapper over libproven's SafeCrypto and SafeHex modules.  All
# cryptographic operations are performed in formally verified Idris 2 code.
# This module does NOT reimplement any crypto or encoding logic.

unit module Proven::SafeCrypto;

use NativeCall;
use Proven::LibProven;

# ============================================================================
# Constant-time comparison
# ============================================================================

#| Constant-time byte comparison (timing-attack safe).
#| Compares two strings for equality without leaking length information via
#| timing side-channels.  Returns False if lengths differ.
#| Returns True/False, or Nil on internal error.
sub constant-time-eq(Str:D $a, Str:D $b --> Bool) is export {
    my ($buf-a, $len-a) = str-to-buf($a);
    my ($buf-b, $len-b) = str-to-buf($b);
    my BoolResult $r = proven_crypto_constant_time_eq(
        nativecast(Pointer, $buf-a), $len-a,
        nativecast(Pointer, $buf-b), $len-b,
    );
    return Nil unless $r.status == 0;
    return $r.value;
}

# ============================================================================
# Secure random bytes
# ============================================================================

#| Fill a Buf with cryptographically secure random bytes.
#| Returns a Buf of $n bytes, or Nil on failure.
sub random-bytes(Int:D $n where * > 0 --> Buf) is export {
    my $ca = CArray[uint8].allocate($n);
    my int32 $status = proven_crypto_random_bytes(nativecast(Pointer, $ca), $n);
    return Nil unless $status == 0;
    my $result = Buf.new;
    for ^$n -> $i {
        $result.push($ca[$i]);
    }
    return $result;
}

# ============================================================================
# Hex encoding / decoding
# ============================================================================

#| Encode bytes (given as a Str) to a hexadecimal string.
#| If $uppercase is True, uppercase hex digits are used.
#| Returns the hex string, or Nil on error.
sub hex-encode(Str:D $data, Bool :$uppercase = False --> Str) is export {
    my ($buf, $len) = str-to-buf($data);
    my StringResult $r = proven_hex_encode(nativecast(Pointer, $buf), $len, $uppercase);
    return extract-string($r);
}

#| Decode a hexadecimal string to the original bytes (returned as a Str).
#| Returns the decoded string, or Nil on error.
sub hex-decode(Str:D $hex --> Str) is export {
    my ($buf, $len) = str-to-buf($hex);
    # proven_hex_decode returns a struct with the same layout as StringResult
    my StringResult $r = proven_hex_decode(nativecast(Pointer, $buf), $len);
    return extract-string($r);
}

# ============================================================================
# Checksum
# ============================================================================

#| Calculate CRC32 checksum of the given string.
#| Returns the CRC32 value as an Int, or Nil on error.
sub checksum-crc32(Str:D $data --> Int) is export {
    my ($buf, $len) = str-to-buf($data);
    my IntResult $r = proven_checksum_crc32(nativecast(Pointer, $buf), $len);
    return Nil unless $r.status == 0;
    return $r.value;
}

#| Verify that the CRC32 of the given data matches an expected value.
#| Returns True/False, or Nil on error.
sub checksum-verify-crc32(Str:D $data, Int:D $expected --> Bool) is export {
    my ($buf, $len) = str-to-buf($data);
    my BoolResult $r = proven_checksum_verify_crc32(nativecast(Pointer, $buf), $len, $expected);
    return Nil unless $r.status == 0;
    return $r.value;
}
