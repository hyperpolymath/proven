# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeCrypto -- thin FFI wrapper around libproven's cryptographic operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeCrypto
    # Constant-time byte comparison (timing-safe).
    # Use for comparing secrets to prevent timing attacks.
    def self.constant_time_equals?(a : Bytes, b : Bytes) : Bool
      result = LibProven.proven_crypto_constant_time_eq(
        a.to_unsafe, a.size,
        b.to_unsafe, b.size
      )
      return false unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end

    # Constant-time string comparison (timing-safe).
    def self.constant_time_equals?(a : String, b : String) : Bool
      constant_time_equals?(a.to_slice, b.to_slice)
    end

    # Fill buffer with cryptographically secure random bytes.
    def self.random_bytes(count : Int32) : Bytes?
      buf = Bytes.new(count)
      status = LibProven.proven_crypto_random_bytes(buf.to_unsafe, count)
      return nil unless status == LibProven::ProvenStatus::Ok
      buf
    end
  end
end
