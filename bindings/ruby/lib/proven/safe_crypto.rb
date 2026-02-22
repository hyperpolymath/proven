# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe cryptographic operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeCrypto
    class << self
      # Compare two byte strings in constant time (timing-safe).
      # Returns nil on error.
      #
      # @param a [String]
      # @param b [String]
      # @return [Boolean, nil]
      def constant_time_compare(a, b)
        return nil if a.nil? || b.nil?
        ptr_a, len_a = FFI.str_to_ptr(a)
        ptr_b, len_b = FFI.str_to_ptr(b)
        result = FFI.invoke_bool_result(
          "proven_crypto_constant_time_eq",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T,
           Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [ptr_a, len_a, ptr_b, len_b]
        )
        return nil unless result
        status, val = result
        status == FFI::STATUS_OK ? val : nil
      end

      # Fill a buffer with cryptographically secure random bytes.
      # Returns nil on error.
      #
      # @param n [Integer] number of bytes
      # @return [String, nil] binary string of random bytes
      def random_bytes(n)
        return nil if n.nil? || n < 0
        return "".b if n.zero?
        buf = Fiddle::Pointer.malloc(n, Fiddle::RUBY_FREE)
        status = FFI.invoke_i32(
          "proven_crypto_random_bytes",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [buf, n]
        )
        return nil unless status == FFI::STATUS_OK
        buf.to_str(n).dup
      end

      # Generate a cryptographically secure random hex string.
      # Returns nil on error.
      #
      # @param n [Integer] number of random bytes (output = 2n hex chars)
      # @return [String, nil]
      def random_hex(n)
        bytes = random_bytes(n)
        return nil if bytes.nil?
        bytes.unpack1("H*")
      end
    end
  end
end
