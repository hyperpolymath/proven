# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

require "openssl"
require "securerandom"

module Proven
  # Cryptographic safety operations with constant-time guarantees.
  module SafeCrypto
    class << self
      # Compare two strings in constant time to prevent timing attacks.
      # Uses OpenSSL's constant-time comparison when available.
      #
      # @param a [String]
      # @param b [String]
      # @return [Boolean]
      def constant_time_compare(a, b)
        # OpenSSL.fixed_length_secure_compare requires same length
        return false unless a.bytesize == b.bytesize
        return true if a.empty? && b.empty?

        if OpenSSL.respond_to?(:fixed_length_secure_compare)
          OpenSSL.fixed_length_secure_compare(a, b)
        else
          # Fallback for older Ruby
          constant_time_compare_fallback(a, b)
        end
      end

      # Securely zero a string (in-place modification).
      # Note: Due to Ruby's string mutability rules, this creates a new zeroed string.
      #
      # @param length [Integer]
      # @return [String]
      def secure_zero(length)
        "\x00" * length
      end

      # Generate cryptographically secure random bytes.
      #
      # @param n [Integer]
      # @return [String]
      def random_bytes(n)
        SecureRandom.random_bytes(n)
      end

      # Generate a cryptographically secure random hex string.
      #
      # @param n [Integer] number of random bytes (output will be 2n hex chars)
      # @return [String]
      def random_hex(n)
        SecureRandom.hex(n)
      end

      private

      # Fallback constant-time comparison for older Ruby versions.
      def constant_time_compare_fallback(a, b)
        return false unless a.bytesize == b.bytesize

        result = 0
        a.bytes.zip(b.bytes).each do |x, y|
          result |= x ^ y
        end
        result.zero?
      end
    end
  end
end
