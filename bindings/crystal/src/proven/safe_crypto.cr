# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

require "openssl"
require "random/secure"

module Proven
  # Safe cryptographic operations.
  module SafeCrypto
    # Constant-time byte array comparison to prevent timing attacks.
    def self.constant_time_equals?(a : Bytes, b : Bytes) : Bool
      return false if a.size != b.size

      result = 0_u8
      a.size.times do |i|
        result |= a[i] ^ b[i]
      end
      result == 0
    end

    # Constant-time string comparison.
    def self.constant_time_equals_string?(a : String, b : String) : Bool
      constant_time_equals?(a.to_slice, b.to_slice)
    end

    # Generate cryptographically secure random bytes.
    def self.random_bytes(count : Int32) : Bytes
      Random::Secure.random_bytes(count)
    end

    # Convert bytes to hex string.
    def self.bytes_to_hex(bytes : Bytes) : String
      bytes.hexstring
    end

    # Generate random bytes as hex string.
    def self.random_hex(byte_count : Int32) : String
      bytes_to_hex(random_bytes(byte_count))
    end

    # Generate random bytes as base64 string.
    def self.random_base64(byte_count : Int32) : String
      Base64.strict_encode(random_bytes(byte_count))
    end

    # Generate URL-safe random string.
    def self.random_url_safe(byte_count : Int32) : String
      Base64.urlsafe_encode(random_bytes(byte_count), padding: false)
    end

    # Generate random integer in range [min, max].
    def self.random_int(min_val : Int32, max_val : Int32) : Int32
      lo, hi = min_val <= max_val ? {min_val, max_val} : {max_val, min_val}
      Random::Secure.rand(lo..hi)
    end

    # Generate a secure token.
    def self.generate_token(length : Int32 = 32) : String
      random_url_safe(length)
    end

    # Generate token with default length.
    def self.generate_token_default : String
      generate_token(32)
    end

    # Hash a string with SHA-256.
    def self.sha256(input : String) : String
      digest = OpenSSL::Digest.new("SHA256")
      digest.update(input)
      digest.final.hexstring
    end

    # Hash bytes with SHA-256.
    def self.sha256_bytes(input : Bytes) : String
      digest = OpenSSL::Digest.new("SHA256")
      digest.update(input)
      digest.final.hexstring
    end

    # Hash a string with SHA-512.
    def self.sha512(input : String) : String
      digest = OpenSSL::Digest.new("SHA512")
      digest.update(input)
      digest.final.hexstring
    end

    # Hash bytes with SHA-512.
    def self.sha512_bytes(input : Bytes) : String
      digest = OpenSSL::Digest.new("SHA512")
      digest.update(input)
      digest.final.hexstring
    end

    # Compute HMAC-SHA256.
    def self.hmac_sha256(key : String, message : String) : String
      hmac = OpenSSL::HMAC.digest(:sha256, key, message)
      hmac.hexstring
    end

    # Compute HMAC-SHA256 with bytes.
    def self.hmac_sha256_bytes(key : Bytes, message : Bytes) : String
      hmac = OpenSSL::HMAC.digest(:sha256, key, message)
      hmac.hexstring
    end

    # Compute HMAC-SHA512.
    def self.hmac_sha512(key : String, message : String) : String
      hmac = OpenSSL::HMAC.digest(:sha512, key, message)
      hmac.hexstring
    end

    # Verify HMAC using constant-time comparison.
    def self.verify_hmac_sha256?(key : String, message : String, expected_mac : String) : Bool
      constant_time_equals_string?(hmac_sha256(key, message), expected_mac)
    end

    # Verify HMAC-SHA512 using constant-time comparison.
    def self.verify_hmac_sha512?(key : String, message : String, expected_mac : String) : Bool
      constant_time_equals_string?(hmac_sha512(key, message), expected_mac)
    end

    # Hash a string with MD5 (NOT for security, only for checksums).
    def self.md5(input : String) : String
      digest = OpenSSL::Digest.new("MD5")
      digest.update(input)
      digest.final.hexstring
    end

    # Derive a key using PBKDF2-SHA256.
    def self.pbkdf2(password : String, salt : String, iterations : Int32, key_length : Int32) : String
      key = OpenSSL::PKCS5.pbkdf2_hmac(
        password,
        salt,
        iterations,
        algorithm: OpenSSL::Algorithm::SHA256,
        key_size: key_length
      )
      key.hexstring
    end

    # Derive key with default parameters.
    def self.pbkdf2_default(password : String, salt : String) : String
      pbkdf2(password, salt, 100000, 32)
    end

    # Generate a random password.
    def self.generate_password(
      length : Int32,
      include_uppercase : Bool = true,
      include_lowercase : Bool = true,
      include_numbers : Bool = true,
      include_symbols : Bool = true
    ) : String
      chars = ""
      chars += "abcdefghijklmnopqrstuvwxyz" if include_lowercase
      chars += "ABCDEFGHIJKLMNOPQRSTUVWXYZ" if include_uppercase
      chars += "0123456789" if include_numbers
      chars += "!@#$%^&*()_+-=[]{}|;:,.<>?" if include_symbols

      return "" if chars.empty?

      String.build(length) do |str|
        length.times do
          str << chars[Random::Secure.rand(chars.size)]
        end
      end
    end

    # Generate password with defaults.
    def self.generate_password_default : String
      generate_password(16, true, true, true, true)
    end

    # Securely wipe a byte array (best effort).
    def self.secure_wipe(data : Bytes) : Nil
      data.fill(0_u8)
    end
  end
end

require "base64"
