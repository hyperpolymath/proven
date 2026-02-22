# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe semantic versioning operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeVersion
    # Parsed semantic version from libproven.
    SemVer = Struct.new(:major, :minor, :patch, :prerelease, keyword_init: true) do
      def to_s
        version = "#{major}.#{minor}.#{patch}"
        version += "-#{prerelease}" if prerelease
        version
      end
    end

    class << self
      # Parse a semantic version string.
      # Returns nil on invalid input.
      #
      # @param version_string [String]
      # @return [SemVer, nil]
      def parse(version_string)
        return nil if version_string.nil?
        ptr, len = FFI.str_to_ptr(version_string)

        # VersionResult = { i32 status, SemanticVersion { u32 major, u32 minor, u32 patch, size_t prerelease_len, ptr prerelease } }
        # Layout: 4(status) + 4pad + 4(major) + 4(minor) + 4(patch) + 4pad + 8(len) + 8(ptr) = 40 bytes
        buf = Fiddle::Pointer.malloc(48, Fiddle::RUBY_FREE)

        fn = Fiddle::Function.new(
          FFI.handler["proven_version_parse"],
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          Fiddle::TYPE_VOID,
          need_gvl: true
        )
        fn.call(buf, ptr, len)

        packed = buf.to_str(40)
        status = packed[0, 4].unpack1("l!")
        return nil unless status == FFI::STATUS_OK

        major = packed[4, 4].unpack1("L") # u32
        minor = packed[8, 4].unpack1("L")
        patch = packed[12, 4].unpack1("L")
        prerelease_len = packed[16, 8].unpack1("Q") # size_t
        prerelease_ptr_val = packed[24, 8].unpack1("Q")

        prerelease = nil
        if prerelease_ptr_val != 0 && prerelease_len > 0
          pre_ptr = Fiddle::Pointer.new(prerelease_ptr_val)
          prerelease = pre_ptr.to_str(prerelease_len).dup
        end

        SemVer.new(
          major: major,
          minor: minor,
          patch: patch,
          prerelease: prerelease
        )
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Check if a string is valid SemVer.
      #
      # @param version_string [String]
      # @return [Boolean]
      def valid?(version_string)
        !parse(version_string).nil?
      end

      # Compare two semantic version structs.
      # Returns -1, 0, or 1.
      # Returns nil on error.
      #
      # @param v1 [String]
      # @param v2 [String]
      # @return [Integer, nil]
      def compare(v1, v2)
        sv1 = parse(v1)
        sv2 = parse(v2)
        return nil if sv1.nil? || sv2.nil?

        # Pack two SemanticVersion structs for proven_version_compare
        # SemanticVersion = { u32, u32, u32, size_t, ptr } = 32 bytes each
        # Pass by value -- this is complex with Fiddle.
        # Use a simpler comparison approach: pack into the struct layout.
        pack_a = [sv1.major, sv1.minor, sv1.patch, 0, 0].pack("LLLQQ")
        pack_b = [sv2.major, sv2.minor, sv2.patch, 0, 0].pack("LLLQQ")

        buf_a = Fiddle::Pointer.to_ptr(pack_a)
        buf_b = Fiddle::Pointer.to_ptr(pack_b)

        # proven_version_compare takes two SemanticVersion by value.
        # Each is 32 bytes on 64-bit. Since > 16 bytes, they would be
        # passed by pointer on some ABIs. Use the direct int approach
        # for the numeric fields only (ignoring prerelease for FFI compare).
        result = FFI.invoke_i32(
          "proven_version_compare",
          # Two structs by value: pass each field individually
          # SemanticVersion a: u32, u32, u32, size_t, ptr
          # SemanticVersion b: u32, u32, u32, size_t, ptr
          [Fiddle::TYPE_INT, Fiddle::TYPE_INT, Fiddle::TYPE_INT,
           Fiddle::TYPE_SIZE_T, Fiddle::TYPE_VOIDP,
           Fiddle::TYPE_INT, Fiddle::TYPE_INT, Fiddle::TYPE_INT,
           Fiddle::TYPE_SIZE_T, Fiddle::TYPE_VOIDP],
          [sv1.major, sv1.minor, sv1.patch, 0, Fiddle::Pointer.new(0),
           sv2.major, sv2.minor, sv2.patch, 0, Fiddle::Pointer.new(0)]
        )
        result
      rescue Fiddle::DLError, TypeError
        nil
      end
    end
  end
end
