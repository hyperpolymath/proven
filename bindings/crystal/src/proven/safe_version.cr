# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeVersion -- thin FFI wrapper around libproven's semver operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeVersion
    # Parse a semantic version string.  Returns {major, minor, patch, prerelease} or nil.
    def self.parse(input : String) : {UInt32, UInt32, UInt32, String?}?
      slice = input.to_slice
      result = LibProven.proven_version_parse(slice.to_unsafe, slice.size)
      return nil unless result.status == LibProven::ProvenStatus::Ok

      v = result.version
      prerelease = if !v.prerelease.null? && v.prerelease_len > 0
                     String.new(v.prerelease, v.prerelease_len)
                   else
                     nil
                   end

      # Free the prerelease string allocated by libproven.
      LibProven.proven_version_free(pointerof(result.version))

      {v.major, v.minor, v.patch, prerelease}
    end

    # Compare two semantic versions.
    # Returns negative if a < b, 0 if equal, positive if a > b.
    def self.compare(a_str : String, b_str : String) : Int32?
      a_slice = a_str.to_slice
      b_slice = b_str.to_slice

      a_result = LibProven.proven_version_parse(a_slice.to_unsafe, a_slice.size)
      return nil unless a_result.status == LibProven::ProvenStatus::Ok

      b_result = LibProven.proven_version_parse(b_slice.to_unsafe, b_slice.size)
      unless b_result.status == LibProven::ProvenStatus::Ok
        LibProven.proven_version_free(pointerof(a_result.version))
        return nil
      end

      cmp = LibProven.proven_version_compare(a_result.version, b_result.version)
      LibProven.proven_version_free(pointerof(a_result.version))
      LibProven.proven_version_free(pointerof(b_result.version))
      cmp
    end
  end
end
