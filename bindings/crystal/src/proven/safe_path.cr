# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafePath -- thin FFI wrapper around libproven's path operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafePath
    # Check if path contains directory traversal sequences ("..").
    def self.has_traversal?(path : String) : Bool
      slice = path.to_slice
      result = LibProven.proven_path_has_traversal(slice.to_unsafe, slice.size)
      return false unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end

    # Sanitize a filename by removing dangerous characters.
    # Keeps only alphanumeric characters, dots, dashes, and underscores.
    def self.sanitize_filename(filename : String) : String?
      slice = filename.to_slice
      result = LibProven.proven_path_sanitize_filename(slice.to_unsafe, slice.size)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      return nil if result.value.null?
      str = String.new(result.value, result.length)
      LibProven.proven_free_string(result.value)
      str
    end
  end
end
