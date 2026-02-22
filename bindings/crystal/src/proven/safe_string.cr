# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeString -- thin FFI wrapper around libproven's string operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeString
    # Helper: convert a StringResult to a Crystal String, freeing the C pointer.
    private def self.string_result_to_crystal(result : LibProven::StringResult) : String?
      return nil unless result.status == LibProven::ProvenStatus::Ok
      return nil if result.value.null?
      str = String.new(result.value, result.length)
      LibProven.proven_free_string(result.value)
      str
    end

    # Check if bytes are valid UTF-8.
    def self.valid_utf8?(input : Bytes) : Bool
      result = LibProven.proven_string_is_valid_utf8(input.to_unsafe, input.size)
      return false unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end

    # Check if a string is valid UTF-8.
    def self.valid_utf8?(input : String) : Bool
      valid_utf8?(input.to_slice)
    end

    # Escape string for SQL (single quotes).
    # Prefer parameterized queries over string escaping.
    def self.escape_sql(input : String) : String?
      slice = input.to_slice
      result = LibProven.proven_string_escape_sql(slice.to_unsafe, slice.size)
      string_result_to_crystal(result)
    end

    # Escape string for HTML (prevents XSS).
    def self.escape_html(input : String) : String?
      slice = input.to_slice
      result = LibProven.proven_string_escape_html(slice.to_unsafe, slice.size)
      string_result_to_crystal(result)
    end

    # Escape string for JavaScript string literals.
    def self.escape_js(input : String) : String?
      slice = input.to_slice
      result = LibProven.proven_string_escape_js(slice.to_unsafe, slice.size)
      string_result_to_crystal(result)
    end
  end
end
