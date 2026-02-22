# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeJson -- thin FFI wrapper around libproven's JSON validation.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeJson
    # Check if a string is valid JSON.
    def self.valid?(input : String) : Bool
      slice = input.to_slice
      result = LibProven.proven_json_is_valid(slice.to_unsafe, slice.size)
      return false unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end

    # Get the root-level JSON value type.
    def self.get_type(input : String) : LibProven::JsonType
      slice = input.to_slice
      LibProven.proven_json_get_type(slice.to_unsafe, slice.size)
    end
  end
end
