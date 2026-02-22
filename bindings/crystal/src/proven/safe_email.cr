# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeEmail -- thin FFI wrapper around libproven's email validation.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeEmail
    # Validate an email address (RFC 5321 simplified).
    def self.valid?(email : String) : Bool
      slice = email.to_slice
      result = LibProven.proven_email_is_valid(slice.to_unsafe, slice.size)
      return false unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end
  end
end
