# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe email validation via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeEmail
    class << self
      # Check if an email address is valid (RFC 5321 simplified).
      #
      # @param email [String]
      # @return [Boolean, nil]
      def valid?(email)
        return nil if email.nil?
        ptr, len = FFI.str_to_ptr(email)
        result = FFI.invoke_bool_result(
          "proven_email_is_valid",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [ptr, len]
        )
        return nil unless result
        status, val = result
        status == FFI::STATUS_OK ? val : nil
      end
    end
  end
end
