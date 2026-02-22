# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafePhone -- thin FFI wrapper around libproven's phone number operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafePhone
    PHONE_FORMAT_BUF_SIZE = 32

    # Parse a phone number string.  Returns the parsed PhoneNumber struct or nil.
    def self.parse(input : String) : LibProven::PhoneNumber?
      slice = input.to_slice
      result = LibProven.phone_parse(slice.to_unsafe, slice.size)
      return nil unless result.status == 0
      result.number
    end

    # Check if a phone number string is valid.
    def self.valid?(input : String) : Bool
      slice = input.to_slice
      LibProven.phone_is_valid(slice.to_unsafe, slice.size)
    end

    # Format phone number in E.164 format ("+15551234567").
    def self.format_e164(number : LibProven::PhoneNumber) : String?
      buf = Bytes.new(PHONE_FORMAT_BUF_SIZE)
      status = LibProven.phone_format_e164(pointerof(number), buf.to_unsafe, buf.size)
      return nil unless status == 0
      String.new(buf.to_unsafe)
    end

    # Format phone number in international format ("+1 555 123 4567").
    def self.format_international(number : LibProven::PhoneNumber) : String?
      buf = Bytes.new(PHONE_FORMAT_BUF_SIZE)
      status = LibProven.phone_format_international(pointerof(number), buf.to_unsafe, buf.size)
      return nil unless status == 0
      String.new(buf.to_unsafe)
    end

    # Format phone number in national format ("(555) 123-4567").
    def self.format_national(number : LibProven::PhoneNumber) : String?
      buf = Bytes.new(PHONE_FORMAT_BUF_SIZE)
      status = LibProven.phone_format_national(pointerof(number), buf.to_unsafe, buf.size)
      return nil unless status == 0
      String.new(buf.to_unsafe)
    end

    # Format phone number in RFC 3966 tel: URI format.
    def self.format_rfc3966(number : LibProven::PhoneNumber) : String?
      buf = Bytes.new(PHONE_FORMAT_BUF_SIZE)
      status = LibProven.phone_format_rfc3966(pointerof(number), buf.to_unsafe, buf.size)
      return nil unless status == 0
      String.new(buf.to_unsafe)
    end

    # Get total digit count for a phone number.
    def self.digit_count(number : LibProven::PhoneNumber) : LibC::SizeT
      LibProven.phone_digit_count(pointerof(number))
    end

    # Check if two phone numbers are equal.
    def self.equals?(a : LibProven::PhoneNumber, b : LibProven::PhoneNumber) : Bool
      LibProven.phone_equals(pointerof(a), pointerof(b))
    end
  end
end
