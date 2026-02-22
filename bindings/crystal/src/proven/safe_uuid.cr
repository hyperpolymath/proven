# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeUuid -- thin FFI wrapper around libproven's UUID operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeUuid
    UUID_STRING_LEN = 36
    UUID_URN_LEN    = 45

    # Generate a v4 (random) UUID and return its canonical string form.
    def self.generate_v4 : String?
      uuid = LibProven::Uuid.new
      status = LibProven.uuid_v4_generate(pointerof(uuid))
      return nil unless status == 0
      to_string(uuid)
    end

    # Parse a UUID from its canonical string form.
    # Returns the raw 16-byte struct or nil on failure.
    def self.parse(input : String) : LibProven::Uuid?
      uuid = LibProven::Uuid.new
      slice = input.to_slice
      status = LibProven.uuid_parse(slice.to_unsafe, slice.size, pointerof(uuid))
      return nil unless status == 0
      uuid
    end

    # Format a UUID as its canonical lowercase string.
    def self.to_string(uuid : LibProven::Uuid) : String?
      buf = Bytes.new(UUID_STRING_LEN + 1)
      status = LibProven.uuid_to_string(pointerof(uuid), buf.to_unsafe, buf.size)
      return nil unless status == 0
      String.new(buf.to_unsafe, UUID_STRING_LEN)
    end

    # Format a UUID as a URN ("urn:uuid:...").
    def self.to_urn(uuid : LibProven::Uuid) : String?
      buf = Bytes.new(UUID_URN_LEN + 1)
      status = LibProven.uuid_to_urn(pointerof(uuid), buf.to_unsafe, buf.size)
      return nil unless status == 0
      String.new(buf.to_unsafe, UUID_URN_LEN)
    end

    # Check if a UUID is nil (all zeros).
    def self.nil?(uuid : LibProven::Uuid) : Bool
      LibProven.uuid_is_nil(pointerof(uuid))
    end

    # Compare two UUIDs.  Returns 0 if equal.
    def self.compare(a : LibProven::Uuid, b : LibProven::Uuid) : Int32
      LibProven.uuid_compare(pointerof(a), pointerof(b))
    end

    # Check if two UUIDs are equal.
    def self.equals?(a : LibProven::Uuid, b : LibProven::Uuid) : Bool
      LibProven.uuid_equals(pointerof(a), pointerof(b))
    end

    # Validate a UUID string without fully parsing it.
    def self.valid?(input : String) : Bool
      slice = input.to_slice
      LibProven.uuid_is_valid(slice.to_unsafe, slice.size)
    end
  end
end
