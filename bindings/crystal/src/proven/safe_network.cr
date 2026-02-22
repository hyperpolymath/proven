# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeNetwork -- thin FFI wrapper around libproven's network operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeNetwork
    # Parse an IPv4 address string.  Returns the four octets or nil on failure.
    def self.parse_ipv4(address : String) : StaticArray(UInt8, 4)?
      slice = address.to_slice
      result = LibProven.proven_network_parse_ipv4(slice.to_unsafe, slice.size)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      result.address.octets
    end

    # Check if an IPv4 address is private (RFC 1918).
    def self.private?(address : String) : Bool
      slice = address.to_slice
      result = LibProven.proven_network_parse_ipv4(slice.to_unsafe, slice.size)
      return false unless result.status == LibProven::ProvenStatus::Ok
      LibProven.proven_network_ipv4_is_private(result.address)
    end

    # Check if an IPv4 address is loopback (127.0.0.0/8).
    def self.loopback?(address : String) : Bool
      slice = address.to_slice
      result = LibProven.proven_network_parse_ipv4(slice.to_unsafe, slice.size)
      return false unless result.status == LibProven::ProvenStatus::Ok
      LibProven.proven_network_ipv4_is_loopback(result.address)
    end

    # Check if an IPv4 address string is valid.
    def self.valid_ipv4?(address : String) : Bool
      slice = address.to_slice
      result = LibProven.proven_network_parse_ipv4(slice.to_unsafe, slice.size)
      result.status == LibProven::ProvenStatus::Ok
    end
  end
end
