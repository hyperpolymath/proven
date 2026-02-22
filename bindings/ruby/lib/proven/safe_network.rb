# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe network operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeNetwork
    # Represents a parsed IPv4 address (4 octets).
    IPv4 = Struct.new(:a, :b, :c, :d, keyword_init: true) do
      def to_s
        "#{a}.#{b}.#{c}.#{d}"
      end
    end

    class << self
      # Parse an IPv4 address string.
      # Returns nil on invalid input.
      #
      # @param address [String]
      # @return [IPv4, nil]
      def parse_ipv4(address)
        return nil if address.nil?
        ptr, len = FFI.str_to_ptr(address)

        # proven_network_parse_ipv4 returns IPv4Result { status, address: { octets: [4]u8 } }
        # Total 8 bytes: i32 status + 4xu8 octets
        buf = Fiddle::Pointer.malloc(8, Fiddle::RUBY_FREE)
        sret_types = [Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T]
        sret_args = [buf, ptr, len]

        fn = Fiddle::Function.new(
          FFI.handler["proven_network_parse_ipv4"],
          sret_types,
          Fiddle::TYPE_VOID,
          need_gvl: true
        )
        fn.call(*sret_args)

        packed = buf.to_str(8)
        status = packed[0, 4].unpack1("l!")
        return nil unless status == FFI::STATUS_OK

        octets = packed[4, 4].unpack("C4")
        IPv4.new(a: octets[0], b: octets[1], c: octets[2], d: octets[3])
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Check if a string is a valid IPv4 address.
      #
      # @param address [String]
      # @return [Boolean]
      def valid_ipv4?(address)
        !parse_ipv4(address).nil?
      end

      # Check if an IPv4 address is in a private range (RFC 1918).
      #
      # @param address [String]
      # @return [Boolean, nil]
      def private?(address)
        ip = parse_ipv4(address)
        return nil if ip.nil?

        # Pack octets into the IPv4Address struct (4 bytes)
        ipv4_bytes = [ip.a, ip.b, ip.c, ip.d].pack("C4")
        ipv4_ptr = Fiddle::Pointer.to_ptr(ipv4_bytes)

        # proven_network_ipv4_is_private takes IPv4Address by value (4 bytes = i32)
        packed_addr = (ip.a << 24) | (ip.b << 16) | (ip.c << 8) | ip.d
        FFI.invoke_bool(
          "proven_network_ipv4_is_private",
          [Fiddle::TYPE_INT],
          [packed_addr]
        )
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Check if an IPv4 address is a loopback address (127.0.0.0/8).
      #
      # @param address [String]
      # @return [Boolean, nil]
      def loopback?(address)
        ip = parse_ipv4(address)
        return nil if ip.nil?

        packed_addr = (ip.a << 24) | (ip.b << 16) | (ip.c << 8) | ip.d
        FFI.invoke_bool(
          "proven_network_ipv4_is_loopback",
          [Fiddle::TYPE_INT],
          [packed_addr]
        )
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Check if an IPv4 address is public (not private or loopback).
      #
      # @param address [String]
      # @return [Boolean, nil]
      def public?(address)
        return nil unless valid_ipv4?(address)
        priv = private?(address)
        loop = loopback?(address)
        return nil if priv.nil? || loop.nil?
        !priv && !loop
      end
    end
  end
end
