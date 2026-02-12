# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe network operations for IP address validation and classification.
  module SafeNetwork
    # Represents an IPv4 address.
    IPv4 = Struct.new(:a, :b, :c, :d, keyword_init: true) do
      def to_s
        "#{a}.#{b}.#{c}.#{d}"
      end
    end

    class << self
      # Parse an IPv4 address string.
      #
      # @param address [String]
      # @return [IPv4, nil]
      def parse_ipv4(address)
        parts = address.split(".")
        return nil unless parts.length == 4

        octets = parts.map do |part|
          num = Integer(part, 10)
          return nil if num.negative? || num > 255

          num
        rescue ArgumentError
          return nil
        end

        IPv4.new(a: octets[0], b: octets[1], c: octets[2], d: octets[3])
      end

      # Check if a string is a valid IPv4 address.
      #
      # @param address [String]
      # @return [Boolean]
      def valid_ipv4?(address)
        !parse_ipv4(address).nil?
      end

      # Check if an IPv4 address is in a private range.
      #
      # @param address [String]
      # @return [Boolean]
      def private?(address)
        ip = parse_ipv4(address)
        return false unless ip

        # 10.0.0.0/8
        return true if ip.a == 10

        # 172.16.0.0/12
        return true if ip.a == 172 && ip.b >= 16 && ip.b <= 31

        # 192.168.0.0/16
        return true if ip.a == 192 && ip.b == 168

        false
      end

      # Check if an IPv4 address is a loopback address (127.0.0.0/8).
      #
      # @param address [String]
      # @return [Boolean]
      def loopback?(address)
        ip = parse_ipv4(address)
        return false unless ip

        ip.a == 127
      end

      # Check if an IPv4 address is public (not private or loopback).
      #
      # @param address [String]
      # @return [Boolean]
      def public?(address)
        valid_ipv4?(address) && !private?(address) && !loopback?(address)
      end

      # Format an IPv4 address as a string.
      #
      # @param ip [IPv4]
      # @return [String]
      def format_ipv4(ip)
        ip.to_s
      end
    end
  end
end
