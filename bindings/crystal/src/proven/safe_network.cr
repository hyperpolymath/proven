# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # IP address classification.
  enum IpClassification
    Loopback
    Private
    Reserved
    Public
    Invalid
  end

  # Parsed IPv4 address.
  struct IPv4Address
    getter a : UInt8
    getter b : UInt8
    getter c : UInt8
    getter d : UInt8

    def initialize(@a : UInt8, @b : UInt8, @c : UInt8, @d : UInt8)
    end

    def to_s : String
      "#{@a}.#{@b}.#{@c}.#{@d}"
    end

    def to_int : UInt32
      (@a.to_u32 << 24) | (@b.to_u32 << 16) | (@c.to_u32 << 8) | @d.to_u32
    end
  end

  # Safe network validation and operations.
  module SafeNetwork
    # Parse IPv4 address string.
    def self.parse_ipv4(address : String) : IPv4Address?
      parts = address.split('.')
      return nil unless parts.size == 4

      octets = parts.map do |part|
        return nil if part.empty?
        return nil unless part.matches?(/^\d+$/)
        return nil if part.size > 1 && part.starts_with?('0')

        value = part.to_i?
        return nil if value.nil?
        return nil if value < 0 || value > 255
        value.to_u8
      end

      IPv4Address.new(octets[0], octets[1], octets[2], octets[3])
    end

    # Check if IPv4 is a loopback address.
    def self.loopback?(ip : IPv4Address) : Bool
      ip.a == 127
    end

    # Check if IPv4 is a private address (RFC 1918).
    def self.private?(ip : IPv4Address) : Bool
      ip.a == 10 ||
        (ip.a == 172 && ip.b >= 16 && ip.b <= 31) ||
        (ip.a == 192 && ip.b == 168)
    end

    # Check if IPv4 is a reserved address.
    def self.reserved?(ip : IPv4Address) : Bool
      ip.a == 0 ||
        (ip.a == 100 && ip.b >= 64 && ip.b <= 127) ||
        (ip.a == 169 && ip.b == 254) ||
        (ip.a == 192 && ip.b == 0 && ip.c == 0) ||
        (ip.a == 192 && ip.b == 0 && ip.c == 2) ||
        (ip.a == 198 && ip.b == 51 && ip.c == 100) ||
        (ip.a == 203 && ip.b == 0 && ip.c == 113) ||
        (ip.a >= 224 && ip.a <= 239) ||
        ip.a >= 240
    end

    # Check if IPv4 is a public address.
    def self.public?(ip : IPv4Address) : Bool
      !loopback?(ip) && !private?(ip) && !reserved?(ip)
    end

    # Classify an IPv4 address.
    def self.classify(ip : IPv4Address) : IpClassification
      return IpClassification::Loopback if loopback?(ip)
      return IpClassification::Private if private?(ip)
      return IpClassification::Reserved if reserved?(ip)
      IpClassification::Public
    end

    # Classify IPv4 from string.
    def self.classify_string(address : String) : IpClassification
      ip = parse_ipv4(address)
      return IpClassification::Invalid if ip.nil?
      classify(ip)
    end

    # Check if IP is in CIDR range.
    def self.in_range?(ip : IPv4Address, network : IPv4Address, prefix_length : Int32) : Bool
      return false if prefix_length < 0 || prefix_length > 32

      mask = if prefix_length == 0
        0_u32
      else
        0xFFFFFFFF_u32 << (32 - prefix_length)
      end

      (ip.to_int & mask) == (network.to_int & mask)
    end

    # Check if string is valid IPv4.
    def self.valid_ipv4?(address : String) : Bool
      !parse_ipv4(address).nil?
    end

    # Check if port number is valid.
    def self.valid_port?(port : Int32) : Bool
      port >= 1 && port <= 65535
    end

    # Check if port is privileged.
    def self.privileged_port?(port : Int32) : Bool
      port >= 1 && port < 1024
    end

    # Check if hostname is valid.
    def self.valid_hostname?(hostname : String) : Bool
      return false if hostname.empty? || hostname.size > 253

      labels = hostname.split('.')
      labels.all? do |label|
        !label.empty? &&
          label.size <= 63 &&
          label.matches?(/^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?$/)
      end
    end

    # Check if URL is valid.
    def self.valid_url?(url : String) : Bool
      begin
        uri = URI.parse(url)
        uri.scheme == "http" || uri.scheme == "https"
      rescue
        false
      end
    end

    # Check if URL host is a private IP (SSRF protection).
    def self.private_url?(url : String) : Bool
      begin
        uri = URI.parse(url)
        host = uri.host
        return false if host.nil?

        return true if host == "localhost" || host == "127.0.0.1" || host == "::1"

        ip = parse_ipv4(host)
        return false if ip.nil?

        private?(ip) || loopback?(ip) || reserved?(ip)
      rescue
        false
      end
    end
  end
end

require "uri"
