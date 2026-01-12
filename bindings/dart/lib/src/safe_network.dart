// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe network operations for Dart with IP validation.
library;

import 'dart:io';

/// Classification of IP addresses.
enum IpClassification {
  loopback,
  private,
  reserved,
  public,
  invalid,
}

/// Parsed IPv4 address.
class IPv4Address {
  final int a;
  final int b;
  final int c;
  final int d;

  const IPv4Address(this.a, this.b, this.c, this.d);

  /// Parse an IPv4 address string.
  static IPv4Address? parse(String address) {
    // Check format
    final parts = address.split('.');
    if (parts.length != 4) return null;

    final octets = <int>[];
    for (final part in parts) {
      // Check for empty parts
      if (part.isEmpty) return null;

      // Check for leading zeros (invalid in strict parsing)
      if (part.length > 1 && part.startsWith('0')) return null;

      // Check for non-digit characters
      if (!RegExp(r'^[0-9]+$').hasMatch(part)) return null;

      // Parse and validate range
      final octet = int.tryParse(part);
      if (octet == null || octet < 0 || octet > 255) return null;

      octets.add(octet);
    }

    return IPv4Address(octets[0], octets[1], octets[2], octets[3]);
  }

  /// Convert to integer representation.
  int toInt() => (a << 24) | (b << 16) | (c << 8) | d;

  /// Check if this is a loopback address (127.0.0.0/8).
  bool get isLoopback => a == 127;

  /// Check if this is a private address (RFC 1918).
  bool get isPrivate {
    // 10.0.0.0/8
    if (a == 10) return true;

    // 172.16.0.0/12
    if (a == 172 && b >= 16 && b <= 31) return true;

    // 192.168.0.0/16
    if (a == 192 && b == 168) return true;

    return false;
  }

  /// Check if this is a reserved address.
  bool get isReserved {
    // 0.0.0.0/8
    if (a == 0) return true;

    // 100.64.0.0/10 - CGNAT
    if (a == 100 && b >= 64 && b <= 127) return true;

    // 169.254.0.0/16 - Link-local
    if (a == 169 && b == 254) return true;

    // 192.0.0.0/24 - IETF
    if (a == 192 && b == 0 && c == 0) return true;

    // 192.0.2.0/24 - TEST-NET-1
    if (a == 192 && b == 0 && c == 2) return true;

    // 198.51.100.0/24 - TEST-NET-2
    if (a == 198 && b == 51 && c == 100) return true;

    // 203.0.113.0/24 - TEST-NET-3
    if (a == 203 && b == 0 && c == 113) return true;

    // 224.0.0.0/4 - Multicast
    if (a >= 224 && a <= 239) return true;

    // 240.0.0.0/4 - Reserved
    if (a >= 240) return true;

    return false;
  }

  /// Check if this is a public address.
  bool get isPublic => !isLoopback && !isPrivate && !isReserved;

  /// Get classification of this address.
  IpClassification get classification {
    if (isLoopback) return IpClassification.loopback;
    if (isPrivate) return IpClassification.private;
    if (isReserved) return IpClassification.reserved;
    return IpClassification.public;
  }

  /// Check if address is in a CIDR range.
  bool isInRange(IPv4Address network, int prefixLength) {
    if (prefixLength < 0 || prefixLength > 32) return false;

    final mask = prefixLength == 0 ? 0 : (0xFFFFFFFF << (32 - prefixLength));
    return (toInt() & mask) == (network.toInt() & mask);
  }

  @override
  String toString() => '$a.$b.$c.$d';

  @override
  bool operator ==(Object other) =>
      other is IPv4Address && a == other.a && b == other.b && c == other.c && d == other.d;

  @override
  int get hashCode => Object.hash(a, b, c, d);
}

/// Parsed IPv6 address.
class IPv6Address {
  final List<int> groups;

  const IPv6Address._(this.groups);

  /// Parse an IPv6 address string.
  static IPv6Address? parse(String address) {
    try {
      final inet = InternetAddress.tryParse(address);
      if (inet == null || inet.type != InternetAddressType.IPv6) {
        return null;
      }

      final rawBytes = inet.rawAddress;
      if (rawBytes.length != 16) return null;

      final groups = <int>[];
      for (var i = 0; i < 16; i += 2) {
        groups.add((rawBytes[i] << 8) | rawBytes[i + 1]);
      }

      return IPv6Address._(groups);
    } catch (_) {
      return null;
    }
  }

  /// Check if this is a loopback address (::1).
  bool get isLoopback {
    for (var i = 0; i < 7; i++) {
      if (groups[i] != 0) return false;
    }
    return groups[7] == 1;
  }

  /// Check if this is a link-local address (fe80::/10).
  bool get isLinkLocal => (groups[0] & 0xFFC0) == 0xFE80;

  /// Check if this is a site-local address (fec0::/10, deprecated).
  bool get isSiteLocal => (groups[0] & 0xFFC0) == 0xFEC0;

  /// Check if this is a unique local address (fc00::/7).
  bool get isUniqueLocal => (groups[0] & 0xFE00) == 0xFC00;

  /// Check if this is a multicast address (ff00::/8).
  bool get isMulticast => (groups[0] & 0xFF00) == 0xFF00;

  @override
  String toString() {
    return groups.map((g) => g.toRadixString(16)).join(':');
  }
}

/// Safe network validation and operations.
class SafeNetwork {
  /// Check if string is a valid IPv4 address.
  static bool isValidIPv4(String address) => IPv4Address.parse(address) != null;

  /// Check if string is a valid IPv6 address.
  static bool isValidIPv6(String address) => IPv6Address.parse(address) != null;

  /// Check if string is any valid IP address.
  static bool isValidIP(String address) => isValidIPv4(address) || isValidIPv6(address);

  /// Parse an IPv4 address.
  static IPv4Address? parseIPv4(String address) => IPv4Address.parse(address);

  /// Parse an IPv6 address.
  static IPv6Address? parseIPv6(String address) => IPv6Address.parse(address);

  /// Classify an IPv4 address.
  static IpClassification classifyIPv4(String address) {
    final ip = IPv4Address.parse(address);
    if (ip == null) return IpClassification.invalid;
    return ip.classification;
  }

  /// Check if port number is valid (1-65535).
  static bool isValidPort(int port) => port >= 1 && port <= 65535;

  /// Check if port is privileged (< 1024).
  static bool isPrivilegedPort(int port) => port >= 1 && port < 1024;

  /// Check if host:port string is valid.
  static bool isValidHostPort(String hostPort) {
    final lastColon = hostPort.lastIndexOf(':');
    if (lastColon == -1) return false;

    final host = hostPort.substring(0, lastColon);
    final portStr = hostPort.substring(lastColon + 1);

    final port = int.tryParse(portStr);
    if (port == null || !isValidPort(port)) return false;

    return isValidIP(host) || _isValidHostname(host);
  }

  /// Check if string is a valid hostname.
  static bool _isValidHostname(String hostname) {
    if (hostname.isEmpty || hostname.length > 253) return false;

    final labels = hostname.split('.');
    for (final label in labels) {
      if (label.isEmpty || label.length > 63) return false;
      if (!RegExp(r'^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?$').hasMatch(label)) {
        return false;
      }
    }

    return true;
  }

  /// Check if string is a valid URL.
  static bool isValidUrl(String url) {
    try {
      final uri = Uri.parse(url);
      return uri.hasScheme && (uri.scheme == 'http' || uri.scheme == 'https');
    } catch (_) {
      return false;
    }
  }

  /// Parse a URL safely.
  static Uri? parseUrl(String url) {
    try {
      final uri = Uri.parse(url);
      if (!uri.hasScheme) return null;
      return uri;
    } catch (_) {
      return null;
    }
  }

  /// Check if URL host is a private IP (SSRF protection).
  static bool isPrivateUrl(String url) {
    final uri = parseUrl(url);
    if (uri == null) return false;

    final host = uri.host;

    // Check for localhost
    if (host == 'localhost' || host == '127.0.0.1' || host == '::1') {
      return true;
    }

    // Check IPv4
    final ipv4 = IPv4Address.parse(host);
    if (ipv4 != null) {
      return ipv4.isPrivate || ipv4.isLoopback || ipv4.isReserved;
    }

    // Check IPv6
    final ipv6 = IPv6Address.parse(host);
    if (ipv6 != null) {
      return ipv6.isLoopback || ipv6.isLinkLocal || ipv6.isUniqueLocal;
    }

    return false;
  }

  /// Format IPv4 from octets.
  static String formatIPv4(int a, int b, int c, int d) => '$a.$b.$c.$d';
}
