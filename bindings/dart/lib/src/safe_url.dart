// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe URL parsing and validation for Dart.
library;

/// Result of a URL operation.
class UrlResult {
  final ParsedUrl? url;
  final String? error;

  const UrlResult.ok(ParsedUrl u)
      : url = u,
        error = null;

  const UrlResult.error(String e)
      : url = null,
        error = e;

  bool get isOk => error == null;

  ParsedUrl unwrap() {
    if (error != null) {
      throw UrlException(error!);
    }
    return url!;
  }
}

/// Exception thrown on URL errors.
class UrlException implements Exception {
  final String message;
  const UrlException(this.message);

  @override
  String toString() => 'UrlException: $message';
}

/// A parsed URL with validated components.
class ParsedUrl {
  final String scheme;
  final String? username;
  final String? password;
  final String host;
  final int? port;
  final String path;
  final String? query;
  final String? fragment;

  const ParsedUrl({
    required this.scheme,
    this.username,
    this.password,
    required this.host,
    this.port,
    required this.path,
    this.query,
    this.fragment,
  });

  /// Get the origin (scheme://host:port).
  String get origin {
    final portSuffix = port != null ? ':$port' : '';
    return '$scheme://$host$portSuffix';
  }

  /// Get the full URL string.
  String get href {
    final buffer = StringBuffer('$scheme://');
    if (username != null) {
      buffer.write(username);
      if (password != null) {
        buffer.write(':$password');
      }
      buffer.write('@');
    }
    buffer.write(host);
    if (port != null) {
      buffer.write(':$port');
    }
    buffer.write(path.isEmpty ? '/' : path);
    if (query != null) {
      buffer.write('?$query');
    }
    if (fragment != null) {
      buffer.write('#$fragment');
    }
    return buffer.toString();
  }

  /// Check if URL uses a secure scheme.
  bool get isSecure => scheme == 'https' || scheme == 'wss';

  /// Get the default port for this scheme.
  int? get defaultPort {
    switch (scheme) {
      case 'http':
        return 80;
      case 'https':
        return 443;
      case 'ftp':
        return 21;
      case 'ws':
        return 80;
      case 'wss':
        return 443;
      default:
        return null;
    }
  }

  /// Get the effective port (explicit or default).
  int? get effectivePort => port ?? defaultPort;

  @override
  String toString() => href;
}

/// Safe URL operations.
class SafeUrl {
  /// Allowed URL schemes.
  static const Set<String> allowedSchemes = {
    'http',
    'https',
    'ftp',
    'ws',
    'wss',
    'mailto',
    'tel',
  };

  /// Parse a URL string.
  static UrlResult parse(String urlString) {
    if (urlString.isEmpty) {
      return const UrlResult.error('Empty URL');
    }

    try {
      final uri = Uri.parse(urlString);

      if (uri.scheme.isEmpty) {
        return const UrlResult.error('Missing scheme');
      }

      if (!allowedSchemes.contains(uri.scheme.toLowerCase())) {
        return UrlResult.error('Disallowed scheme: ${uri.scheme}');
      }

      if (uri.host.isEmpty && uri.scheme != 'mailto' && uri.scheme != 'tel') {
        return const UrlResult.error('Missing host');
      }

      return UrlResult.ok(ParsedUrl(
        scheme: uri.scheme.toLowerCase(),
        username: uri.userInfo.contains(':')
            ? uri.userInfo.split(':')[0]
            : (uri.userInfo.isNotEmpty ? uri.userInfo : null),
        password: uri.userInfo.contains(':')
            ? uri.userInfo.split(':').skip(1).join(':')
            : null,
        host: uri.host.toLowerCase(),
        port: uri.hasPort ? uri.port : null,
        path: uri.path,
        query: uri.hasQuery ? uri.query : null,
        fragment: uri.hasFragment ? uri.fragment : null,
      ));
    } catch (e) {
      return UrlResult.error('Invalid URL: $e');
    }
  }

  /// Check if a URL string is valid.
  static bool isValid(String urlString) {
    return parse(urlString).isOk;
  }

  /// Check if a URL is using HTTPS.
  static bool isHttps(String urlString) {
    final result = parse(urlString);
    return result.isOk && result.url!.scheme == 'https';
  }

  /// Normalize a URL (lowercase scheme and host, remove default port).
  static String? normalize(String urlString) {
    final result = parse(urlString);
    if (!result.isOk) return null;

    final url = result.url!;
    final buffer = StringBuffer('${url.scheme}://');

    if (url.username != null) {
      buffer.write(url.username);
      if (url.password != null) {
        buffer.write(':${url.password}');
      }
      buffer.write('@');
    }

    buffer.write(url.host);

    // Only include port if it's not the default
    if (url.port != null && url.port != url.defaultPort) {
      buffer.write(':${url.port}');
    }

    buffer.write(url.path.isEmpty ? '/' : url.path);

    if (url.query != null) {
      buffer.write('?${url.query}');
    }

    if (url.fragment != null) {
      buffer.write('#${url.fragment}');
    }

    return buffer.toString();
  }

  /// Extract the domain from a URL.
  static String? extractDomain(String urlString) {
    final result = parse(urlString);
    return result.isOk ? result.url!.host : null;
  }

  /// Check if URL points to localhost.
  static bool isLocalhost(String urlString) {
    final result = parse(urlString);
    if (!result.isOk) return false;

    final host = result.url!.host;
    return host == 'localhost' ||
        host == '127.0.0.1' ||
        host == '::1' ||
        host.startsWith('192.168.') ||
        host.startsWith('10.') ||
        (host.startsWith('172.') && _isPrivate172(host));
  }

  static bool _isPrivate172(String host) {
    final parts = host.split('.');
    if (parts.length != 4) return false;
    final second = int.tryParse(parts[1]);
    return second != null && second >= 16 && second <= 31;
  }

  /// Join a base URL with a relative path.
  static String? join(String base, String relative) {
    try {
      final baseUri = Uri.parse(base);
      final resolved = baseUri.resolve(relative);
      return resolved.toString();
    } catch (e) {
      return null;
    }
  }

  /// URL-encode a string.
  static String encode(String value) {
    return Uri.encodeComponent(value);
  }

  /// URL-decode a string.
  static String? decode(String value) {
    try {
      return Uri.decodeComponent(value);
    } catch (e) {
      return null;
    }
  }

  /// Parse query parameters from a URL.
  static Map<String, String> parseQueryParams(String urlString) {
    final result = parse(urlString);
    if (!result.isOk || result.url!.query == null) {
      return {};
    }

    final params = <String, String>{};
    for (final part in result.url!.query!.split('&')) {
      final idx = part.indexOf('=');
      if (idx > 0) {
        final key = decode(part.substring(0, idx));
        final value = decode(part.substring(idx + 1));
        if (key != null && value != null) {
          params[key] = value;
        }
      }
    }
    return params;
  }

  /// Build a query string from parameters.
  static String buildQueryString(Map<String, String> params) {
    return params.entries.map((e) => '${encode(e.key)}=${encode(e.value)}').join('&');
  }
}
