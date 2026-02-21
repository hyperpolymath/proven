// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe email validation for Dart.
library;

/// Email validation result with parsed components.
class EmailResult {
  final String? localPart;
  final String? domain;
  final String? error;

  const EmailResult.ok(this.localPart, this.domain) : error = null;

  const EmailResult.error(String e)
      : localPart = null,
        domain = null,
        error = e;

  bool get isOk => error == null;
  bool get isError => error != null;

  String get email => isOk ? '$localPart@$domain' : '';
}

/// Safe email validation and manipulation.
class SafeEmail {
  /// Common disposable email domains.
  static const List<String> disposableDomains = [
    'tempmail.com',
    'throwaway.email',
    'guerrillamail.com',
    'mailinator.com',
    '10minutemail.com',
    'temp-mail.org',
    'fakeinbox.com',
    'trashmail.com',
    'yopmail.com',
    'sharklasers.com',
    'getairmail.com',
    'tempail.com',
    'discard.email',
    'maildrop.cc',
  ];

  /// RFC 5322 local part allowed characters (simplified).
  static final RegExp _localPartPattern = RegExp(
    r'^[a-zA-Z0-9.!#$%&\x27*+/=?^_`{|}~-]+$',
  );

  /// Domain pattern.
  static final RegExp _domainPattern = RegExp(
    r'^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?(\.[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?)*$',
  );

  /// Validate an email address (basic check).
  static bool isValid(String email) {
    return parse(email).isOk;
  }

  /// Parse and validate an email address.
  static EmailResult parse(String email) {
    // Check for empty
    if (email.isEmpty) {
      return const EmailResult.error('empty_email');
    }

    // Check length
    if (email.length > 254) {
      return const EmailResult.error('email_too_long');
    }

    // Check for @ symbol
    final atIndex = email.indexOf('@');
    if (atIndex == -1) {
      return const EmailResult.error('missing_at_symbol');
    }

    // Check for multiple @
    if (email.indexOf('@', atIndex + 1) != -1) {
      return const EmailResult.error('multiple_at_symbols');
    }

    // Split at @
    final localPart = email.substring(0, atIndex);
    final domain = email.substring(atIndex + 1);

    // Validate local part
    if (localPart.isEmpty) {
      return const EmailResult.error('empty_local_part');
    }
    if (localPart.length > 64) {
      return const EmailResult.error('local_part_too_long');
    }
    if (localPart.startsWith('.') || localPart.endsWith('.')) {
      return const EmailResult.error('local_part_dot_position');
    }
    if (localPart.contains('..')) {
      return const EmailResult.error('local_part_consecutive_dots');
    }
    if (!_localPartPattern.hasMatch(localPart)) {
      return const EmailResult.error('invalid_local_part_chars');
    }

    // Validate domain
    if (domain.isEmpty) {
      return const EmailResult.error('empty_domain');
    }
    if (domain.length > 253) {
      return const EmailResult.error('domain_too_long');
    }

    // Domain must have a dot (unless localhost)
    if (!domain.contains('.') && domain.toLowerCase() != 'localhost') {
      return const EmailResult.error('domain_missing_dot');
    }

    // Domain can't start or end with dot or hyphen
    if (domain.startsWith('.') ||
        domain.endsWith('.') ||
        domain.startsWith('-') ||
        domain.endsWith('-')) {
      return const EmailResult.error('invalid_domain_format');
    }

    if (!_domainPattern.hasMatch(domain)) {
      return const EmailResult.error('invalid_domain_chars');
    }

    return EmailResult.ok(localPart, domain);
  }

  /// Extract domain from email.
  static String? getDomain(String email) {
    final result = parse(email);
    return result.isOk ? result.domain : null;
  }

  /// Extract local part from email.
  static String? getLocalPart(String email) {
    final result = parse(email);
    return result.isOk ? result.localPart : null;
  }

  /// Normalize email (lowercase domain, preserve local part case).
  static String? normalize(String email) {
    final result = parse(email);
    if (!result.isOk) return null;

    return '${result.localPart}@${result.domain!.toLowerCase()}';
  }

  /// Fully normalize email (lowercase everything).
  static String? normalizeFull(String email) {
    final result = parse(email);
    if (!result.isOk) return null;

    return '${result.localPart!.toLowerCase()}@${result.domain!.toLowerCase()}';
  }

  /// Check if email is from a disposable service.
  static bool isDisposable(String email) {
    final domain = getDomain(email)?.toLowerCase();
    if (domain == null) return false;

    return disposableDomains.contains(domain);
  }

  /// Check if email is from a specific domain.
  static bool isFromDomain(String email, String expectedDomain) {
    final domain = getDomain(email)?.toLowerCase();
    if (domain == null) return false;

    return domain == expectedDomain.toLowerCase();
  }

  /// Check if email is from one of a list of allowed domains.
  static bool isFromAllowedDomain(String email, List<String> allowedDomains) {
    final domain = getDomain(email)?.toLowerCase();
    if (domain == null) return false;

    return allowedDomains.any((allowed) => allowed.toLowerCase() == domain);
  }

  /// Get the TLD (top-level domain) of an email.
  static String? getTld(String email) {
    final domain = getDomain(email);
    if (domain == null) return null;

    final lastDot = domain.lastIndexOf('.');
    if (lastDot == -1) return null;

    return domain.substring(lastDot + 1).toLowerCase();
  }

  /// Obfuscate email for display (e.g., "u***r@example.com").
  static String? obfuscate(String email) {
    final result = parse(email);
    if (!result.isOk) return null;

    final local = result.localPart!;
    final domain = result.domain!;

    String obfuscatedLocal;
    if (local.length <= 2) {
      obfuscatedLocal = '***';
    } else {
      obfuscatedLocal = '${local[0]}***${local[local.length - 1]}';
    }

    return '$obfuscatedLocal@$domain';
  }

  /// Check if two emails are equivalent (after normalization).
  static bool areEquivalent(String email1, String email2) {
    final norm1 = normalizeFull(email1);
    final norm2 = normalizeFull(email2);

    if (norm1 == null || norm2 == null) return false;
    return norm1 == norm2;
  }
}
