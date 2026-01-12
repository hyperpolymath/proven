// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe phone number validation following E.164.
library;

/// Country calling codes.
enum CountryCode {
  // North America (NANP)
  us(1, 'US', 'United States'),
  ca(1, 'CA', 'Canada'),

  // Russia and CIS
  ru(7, 'RU', 'Russia'),
  kz(7, 'KZ', 'Kazakhstan'),

  // Africa
  eg(20, 'EG', 'Egypt'),
  za(27, 'ZA', 'South Africa'),
  ng(234, 'NG', 'Nigeria'),
  ke(254, 'KE', 'Kenya'),

  // Europe
  fr(33, 'FR', 'France'),
  es(34, 'ES', 'Spain'),
  it(39, 'IT', 'Italy'),
  uk(44, 'GB', 'United Kingdom'),
  de(49, 'DE', 'Germany'),
  pl(48, 'PL', 'Poland'),
  nl(31, 'NL', 'Netherlands'),
  be(32, 'BE', 'Belgium'),
  ch(41, 'CH', 'Switzerland'),
  at(43, 'AT', 'Austria'),
  se(46, 'SE', 'Sweden'),
  no(47, 'NO', 'Norway'),
  dk(45, 'DK', 'Denmark'),
  fi(358, 'FI', 'Finland'),
  ie(353, 'IE', 'Ireland'),
  pt(351, 'PT', 'Portugal'),
  gr(30, 'GR', 'Greece'),
  cz(420, 'CZ', 'Czech Republic'),
  hu(36, 'HU', 'Hungary'),
  ro(40, 'RO', 'Romania'),
  ua(380, 'UA', 'Ukraine'),
  tr(90, 'TR', 'Turkey'),

  // Latin America
  mx(52, 'MX', 'Mexico'),
  br(55, 'BR', 'Brazil'),
  ar(54, 'AR', 'Argentina'),
  cl(56, 'CL', 'Chile'),
  co(57, 'CO', 'Colombia'),
  pe(51, 'PE', 'Peru'),
  ve(58, 'VE', 'Venezuela'),

  // Asia-Pacific
  au(61, 'AU', 'Australia'),
  nz(64, 'NZ', 'New Zealand'),
  jp(81, 'JP', 'Japan'),
  kr(82, 'KR', 'South Korea'),
  cn(86, 'CN', 'China'),
  hk(852, 'HK', 'Hong Kong'),
  tw(886, 'TW', 'Taiwan'),
  sg(65, 'SG', 'Singapore'),
  my(60, 'MY', 'Malaysia'),
  th(66, 'TH', 'Thailand'),
  vn(84, 'VN', 'Vietnam'),
  ph(63, 'PH', 'Philippines'),
  id(62, 'ID', 'Indonesia'),
  inCode(91, 'IN', 'India'),
  pk(92, 'PK', 'Pakistan'),
  bd(880, 'BD', 'Bangladesh'),

  // Middle East
  ae(971, 'AE', 'United Arab Emirates'),
  sa(966, 'SA', 'Saudi Arabia'),
  il(972, 'IL', 'Israel'),
  qa(974, 'QA', 'Qatar'),
  kw(965, 'KW', 'Kuwait'),

  unknown(0, '', 'Unknown');

  final int callingCode;
  final String iso2;
  final String name;

  const CountryCode(this.callingCode, this.iso2, this.name);

  /// Get the digit count of the calling code.
  int get digitCount {
    if (callingCode >= 100) return 3;
    if (callingCode >= 10) return 2;
    return 1;
  }

  /// Parse from numeric value.
  static CountryCode fromCallingCode(int code) {
    for (final country in CountryCode.values) {
      if (country.callingCode == code && country != unknown) {
        return country;
      }
    }
    return unknown;
  }

  /// Parse from ISO 3166-1 alpha-2 code.
  static CountryCode fromIso2(String code) {
    final upperCode = code.toUpperCase();
    for (final country in CountryCode.values) {
      if (country.iso2 == upperCode) {
        return country;
      }
    }
    return unknown;
  }
}

/// Phone number parsing result.
class PhoneResult {
  final PhoneNumber? value;
  final String? error;

  const PhoneResult.ok(PhoneNumber phone)
      : value = phone,
        error = null;

  const PhoneResult.error(String e)
      : value = null,
        error = e;

  bool get isOk => error == null;
  bool get isError => error != null;

  /// Get phone or throw if error.
  PhoneNumber unwrap() {
    if (error != null) {
      throw PhoneException(error!);
    }
    return value!;
  }

  /// Get phone or return default.
  PhoneNumber unwrapOr(PhoneNumber defaultValue) => value ?? defaultValue;
}

/// Exception thrown for phone errors.
class PhoneException implements Exception {
  final String message;
  const PhoneException(this.message);

  @override
  String toString() => 'PhoneException: $message';
}

/// Validated phone number.
class PhoneNumber {
  final CountryCode countryCode;
  final String nationalNumber;

  const PhoneNumber(this.countryCode, this.nationalNumber);

  /// Get total digit count (country code + national number).
  int get digitCount => countryCode.digitCount + nationalNumber.length;

  /// Format in E.164 format (+15551234567).
  String toE164() => '+${countryCode.callingCode}$nationalNumber';

  /// Format in international format (+1 555 123 4567).
  String toInternational() {
    final cc = countryCode.callingCode;
    final nat = nationalNumber;

    if (nat.length <= 4) {
      return '+$cc $nat';
    } else if (nat.length <= 7) {
      return '+$cc ${nat.substring(0, 3)} ${nat.substring(3)}';
    } else if (nat.length == 10) {
      return '+$cc ${nat.substring(0, 3)} ${nat.substring(3, 6)} ${nat.substring(6)}';
    } else if (nat.length > 10) {
      // Generic grouping for longer numbers
      return '+$cc ${nat.substring(0, 3)} ${nat.substring(3, 6)} ${nat.substring(6, nat.length - 4)} ${nat.substring(nat.length - 4)}';
    } else {
      return '+$cc $nat';
    }
  }

  /// Format in national format (without country code).
  String toNational() {
    final nat = nationalNumber;

    if (nat.length == 10) {
      return '(${nat.substring(0, 3)}) ${nat.substring(3, 6)}-${nat.substring(6)}';
    } else if (nat.length == 7) {
      return '${nat.substring(0, 3)}-${nat.substring(3)}';
    }
    return nat;
  }

  /// Format with dashes.
  String toDashed() {
    final nat = nationalNumber;

    if (nat.length == 10) {
      return '+${countryCode.callingCode}-${nat.substring(0, 3)}-${nat.substring(3, 6)}-${nat.substring(6)}';
    }
    return toE164();
  }

  /// Get digits only (without + sign).
  String get digits => '${countryCode.callingCode}$nationalNumber';

  /// Obfuscate phone number for display.
  String obfuscate({int visibleDigits = 4}) {
    if (nationalNumber.length <= visibleDigits) {
      return '+${countryCode.callingCode} ****';
    }

    final hidden = nationalNumber.length - visibleDigits;
    final stars = '*' * hidden;
    final visible = nationalNumber.substring(hidden);
    return '+${countryCode.callingCode} $stars$visible';
  }

  @override
  String toString() => toE164();

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    return other is PhoneNumber &&
        other.countryCode == countryCode &&
        other.nationalNumber == nationalNumber;
  }

  @override
  int get hashCode => Object.hash(countryCode, nationalNumber);
}

/// Safe phone number operations.
class SafePhone {
  /// Parse phone number from string.
  static PhoneResult parse(String input) {
    final trimmed = input.trim();
    if (trimmed.isEmpty) {
      return const PhoneResult.error('Empty phone number');
    }

    // Extract digits only (ignore +, spaces, dashes, parentheses)
    final digits = trimmed.replaceAll(RegExp(r'[^\d]'), '');

    if (digits.length < 7) {
      return const PhoneResult.error('Phone number too short');
    }
    if (digits.length > 15) {
      return const PhoneResult.error('Phone number too long (E.164 max is 15)');
    }

    // Try to parse country code
    final (countryCode, nationalStart) = _parseCountryCode(digits);

    if (countryCode == CountryCode.unknown) {
      return const PhoneResult.error('Unknown or missing country code');
    }

    final nationalNumber = digits.substring(nationalStart);

    if (nationalNumber.length < 4) {
      return const PhoneResult.error('National number too short');
    }

    if (nationalNumber.length > 12) {
      return const PhoneResult.error('National number too long');
    }

    return PhoneResult.ok(PhoneNumber(countryCode, nationalNumber));
  }

  /// Parse phone number with explicit country code.
  static PhoneResult parseWithCountry(String input, CountryCode country) {
    final trimmed = input.trim();
    if (trimmed.isEmpty) {
      return const PhoneResult.error('Empty phone number');
    }

    // Extract digits only
    final digits = trimmed.replaceAll(RegExp(r'[^\d]'), '');

    if (digits.length < 4) {
      return const PhoneResult.error('Phone number too short');
    }

    // Check if number starts with country code
    final ccStr = country.callingCode.toString();
    String nationalNumber;

    if (digits.startsWith(ccStr)) {
      nationalNumber = digits.substring(ccStr.length);
    } else {
      nationalNumber = digits;
    }

    if (nationalNumber.length < 4) {
      return const PhoneResult.error('National number too short');
    }

    if (nationalNumber.length > 12) {
      return const PhoneResult.error('National number too long');
    }

    return PhoneResult.ok(PhoneNumber(country, nationalNumber));
  }

  /// Check if valid phone number.
  static bool isValid(String input) {
    return parse(input).isOk;
  }

  /// Check if valid E.164 format.
  static bool isValidE164(String input) {
    if (!input.startsWith('+')) return false;
    final digits = input.substring(1);
    if (!RegExp(r'^\d+$').hasMatch(digits)) return false;
    if (digits.length < 7 || digits.length > 15) return false;
    return parse(input).isOk;
  }

  /// Extract country code from a phone number string.
  static CountryCode? getCountryCode(String input) {
    final result = parse(input);
    return result.isOk ? result.value!.countryCode : null;
  }

  /// Normalize phone number to E.164 format.
  static String? normalize(String input) {
    final result = parse(input);
    return result.isOk ? result.value!.toE164() : null;
  }

  /// Compare two phone numbers.
  static bool areEqual(String a, String b) {
    final resultA = parse(a);
    final resultB = parse(b);

    if (!resultA.isOk || !resultB.isOk) return false;
    return resultA.value! == resultB.value!;
  }

  /// Get all supported country codes.
  static List<CountryCode> allCountryCodes() {
    return CountryCode.values
        .where((c) => c != CountryCode.unknown)
        .toList();
  }

  /// Get country codes by region.
  static List<CountryCode> byCallingCodePrefix(int prefix) {
    return CountryCode.values.where((c) {
      final ccStr = c.callingCode.toString();
      return ccStr.startsWith(prefix.toString());
    }).toList();
  }

  static (CountryCode, int) _parseCountryCode(String digits) {
    // Try 3-digit codes first, then 2, then 1
    for (final len in [3, 2, 1]) {
      if (digits.length > len) {
        final code = int.tryParse(digits.substring(0, len));
        if (code != null) {
          final country = CountryCode.fromCallingCode(code);
          if (country != CountryCode.unknown) {
            return (country, len);
          }
        }
      }
    }
    return (CountryCode.unknown, 0);
  }
}
