// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe currency operations with type-safe monetary values.
library;

/// ISO 4217 currency codes.
enum CurrencyCode {
  // Major world currencies
  usd('USD', '\$', 'US Dollar', 2),
  eur('EUR', '\u20AC', 'Euro', 2),
  gbp('GBP', '\u00A3', 'British Pound', 2),
  jpy('JPY', '\u00A5', 'Japanese Yen', 0),
  chf('CHF', 'Fr', 'Swiss Franc', 2),
  cad('CAD', 'C\$', 'Canadian Dollar', 2),
  aud('AUD', 'A\$', 'Australian Dollar', 2),
  nzd('NZD', 'NZ\$', 'New Zealand Dollar', 2),
  cny('CNY', '\u00A5', 'Chinese Yuan', 2),
  inr('INR', '\u20B9', 'Indian Rupee', 2),

  // Americas
  brl('BRL', 'R\$', 'Brazilian Real', 2),
  mxn('MXN', 'Mex\$', 'Mexican Peso', 2),
  clp('CLP', 'CL\$', 'Chilean Peso', 0),
  cop('COP', 'CO\$', 'Colombian Peso', 2),
  pen('PEN', 'S/', 'Peruvian Sol', 2),
  ars('ARS', 'AR\$', 'Argentine Peso', 2),

  // Asia-Pacific
  krw('KRW', '\u20A9', 'South Korean Won', 0),
  sgd('SGD', 'S\$', 'Singapore Dollar', 2),
  hkd('HKD', 'HK\$', 'Hong Kong Dollar', 2),
  thb('THB', '\u0E3F', 'Thai Baht', 2),
  myr('MYR', 'RM', 'Malaysian Ringgit', 2),
  idr('IDR', 'Rp', 'Indonesian Rupiah', 2),
  php('PHP', '\u20B1', 'Philippine Peso', 2),
  vnd('VND', '\u20AB', 'Vietnamese Dong', 0),

  // Europe
  sek('SEK', 'kr', 'Swedish Krona', 2),
  nok('NOK', 'kr', 'Norwegian Krone', 2),
  dkk('DKK', 'kr', 'Danish Krone', 2),
  pln('PLN', 'z\u0142', 'Polish Zloty', 2),
  czk('CZK', 'K\u010D', 'Czech Koruna', 2),
  huf('HUF', 'Ft', 'Hungarian Forint', 2),
  ron('RON', 'lei', 'Romanian Leu', 2),
  bgn('BGN', 'лв', 'Bulgarian Lev', 2),
  hrk('HRK', 'kn', 'Croatian Kuna', 2),
  isk('ISK', 'kr', 'Icelandic Krona', 0),
  rub('RUB', '\u20BD', 'Russian Ruble', 2),
  trY('TRY', '\u20BA', 'Turkish Lira', 2),

  // Middle East & Africa
  aed('AED', 'د.إ', 'UAE Dirham', 2),
  sar('SAR', '﷼', 'Saudi Riyal', 2),
  ils('ILS', '\u20AA', 'Israeli Shekel', 2),
  zar('ZAR', 'R', 'South African Rand', 2),

  // Cryptocurrencies
  btc('BTC', '\u20BF', 'Bitcoin', 8),
  eth('ETH', '\u039E', 'Ethereum', 8);

  final String code;
  final String symbol;
  final String name;
  final int decimals;

  const CurrencyCode(this.code, this.symbol, this.name, this.decimals);

  /// Get the minor unit multiplier.
  int get minorUnitMultiplier {
    var multiplier = 1;
    for (var i = 0; i < decimals; i++) {
      multiplier *= 10;
    }
    return multiplier;
  }

  /// Parse currency code from string.
  static CurrencyCode? fromString(String code) {
    final upperCode = code.toUpperCase();
    for (final currency in CurrencyCode.values) {
      if (currency.code == upperCode) {
        return currency;
      }
    }
    return null;
  }
}

/// Currency parsing result.
class CurrencyResult {
  final Money? value;
  final String? error;

  const CurrencyResult.ok(Money money)
      : value = money,
        error = null;

  const CurrencyResult.error(String e)
      : value = null,
        error = e;

  bool get isOk => error == null;
  bool get isError => error != null;

  /// Get money or throw if error.
  Money unwrap() {
    if (error != null) {
      throw CurrencyException(error!);
    }
    return value!;
  }

  /// Get money or return default.
  Money unwrapOr(Money defaultValue) => value ?? defaultValue;
}

/// Exception thrown for currency errors.
class CurrencyException implements Exception {
  final String message;
  const CurrencyException(this.message);

  @override
  String toString() => 'CurrencyException: $message';
}

/// Type-safe monetary value.
class Money implements Comparable<Money> {
  /// Amount in minor units (cents, satoshis, etc.)
  final int minorUnits;

  /// Currency code.
  final CurrencyCode currency;

  /// Create from minor units.
  const Money.fromMinor(this.minorUnits, this.currency);

  /// Create from major units.
  factory Money.fromMajor(int majorUnits, CurrencyCode currency) {
    return Money.fromMinor(
      majorUnits * currency.minorUnitMultiplier,
      currency,
    );
  }

  /// Create from decimal string.
  factory Money.fromDecimal(String decimal, CurrencyCode currency) {
    final trimmed = decimal.trim();
    if (trimmed.isEmpty) {
      throw const CurrencyException('Empty decimal string');
    }

    final isNegative = trimmed.startsWith('-');
    final absValue = isNegative ? trimmed.substring(1) : trimmed;

    final parts = absValue.split('.');
    if (parts.length > 2) {
      throw const CurrencyException('Invalid decimal format');
    }

    final majorPart = int.tryParse(parts[0]);
    if (majorPart == null) {
      throw const CurrencyException('Invalid major units');
    }

    var minorPart = 0;
    if (parts.length == 2) {
      var minorStr = parts[1];
      // Pad or truncate to match currency decimals
      if (minorStr.length < currency.decimals) {
        minorStr = minorStr.padRight(currency.decimals, '0');
      } else if (minorStr.length > currency.decimals) {
        minorStr = minorStr.substring(0, currency.decimals);
      }
      minorPart = int.tryParse(minorStr) ?? 0;
    }

    var totalMinor = majorPart * currency.minorUnitMultiplier + minorPart;
    if (isNegative) {
      totalMinor = -totalMinor;
    }

    return Money.fromMinor(totalMinor, currency);
  }

  /// Create zero amount.
  factory Money.zero(CurrencyCode currency) {
    return Money.fromMinor(0, currency);
  }

  /// Get major units (truncated).
  int get majorUnits => minorUnits ~/ currency.minorUnitMultiplier;

  /// Get the fractional minor units.
  int get fractionalMinorUnits =>
      minorUnits.abs() % currency.minorUnitMultiplier;

  /// Check if zero.
  bool get isZero => minorUnits == 0;

  /// Check if positive.
  bool get isPositive => minorUnits > 0;

  /// Check if negative.
  bool get isNegative => minorUnits < 0;

  /// Add two monetary values.
  CurrencyResult add(Money other) {
    if (currency != other.currency) {
      return const CurrencyResult.error('Currency mismatch');
    }
    return CurrencyResult.ok(
      Money.fromMinor(minorUnits + other.minorUnits, currency),
    );
  }

  /// Subtract two monetary values.
  CurrencyResult subtract(Money other) {
    if (currency != other.currency) {
      return const CurrencyResult.error('Currency mismatch');
    }
    return CurrencyResult.ok(
      Money.fromMinor(minorUnits - other.minorUnits, currency),
    );
  }

  /// Multiply by scalar.
  Money multiply(int scalar) {
    return Money.fromMinor(minorUnits * scalar, currency);
  }

  /// Multiply by decimal factor.
  Money multiplyDecimal(double factor) {
    return Money.fromMinor((minorUnits * factor).round(), currency);
  }

  /// Divide by scalar.
  CurrencyResult divide(int divisor) {
    if (divisor == 0) {
      return const CurrencyResult.error('Division by zero');
    }
    return CurrencyResult.ok(
      Money.fromMinor(minorUnits ~/ divisor, currency),
    );
  }

  /// Get absolute value.
  Money abs() {
    return Money.fromMinor(minorUnits.abs(), currency);
  }

  /// Negate value.
  Money negate() {
    return Money.fromMinor(-minorUnits, currency);
  }

  /// Allocate amount across parts with remainder distribution.
  List<Money> allocate(List<int> ratios) {
    if (ratios.isEmpty) {
      throw const CurrencyException('Empty ratios');
    }

    final total = ratios.fold<int>(0, (sum, r) => sum + r);
    if (total == 0) {
      throw const CurrencyException('Ratios sum to zero');
    }

    final results = <Money>[];
    var remainder = minorUnits;

    for (var i = 0; i < ratios.length; i++) {
      final share = (minorUnits * ratios[i]) ~/ total;
      results.add(Money.fromMinor(share, currency));
      remainder -= share;
    }

    // Distribute remainder
    for (var i = 0; remainder > 0 && i < results.length; i++) {
      results[i] = Money.fromMinor(results[i].minorUnits + 1, currency);
      remainder--;
    }

    return results;
  }

  /// Split into equal parts.
  List<Money> split(int parts) {
    if (parts <= 0) {
      throw const CurrencyException('Parts must be positive');
    }

    final share = minorUnits ~/ parts;
    var remainder = minorUnits % parts;

    final results = <Money>[];
    for (var i = 0; i < parts; i++) {
      var amount = share;
      if (remainder > 0) {
        amount++;
        remainder--;
      }
      results.add(Money.fromMinor(amount, currency));
    }

    return results;
  }

  @override
  int compareTo(Money other) {
    if (currency != other.currency) {
      throw const CurrencyException('Cannot compare different currencies');
    }
    return minorUnits.compareTo(other.minorUnits);
  }

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    return other is Money &&
        other.minorUnits == minorUnits &&
        other.currency == currency;
  }

  @override
  int get hashCode => Object.hash(minorUnits, currency);

  /// Format as string with symbol.
  @override
  String toString() {
    final absMinor = minorUnits.abs();
    final major = absMinor ~/ currency.minorUnitMultiplier;
    final fractional = absMinor % currency.minorUnitMultiplier;
    final sign = minorUnits < 0 ? '-' : '';

    if (currency.decimals == 0) {
      return '$sign${currency.symbol}$major';
    }

    final fractionalStr =
        fractional.toString().padLeft(currency.decimals, '0');
    return '$sign${currency.symbol}$major.$fractionalStr';
  }

  /// Format without symbol.
  String toDecimalString() {
    final absMinor = minorUnits.abs();
    final major = absMinor ~/ currency.minorUnitMultiplier;
    final fractional = absMinor % currency.minorUnitMultiplier;
    final sign = minorUnits < 0 ? '-' : '';

    if (currency.decimals == 0) {
      return '$sign$major';
    }

    final fractionalStr =
        fractional.toString().padLeft(currency.decimals, '0');
    return '$sign$major.$fractionalStr';
  }

  /// Format with currency code.
  String toCodeString() {
    return '${toDecimalString()} ${currency.code}';
  }

  /// Format with thousands separators.
  String toFormattedString({String separator = ','}) {
    final absMinor = minorUnits.abs();
    final major = absMinor ~/ currency.minorUnitMultiplier;
    final fractional = absMinor % currency.minorUnitMultiplier;
    final sign = minorUnits < 0 ? '-' : '';

    // Add thousands separators
    final majorStr = major.toString();
    final buffer = StringBuffer();
    for (var i = 0; i < majorStr.length; i++) {
      if (i > 0 && (majorStr.length - i) % 3 == 0) {
        buffer.write(separator);
      }
      buffer.write(majorStr[i]);
    }

    if (currency.decimals == 0) {
      return '$sign${currency.symbol}$buffer';
    }

    final fractionalStr =
        fractional.toString().padLeft(currency.decimals, '0');
    return '$sign${currency.symbol}$buffer.$fractionalStr';
  }
}

/// Safe currency operations.
class SafeCurrency {
  /// Parse currency code from string.
  static CurrencyCode? parseCode(String code) {
    return CurrencyCode.fromString(code);
  }

  /// Check if valid currency code.
  static bool isValidCode(String code) {
    return CurrencyCode.fromString(code) != null;
  }

  /// Get all supported currency codes.
  static List<String> allCodes() {
    return CurrencyCode.values.map((c) => c.code).toList();
  }

  /// Get currencies by decimal places.
  static List<CurrencyCode> byDecimals(int decimals) {
    return CurrencyCode.values.where((c) => c.decimals == decimals).toList();
  }

  /// Check if two money values have the same currency.
  static bool sameCurrency(Money a, Money b) {
    return a.currency == b.currency;
  }

  /// Sum a list of money values (must be same currency).
  static CurrencyResult sum(List<Money> values) {
    if (values.isEmpty) {
      return const CurrencyResult.error('Empty list');
    }

    final currency = values.first.currency;
    var total = 0;

    for (final money in values) {
      if (money.currency != currency) {
        return const CurrencyResult.error('Currency mismatch in list');
      }
      total += money.minorUnits;
    }

    return CurrencyResult.ok(Money.fromMinor(total, currency));
  }

  /// Get the maximum value from a list.
  static Money? max(List<Money> values) {
    if (values.isEmpty) return null;

    final currency = values.first.currency;
    Money? maxValue;

    for (final money in values) {
      if (money.currency != currency) return null;
      if (maxValue == null || money.minorUnits > maxValue.minorUnits) {
        maxValue = money;
      }
    }

    return maxValue;
  }

  /// Get the minimum value from a list.
  static Money? min(List<Money> values) {
    if (values.isEmpty) return null;

    final currency = values.first.currency;
    Money? minValue;

    for (final money in values) {
      if (money.currency != currency) return null;
      if (minValue == null || money.minorUnits < minValue.minorUnits) {
        minValue = money;
      }
    }

    return minValue;
  }
}
