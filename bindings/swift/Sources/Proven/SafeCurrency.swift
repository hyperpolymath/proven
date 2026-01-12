// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Safe currency operations with type-safe monetary values.
/// All monetary values use integer minor units (cents, satoshis, etc.) to avoid floating-point errors.
public enum SafeCurrency {
    // MARK: - Currency Code

    /// ISO 4217 currency codes (common currencies).
    public enum CurrencyCode: String, Equatable, Hashable, CaseIterable, CustomStringConvertible {
        case usd = "USD"  // US Dollar
        case eur = "EUR"  // Euro
        case gbp = "GBP"  // British Pound
        case jpy = "JPY"  // Japanese Yen
        case chf = "CHF"  // Swiss Franc
        case cad = "CAD"  // Canadian Dollar
        case aud = "AUD"  // Australian Dollar
        case nzd = "NZD"  // New Zealand Dollar
        case cny = "CNY"  // Chinese Yuan
        case inr = "INR"  // Indian Rupee
        case brl = "BRL"  // Brazilian Real
        case mxn = "MXN"  // Mexican Peso
        case krw = "KRW"  // South Korean Won
        case sgd = "SGD"  // Singapore Dollar
        case hkd = "HKD"  // Hong Kong Dollar
        case sek = "SEK"  // Swedish Krona
        case nok = "NOK"  // Norwegian Krone
        case dkk = "DKK"  // Danish Krone
        case pln = "PLN"  // Polish Zloty
        case rub = "RUB"  // Russian Ruble
        case zar = "ZAR"  // South African Rand
        case `try` = "TRY" // Turkish Lira
        case thb = "THB"  // Thai Baht
        case myr = "MYR"  // Malaysian Ringgit
        case idr = "IDR"  // Indonesian Rupiah
        case php = "PHP"  // Philippine Peso
        case vnd = "VND"  // Vietnamese Dong
        case aed = "AED"  // UAE Dirham
        case sar = "SAR"  // Saudi Riyal
        case ils = "ILS"  // Israeli Shekel
        case czk = "CZK"  // Czech Koruna
        case huf = "HUF"  // Hungarian Forint
        case ron = "RON"  // Romanian Leu
        case bgn = "BGN"  // Bulgarian Lev
        case hrk = "HRK"  // Croatian Kuna
        case isk = "ISK"  // Icelandic Krona
        case clp = "CLP"  // Chilean Peso
        case cop = "COP"  // Colombian Peso
        case pen = "PEN"  // Peruvian Sol
        case ars = "ARS"  // Argentine Peso
        case btc = "BTC"  // Bitcoin
        case eth = "ETH"  // Ethereum

        public var description: String {
            rawValue
        }

        /// Number of decimal places for this currency.
        public var decimals: Int {
            switch self {
            case .jpy, .krw, .vnd:
                return 0
            case .btc:
                return 8
            case .eth:
                return 8  // Capped at 8 for practicality (wei is 18)
            default:
                return 2
            }
        }

        /// Currency symbol.
        public var symbol: String {
            switch self {
            case .usd: return "$"
            case .eur: return "€"
            case .gbp: return "£"
            case .jpy: return "¥"
            case .chf: return "Fr"
            case .cad: return "C$"
            case .aud: return "A$"
            case .nzd: return "NZ$"
            case .cny: return "¥"
            case .inr: return "₹"
            case .brl: return "R$"
            case .mxn: return "Mex$"
            case .krw: return "₩"
            case .sgd: return "S$"
            case .hkd: return "HK$"
            case .sek: return "kr"
            case .nok: return "kr"
            case .dkk: return "kr"
            case .pln: return "zł"
            case .rub: return "₽"
            case .zar: return "R"
            case .try: return "₺"
            case .thb: return "฿"
            case .myr: return "RM"
            case .idr: return "Rp"
            case .php: return "₱"
            case .vnd: return "₫"
            case .aed: return "د.إ"
            case .sar: return "﷼"
            case .ils: return "₪"
            case .czk: return "Kč"
            case .huf: return "Ft"
            case .ron: return "lei"
            case .bgn: return "лв"
            case .hrk: return "kn"
            case .isk: return "kr"
            case .clp: return "$"
            case .cop: return "$"
            case .pen: return "S/"
            case .ars: return "$"
            case .btc: return "₿"
            case .eth: return "Ξ"
            }
        }

        /// Full currency name.
        public var name: String {
            switch self {
            case .usd: return "US Dollar"
            case .eur: return "Euro"
            case .gbp: return "British Pound Sterling"
            case .jpy: return "Japanese Yen"
            case .chf: return "Swiss Franc"
            case .cad: return "Canadian Dollar"
            case .aud: return "Australian Dollar"
            case .nzd: return "New Zealand Dollar"
            case .cny: return "Chinese Yuan"
            case .inr: return "Indian Rupee"
            case .brl: return "Brazilian Real"
            case .mxn: return "Mexican Peso"
            case .krw: return "South Korean Won"
            case .sgd: return "Singapore Dollar"
            case .hkd: return "Hong Kong Dollar"
            case .sek: return "Swedish Krona"
            case .nok: return "Norwegian Krone"
            case .dkk: return "Danish Krone"
            case .pln: return "Polish Zloty"
            case .rub: return "Russian Ruble"
            case .zar: return "South African Rand"
            case .try: return "Turkish Lira"
            case .thb: return "Thai Baht"
            case .myr: return "Malaysian Ringgit"
            case .idr: return "Indonesian Rupiah"
            case .php: return "Philippine Peso"
            case .vnd: return "Vietnamese Dong"
            case .aed: return "UAE Dirham"
            case .sar: return "Saudi Riyal"
            case .ils: return "Israeli New Shekel"
            case .czk: return "Czech Koruna"
            case .huf: return "Hungarian Forint"
            case .ron: return "Romanian Leu"
            case .bgn: return "Bulgarian Lev"
            case .hrk: return "Croatian Kuna"
            case .isk: return "Icelandic Krona"
            case .clp: return "Chilean Peso"
            case .cop: return "Colombian Peso"
            case .pen: return "Peruvian Sol"
            case .ars: return "Argentine Peso"
            case .btc: return "Bitcoin"
            case .eth: return "Ethereum"
            }
        }
    }

    // MARK: - Currency Errors

    /// Currency operation errors.
    public enum CurrencyError: Error, Equatable, CustomStringConvertible {
        case invalidAmount(String)
        case currencyMismatch(CurrencyCode, CurrencyCode)
        case divisionByZero
        case overflow
        case invalidCurrencyCode(String)

        public var description: String {
            switch self {
            case .invalidAmount(let message):
                return "Invalid amount: \(message)"
            case .currencyMismatch(let first, let second):
                return "Currency mismatch: \(first) vs \(second)"
            case .divisionByZero:
                return "Division by zero"
            case .overflow:
                return "Arithmetic overflow"
            case .invalidCurrencyCode(let code):
                return "Invalid currency code: \(code)"
            }
        }
    }

    // MARK: - Money Structure

    /// Monetary value in minor units (cents, satoshis, etc.).
    public struct Money: Equatable, Hashable, Comparable, CustomStringConvertible {
        /// The currency code.
        public let currency: CurrencyCode

        /// The amount in minor units (cents, satoshis, etc.).
        public let minorUnits: Int64

        /// Create money from minor units.
        public init(currency: CurrencyCode, minorUnits: Int64) {
            self.currency = currency
            self.minorUnits = minorUnits
        }

        /// Create money from major units (dollars, euros, etc.).
        public init(currency: CurrencyCode, majorUnits: Int64) {
            self.currency = currency
            let multiplier = Self.multiplier(for: currency)
            self.minorUnits = majorUnits * multiplier
        }

        /// Zero amount for a currency.
        public static func zero(_ currency: CurrencyCode) -> Money {
            Money(currency: currency, minorUnits: 0)
        }

        /// Get the major units (dollars) as an integer (truncated).
        public var majorUnits: Int64 {
            let divisor = Self.multiplier(for: currency)
            return minorUnits / divisor
        }

        /// Get the fractional minor units (cents after the decimal).
        public var fractionalMinorUnits: Int64 {
            let divisor = Self.multiplier(for: currency)
            return abs(minorUnits) % divisor
        }

        /// Check if zero.
        public var isZero: Bool {
            minorUnits == 0
        }

        /// Check if positive.
        public var isPositive: Bool {
            minorUnits > 0
        }

        /// Check if negative.
        public var isNegative: Bool {
            minorUnits < 0
        }

        public var description: String {
            formatWithCode()
        }

        public static func < (lhs: Money, rhs: Money) -> Bool {
            guard lhs.currency == rhs.currency else {
                return false  // Not comparable across currencies
            }
            return lhs.minorUnits < rhs.minorUnits
        }

        private static func multiplier(for currency: CurrencyCode) -> Int64 {
            var result: Int64 = 1
            for _ in 0..<currency.decimals {
                result *= 10
            }
            return result
        }
    }

    // MARK: - Money Arithmetic

    /// Add two monetary values (same currency).
    public static func add(_ lhs: Money, _ rhs: Money) -> Result<Money, CurrencyError> {
        guard lhs.currency == rhs.currency else {
            return .failure(.currencyMismatch(lhs.currency, rhs.currency))
        }
        let (result, overflow) = lhs.minorUnits.addingReportingOverflow(rhs.minorUnits)
        if overflow {
            return .failure(.overflow)
        }
        return .success(Money(currency: lhs.currency, minorUnits: result))
    }

    /// Subtract two monetary values (same currency).
    public static func subtract(_ lhs: Money, _ rhs: Money) -> Result<Money, CurrencyError> {
        guard lhs.currency == rhs.currency else {
            return .failure(.currencyMismatch(lhs.currency, rhs.currency))
        }
        let (result, overflow) = lhs.minorUnits.subtractingReportingOverflow(rhs.minorUnits)
        if overflow {
            return .failure(.overflow)
        }
        return .success(Money(currency: lhs.currency, minorUnits: result))
    }

    /// Multiply money by a scalar.
    public static func multiply(_ money: Money, by scalar: Int64) -> Result<Money, CurrencyError> {
        let (result, overflow) = money.minorUnits.multipliedReportingOverflow(by: scalar)
        if overflow {
            return .failure(.overflow)
        }
        return .success(Money(currency: money.currency, minorUnits: result))
    }

    /// Divide money by a scalar (truncates).
    public static func divide(_ money: Money, by divisor: Int64) -> Result<Money, CurrencyError> {
        guard divisor != 0 else {
            return .failure(.divisionByZero)
        }
        return .success(Money(currency: money.currency, minorUnits: money.minorUnits / divisor))
    }

    /// Negate a monetary value.
    public static func negate(_ money: Money) -> Money {
        Money(currency: money.currency, minorUnits: -money.minorUnits)
    }

    /// Absolute value.
    public static func abs(_ money: Money) -> Money {
        Money(currency: money.currency, minorUnits: Swift.abs(money.minorUnits))
    }

    // MARK: - Money Formatting

    /// Format money with symbol (e.g., "$123.45").
    public static func format(_ money: Money) -> String {
        let isNegative = money.minorUnits < 0
        let absMinorUnits = Swift.abs(money.minorUnits)
        let decimals = money.currency.decimals
        let symbol = money.currency.symbol

        var multiplier: Int64 = 1
        for _ in 0..<decimals {
            multiplier *= 10
        }

        let majorPart = absMinorUnits / multiplier
        let minorPart = absMinorUnits % multiplier

        let negSign = isNegative ? "-" : ""

        if decimals == 0 {
            return "\(negSign)\(symbol)\(majorPart)"
        } else {
            let minorString = String(format: "%0\(decimals)lld", minorPart)
            return "\(negSign)\(symbol)\(majorPart).\(minorString)"
        }
    }

    /// Format money with code (e.g., "123.45 USD").
    public static func formatWithCode(_ money: Money) -> String {
        let isNegative = money.minorUnits < 0
        let absMinorUnits = Swift.abs(money.minorUnits)
        let decimals = money.currency.decimals

        var multiplier: Int64 = 1
        for _ in 0..<decimals {
            multiplier *= 10
        }

        let majorPart = absMinorUnits / multiplier
        let minorPart = absMinorUnits % multiplier

        let negSign = isNegative ? "-" : ""

        if decimals == 0 {
            return "\(negSign)\(majorPart) \(money.currency.rawValue)"
        } else {
            let minorString = String(format: "%0\(decimals)lld", minorPart)
            return "\(negSign)\(majorPart).\(minorString) \(money.currency.rawValue)"
        }
    }

    // MARK: - Currency Code Parsing

    /// Parse currency code from string.
    public static func parseCurrencyCode(_ string: String) -> CurrencyCode? {
        CurrencyCode(rawValue: string.uppercased())
    }

    /// Check if string is valid currency code.
    public static func isValidCurrencyCode(_ string: String) -> Bool {
        parseCurrencyCode(string) != nil
    }

    // MARK: - Exchange Rate

    /// Exchange rate between two currencies.
    /// Rate is stored as integer with 6 decimal places (1000000 = 1.0).
    public struct ExchangeRate {
        public let from: CurrencyCode
        public let to: CurrencyCode
        public let rateX1M: Int64  // Rate multiplied by 1,000,000

        /// Create exchange rate from whole and decimal parts.
        /// For example, 1.25 would be (whole: 1, decimal: 250000).
        public init(from: CurrencyCode, to: CurrencyCode, whole: Int64, decimal: Int64) {
            self.from = from
            self.to = to
            self.rateX1M = whole * 1_000_000 + decimal
        }

        /// Create exchange rate from a rate value (multiplied by 1,000,000).
        public init(from: CurrencyCode, to: CurrencyCode, rateX1M: Int64) {
            self.from = from
            self.to = to
            self.rateX1M = rateX1M
        }
    }

    /// Convert money using exchange rate.
    public static func convert(_ money: Money, using rate: ExchangeRate) -> Result<Money, CurrencyError> {
        guard money.currency == rate.from else {
            return .failure(.currencyMismatch(money.currency, rate.from))
        }
        let (product, overflow) = money.minorUnits.multipliedReportingOverflow(by: rate.rateX1M)
        if overflow {
            return .failure(.overflow)
        }
        let convertedUnits = product / 1_000_000
        return .success(Money(currency: rate.to, minorUnits: convertedUnits))
    }
}

// MARK: - Money Extension Methods

extension SafeCurrency.Money {
    /// Add another money value.
    public func adding(_ other: SafeCurrency.Money) -> Result<SafeCurrency.Money, SafeCurrency.CurrencyError> {
        SafeCurrency.add(self, other)
    }

    /// Subtract another money value.
    public func subtracting(_ other: SafeCurrency.Money) -> Result<SafeCurrency.Money, SafeCurrency.CurrencyError> {
        SafeCurrency.subtract(self, other)
    }

    /// Multiply by scalar.
    public func multiplied(by scalar: Int64) -> Result<SafeCurrency.Money, SafeCurrency.CurrencyError> {
        SafeCurrency.multiply(self, by: scalar)
    }

    /// Divide by scalar.
    public func divided(by divisor: Int64) -> Result<SafeCurrency.Money, SafeCurrency.CurrencyError> {
        SafeCurrency.divide(self, by: divisor)
    }

    /// Get negated value.
    public func negated() -> SafeCurrency.Money {
        SafeCurrency.negate(self)
    }

    /// Get absolute value.
    public func absoluteValue() -> SafeCurrency.Money {
        SafeCurrency.abs(self)
    }

    /// Format with symbol.
    public func format() -> String {
        SafeCurrency.format(self)
    }

    /// Format with currency code.
    public func formatWithCode() -> String {
        SafeCurrency.formatWithCode(self)
    }
}

// MARK: - Operators

extension SafeCurrency.Money {
    public static func + (lhs: SafeCurrency.Money, rhs: SafeCurrency.Money) -> SafeCurrency.Money? {
        switch SafeCurrency.add(lhs, rhs) {
        case .success(let result): return result
        case .failure: return nil
        }
    }

    public static func - (lhs: SafeCurrency.Money, rhs: SafeCurrency.Money) -> SafeCurrency.Money? {
        switch SafeCurrency.subtract(lhs, rhs) {
        case .success(let result): return result
        case .failure: return nil
        }
    }

    public static func * (lhs: SafeCurrency.Money, rhs: Int64) -> SafeCurrency.Money? {
        switch SafeCurrency.multiply(lhs, by: rhs) {
        case .success(let result): return result
        case .failure: return nil
        }
    }

    public static func / (lhs: SafeCurrency.Money, rhs: Int64) -> SafeCurrency.Money? {
        switch SafeCurrency.divide(lhs, by: rhs) {
        case .success(let result): return result
        case .failure: return nil
        }
    }

    public static prefix func - (operand: SafeCurrency.Money) -> SafeCurrency.Money {
        SafeCurrency.negate(operand)
    }
}
