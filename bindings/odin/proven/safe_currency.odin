// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:strings"
import "core:strconv"
import "core:fmt"
import "core:math"

// ISO 4217 currency codes.
// This is a subset of the most commonly used currencies.
Currency_Code :: enum u16 {
    UNKNOWN = 0,

    // Major currencies
    USD = 840,  // US Dollar
    EUR = 978,  // Euro
    GBP = 826,  // British Pound
    JPY = 392,  // Japanese Yen
    CHF = 756,  // Swiss Franc
    CAD = 124,  // Canadian Dollar
    AUD = 36,   // Australian Dollar
    NZD = 554,  // New Zealand Dollar

    // Asian currencies
    CNY = 156,  // Chinese Yuan
    HKD = 344,  // Hong Kong Dollar
    SGD = 702,  // Singapore Dollar
    KRW = 410,  // South Korean Won
    INR = 356,  // Indian Rupee
    THB = 764,  // Thai Baht
    MYR = 458,  // Malaysian Ringgit
    IDR = 360,  // Indonesian Rupiah
    PHP = 608,  // Philippine Peso
    VND = 704,  // Vietnamese Dong
    TWD = 901,  // New Taiwan Dollar

    // European currencies
    SEK = 752,  // Swedish Krona
    NOK = 578,  // Norwegian Krone
    DKK = 208,  // Danish Krone
    PLN = 985,  // Polish Zloty
    CZK = 203,  // Czech Koruna
    HUF = 348,  // Hungarian Forint
    RUB = 643,  // Russian Ruble
    TRY = 949,  // Turkish Lira
    RON = 946,  // Romanian Leu
    BGN = 975,  // Bulgarian Lev
    HRK = 191,  // Croatian Kuna
    ISK = 352,  // Icelandic Krona

    // Americas currencies
    MXN = 484,  // Mexican Peso
    BRL = 986,  // Brazilian Real
    ARS = 32,   // Argentine Peso
    CLP = 152,  // Chilean Peso
    COP = 170,  // Colombian Peso
    PEN = 604,  // Peruvian Sol

    // Middle East / Africa currencies
    ILS = 376,  // Israeli Shekel
    AED = 784,  // UAE Dirham
    SAR = 682,  // Saudi Riyal
    ZAR = 710,  // South African Rand
    EGP = 818,  // Egyptian Pound
    NGN = 566,  // Nigerian Naira
    KES = 404,  // Kenyan Shilling

    // Cryptocurrencies (using high values to avoid conflicts)
    BTC = 1000, // Bitcoin
    ETH = 1001, // Ethereum

    // Precious metals
    XAU = 959,  // Gold
    XAG = 961,  // Silver
    XPT = 962,  // Platinum
}

// Money represents a monetary amount with currency.
// Uses minor units (cents for USD, pence for GBP, etc.) for precision.
Money :: struct {
    minor_units: i64,           // Amount in smallest currency unit
    currency:    Currency_Code, // Currency code
}

// Get the number of decimal places for a currency.
currency_decimals :: proc(currency: Currency_Code) -> int {
    switch currency {
    // Zero decimal currencies
    case .JPY, .KRW, .VND, .IDR, .ISK, .CLP, .HUF:
        return 0
    // Three decimal currencies
    case .BTC:
        return 8  // Bitcoin uses satoshis (10^-8)
    case .ETH:
        return 18 // Ethereum uses wei (10^-18), but we cap at 8 for practical use
    // Standard two decimal currencies
    case:
        return 2
    }
}

// Get the minor unit divisor for a currency.
currency_divisor :: proc(currency: Currency_Code) -> i64 {
    decimals := currency_decimals(currency)
    result: i64 = 1
    for _ in 0..<decimals {
        result *= 10
    }
    return result
}

// Get the ISO 4217 alpha-3 code for a currency.
currency_code_string :: proc(currency: Currency_Code) -> string {
    switch currency {
    case .USD: return "USD"
    case .EUR: return "EUR"
    case .GBP: return "GBP"
    case .JPY: return "JPY"
    case .CHF: return "CHF"
    case .CAD: return "CAD"
    case .AUD: return "AUD"
    case .NZD: return "NZD"
    case .CNY: return "CNY"
    case .HKD: return "HKD"
    case .SGD: return "SGD"
    case .KRW: return "KRW"
    case .INR: return "INR"
    case .THB: return "THB"
    case .MYR: return "MYR"
    case .IDR: return "IDR"
    case .PHP: return "PHP"
    case .VND: return "VND"
    case .TWD: return "TWD"
    case .SEK: return "SEK"
    case .NOK: return "NOK"
    case .DKK: return "DKK"
    case .PLN: return "PLN"
    case .CZK: return "CZK"
    case .HUF: return "HUF"
    case .RUB: return "RUB"
    case .TRY: return "TRY"
    case .RON: return "RON"
    case .BGN: return "BGN"
    case .HRK: return "HRK"
    case .ISK: return "ISK"
    case .MXN: return "MXN"
    case .BRL: return "BRL"
    case .ARS: return "ARS"
    case .CLP: return "CLP"
    case .COP: return "COP"
    case .PEN: return "PEN"
    case .ILS: return "ILS"
    case .AED: return "AED"
    case .SAR: return "SAR"
    case .ZAR: return "ZAR"
    case .EGP: return "EGP"
    case .NGN: return "NGN"
    case .KES: return "KES"
    case .BTC: return "BTC"
    case .ETH: return "ETH"
    case .XAU: return "XAU"
    case .XAG: return "XAG"
    case .XPT: return "XPT"
    case: return "???"
    }
}

// Get the currency symbol.
currency_symbol :: proc(currency: Currency_Code) -> string {
    switch currency {
    case .USD, .CAD, .AUD, .NZD, .HKD, .SGD, .MXN, .ARS, .CLP, .COP:
        return "$"
    case .EUR: return "€"
    case .GBP, .EGP: return "£"
    case .JPY, .CNY: return "¥"
    case .CHF: return "CHF"
    case .KRW: return "₩"
    case .INR: return "₹"
    case .THB: return "฿"
    case .RUB: return "₽"
    case .TRY: return "₺"
    case .ILS: return "₪"
    case .BRL: return "R$"
    case .ZAR: return "R"
    case .BTC: return "₿"
    case .ETH: return "Ξ"
    case: return ""
    }
}

// Parse currency code from string.
parse_currency_code :: proc(code: string) -> (currency: Currency_Code, ok: bool) {
    upper := strings.to_upper(code)
    switch upper {
    case "USD": return .USD, true
    case "EUR": return .EUR, true
    case "GBP": return .GBP, true
    case "JPY": return .JPY, true
    case "CHF": return .CHF, true
    case "CAD": return .CAD, true
    case "AUD": return .AUD, true
    case "NZD": return .NZD, true
    case "CNY": return .CNY, true
    case "HKD": return .HKD, true
    case "SGD": return .SGD, true
    case "KRW": return .KRW, true
    case "INR": return .INR, true
    case "THB": return .THB, true
    case "MYR": return .MYR, true
    case "IDR": return .IDR, true
    case "PHP": return .PHP, true
    case "VND": return .VND, true
    case "TWD": return .TWD, true
    case "SEK": return .SEK, true
    case "NOK": return .NOK, true
    case "DKK": return .DKK, true
    case "PLN": return .PLN, true
    case "CZK": return .CZK, true
    case "HUF": return .HUF, true
    case "RUB": return .RUB, true
    case "TRY": return .TRY, true
    case "RON": return .RON, true
    case "BGN": return .BGN, true
    case "HRK": return .HRK, true
    case "ISK": return .ISK, true
    case "MXN": return .MXN, true
    case "BRL": return .BRL, true
    case "ARS": return .ARS, true
    case "CLP": return .CLP, true
    case "COP": return .COP, true
    case "PEN": return .PEN, true
    case "ILS": return .ILS, true
    case "AED": return .AED, true
    case "SAR": return .SAR, true
    case "ZAR": return .ZAR, true
    case "EGP": return .EGP, true
    case "NGN": return .NGN, true
    case "KES": return .KES, true
    case "BTC": return .BTC, true
    case "ETH": return .ETH, true
    case "XAU": return .XAU, true
    case "XAG": return .XAG, true
    case "XPT": return .XPT, true
    case: return .UNKNOWN, false
    }
}

// Create money from major units (e.g., dollars).
money_from_major :: proc(amount: f64, currency: Currency_Code) -> Money {
    divisor := f64(currency_divisor(currency))
    minor := i64(math.round(amount * divisor))
    return Money{minor_units = minor, currency = currency}
}

// Create money from minor units (e.g., cents).
money_from_minor :: proc(minor_units: i64, currency: Currency_Code) -> Money {
    return Money{minor_units = minor_units, currency = currency}
}

// Get the major unit value as a float.
money_to_major :: proc(money: Money) -> f64 {
    divisor := f64(currency_divisor(money.currency))
    return f64(money.minor_units) / divisor
}

// Check if two money values have the same currency.
same_currency :: proc(a, b: Money) -> bool {
    return a.currency == b.currency
}

// Add two money values.
money_add :: proc(a, b: Money) -> (result: Money, ok: bool) {
    if !same_currency(a, b) {
        return {}, false
    }

    // Check for overflow
    if b.minor_units > 0 && a.minor_units > max(i64) - b.minor_units {
        return {}, false
    }
    if b.minor_units < 0 && a.minor_units < min(i64) - b.minor_units {
        return {}, false
    }

    return Money{
        minor_units = a.minor_units + b.minor_units,
        currency = a.currency,
    }, true
}

// Subtract two money values.
money_sub :: proc(a, b: Money) -> (result: Money, ok: bool) {
    if !same_currency(a, b) {
        return {}, false
    }

    // Check for overflow
    if b.minor_units < 0 && a.minor_units > max(i64) + b.minor_units {
        return {}, false
    }
    if b.minor_units > 0 && a.minor_units < min(i64) + b.minor_units {
        return {}, false
    }

    return Money{
        minor_units = a.minor_units - b.minor_units,
        currency = a.currency,
    }, true
}

// Multiply money by a scalar.
money_mul :: proc(money: Money, factor: i64) -> (result: Money, ok: bool) {
    if factor == 0 {
        return Money{minor_units = 0, currency = money.currency}, true
    }

    // Check for overflow
    r := money.minor_units * factor
    if money.minor_units != 0 && r / money.minor_units != factor {
        return {}, false
    }

    return Money{minor_units = r, currency = money.currency}, true
}

// Multiply money by a float factor (with rounding).
money_mul_float :: proc(money: Money, factor: f64) -> Money {
    result := f64(money.minor_units) * factor
    return Money{
        minor_units = i64(math.round(result)),
        currency = money.currency,
    }
}

// Divide money by a scalar.
money_div :: proc(money: Money, divisor: i64) -> (result: Money, ok: bool) {
    if divisor == 0 {
        return {}, false
    }

    return Money{
        minor_units = money.minor_units / divisor,
        currency = money.currency,
    }, true
}

// Divide money by a float divisor (with rounding).
money_div_float :: proc(money: Money, divisor: f64) -> (result: Money, ok: bool) {
    if divisor == 0.0 {
        return {}, false
    }

    result_value := f64(money.minor_units) / divisor
    return Money{
        minor_units = i64(math.round(result_value)),
        currency = money.currency,
    }, true
}

// Negate money value.
money_neg :: proc(money: Money) -> (result: Money, ok: bool) {
    if money.minor_units == min(i64) {
        return {}, false
    }
    return Money{minor_units = -money.minor_units, currency = money.currency}, true
}

// Get absolute value of money.
money_abs :: proc(money: Money) -> (result: Money, ok: bool) {
    if money.minor_units == min(i64) {
        return {}, false
    }
    return Money{minor_units = abs(money.minor_units), currency = money.currency}, true
}

// Compare two money values.
// Returns -1 if a < b, 0 if a == b, 1 if a > b.
// Returns ok = false if currencies differ.
money_compare :: proc(a, b: Money) -> (result: int, ok: bool) {
    if !same_currency(a, b) {
        return 0, false
    }

    if a.minor_units < b.minor_units {
        return -1, true
    }
    if a.minor_units > b.minor_units {
        return 1, true
    }
    return 0, true
}

// Check if money is zero.
money_is_zero :: proc(money: Money) -> bool {
    return money.minor_units == 0
}

// Check if money is positive.
money_is_positive :: proc(money: Money) -> bool {
    return money.minor_units > 0
}

// Check if money is negative.
money_is_negative :: proc(money: Money) -> bool {
    return money.minor_units < 0
}

// Format money as a string.
format_money :: proc(money: Money, allocator := context.allocator) -> string {
    decimals := currency_decimals(money.currency)
    code := currency_code_string(money.currency)

    if decimals == 0 {
        return fmt.aprintf("%d %s", money.minor_units, code)
    }

    divisor := currency_divisor(money.currency)
    major := money.minor_units / divisor
    minor := abs(money.minor_units % divisor)

    // Build format string for the correct number of decimal places
    if money.minor_units < 0 && major == 0 {
        return fmt.aprintf("-%d.%0*d %s", major, decimals, minor, code)
    }
    return fmt.aprintf("%d.%0*d %s", major, decimals, minor, code)
}

// Format money with symbol.
format_money_symbol :: proc(money: Money, allocator := context.allocator) -> string {
    decimals := currency_decimals(money.currency)
    symbol := currency_symbol(money.currency)

    if len(symbol) == 0 {
        return format_money(money, allocator)
    }

    if decimals == 0 {
        return fmt.aprintf("%s%d", symbol, money.minor_units)
    }

    divisor := currency_divisor(money.currency)
    major := money.minor_units / divisor
    minor := abs(money.minor_units % divisor)

    if money.minor_units < 0 && major == 0 {
        return fmt.aprintf("-%s%d.%0*d", symbol, major, decimals, minor)
    }
    return fmt.aprintf("%s%d.%0*d", symbol, major, decimals, minor)
}

// Parse money from string (e.g., "123.45 USD" or "$123.45").
parse_money :: proc(input: string) -> (money: Money, ok: bool) {
    s := strings.trim_space(input)
    if len(s) == 0 {
        return {}, false
    }

    // Try to find currency code at end (e.g., "123.45 USD")
    parts := strings.split(s, " ")
    defer delete(parts)

    if len(parts) == 2 {
        amount_str := parts[0]
        currency_str := parts[1]

        currency, currency_ok := parse_currency_code(currency_str)
        if !currency_ok {
            return {}, false
        }

        return parse_money_amount(amount_str, currency)
    }

    // Try to detect currency from symbol prefix
    for curr in Currency_Code {
        symbol := currency_symbol(curr)
        if len(symbol) > 0 && strings.has_prefix(s, symbol) {
            return parse_money_amount(s[len(symbol):], curr)
        }
    }

    return {}, false
}

// Parse money amount with known currency.
parse_money_amount :: proc(amount_str: string, currency: Currency_Code) -> (money: Money, ok: bool) {
    s := strings.trim_space(amount_str)

    // Handle negative numbers
    negative := false
    if len(s) > 0 && s[0] == '-' {
        negative = true
        s = s[1:]
    }

    // Split on decimal point
    parts := strings.split(s, ".")
    defer delete(parts)

    decimals := currency_decimals(currency)
    divisor := currency_divisor(currency)

    major: i64 = 0
    minor: i64 = 0

    if len(parts) >= 1 {
        major_val, major_ok := strconv.parse_i64(parts[0])
        if !major_ok {
            return {}, false
        }
        major = major_val
    }

    if len(parts) == 2 {
        minor_str := parts[1]

        // Pad or truncate to match currency decimals
        if len(minor_str) > decimals {
            minor_str = minor_str[:decimals]
        }
        for len(minor_str) < decimals {
            minor_str = strings.concatenate({minor_str, "0"})
        }

        minor_val, minor_ok := strconv.parse_i64(minor_str)
        if !minor_ok {
            return {}, false
        }
        minor = minor_val
    } else if len(parts) > 2 {
        return {}, false
    }

    total := major * divisor + minor
    if negative {
        total = -total
    }

    return Money{minor_units = total, currency = currency}, true
}

// Split money into equal parts (handles remainder).
money_split :: proc(money: Money, parts: int, allocator := context.allocator) -> (result: []Money, ok: bool) {
    if parts <= 0 {
        return nil, false
    }

    base := money.minor_units / i64(parts)
    remainder := money.minor_units % i64(parts)

    result_slice := make([]Money, parts, allocator)

    for i := 0; i < parts; i += 1 {
        amount := base
        if i64(i) < remainder {
            amount += 1
        }
        result_slice[i] = Money{minor_units = amount, currency = money.currency}
    }

    return result_slice, true
}

// Allocate money by percentages (percentages should sum to 100).
money_allocate :: proc(money: Money, percentages: []f64, allocator := context.allocator) -> (result: []Money, ok: bool) {
    if len(percentages) == 0 {
        return nil, false
    }

    // Verify percentages sum to ~100
    total_pct: f64 = 0
    for pct in percentages {
        if pct < 0 {
            return nil, false
        }
        total_pct += pct
    }
    if total_pct < 99.99 || total_pct > 100.01 {
        return nil, false
    }

    result_slice := make([]Money, len(percentages), allocator)
    allocated: i64 = 0

    for pct, i in percentages {
        amount := i64(math.round(f64(money.minor_units) * pct / 100.0))
        result_slice[i] = Money{minor_units = amount, currency = money.currency}
        allocated += amount
    }

    // Adjust for rounding errors (add/subtract difference to largest allocation)
    diff := money.minor_units - allocated
    if diff != 0 {
        max_idx := 0
        max_val: i64 = 0
        for m, i in result_slice {
            if abs(m.minor_units) > max_val {
                max_val = abs(m.minor_units)
                max_idx = i
            }
        }
        result_slice[max_idx].minor_units += diff
    }

    return result_slice, true
}
