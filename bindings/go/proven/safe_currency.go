// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"errors"
	"fmt"
	"strings"
)

// CurrencyCode represents ISO 4217 currency codes.
type CurrencyCode int

const (
	CurrencyUSD CurrencyCode = iota
	CurrencyEUR
	CurrencyGBP
	CurrencyJPY
	CurrencyCHF
	CurrencyCAD
	CurrencyAUD
	CurrencyNZD
	CurrencyCNY
	CurrencyINR
	CurrencyBRL
	CurrencyMXN
	CurrencyKRW
	CurrencySGD
	CurrencyHKD
	CurrencySEK
	CurrencyNOK
	CurrencyDKK
	CurrencyPLN
	CurrencyRUB
	CurrencyZAR
	CurrencyTRY
	CurrencyTHB
	CurrencyMYR
	CurrencyIDR
	CurrencyPHP
	CurrencyVND
	CurrencyAED
	CurrencySAR
	CurrencyILS
	CurrencyCZK
	CurrencyHUF
	CurrencyRON
	CurrencyBGN
	CurrencyHRK
	CurrencyISK
	CurrencyCLP
	CurrencyCOP
	CurrencyPEN
	CurrencyARS
	CurrencyBTC
	CurrencyETH
	CurrencyUnknown
)

var (
	// ErrCurrencyMismatch indicates an operation with different currencies.
	ErrCurrencyMismatch = errors.New("currency mismatch")

	// ErrCurrencyDivisionByZero indicates division by zero in currency operation.
	ErrCurrencyDivisionByZero = errors.New("division by zero")

	// ErrUnknownCurrencyCode indicates an unknown currency code.
	ErrUnknownCurrencyCode = errors.New("unknown currency code")
)

// currencyCodeStrings maps CurrencyCode to string representation.
var currencyCodeStrings = map[CurrencyCode]string{
	CurrencyUSD: "USD", CurrencyEUR: "EUR", CurrencyGBP: "GBP", CurrencyJPY: "JPY",
	CurrencyCHF: "CHF", CurrencyCAD: "CAD", CurrencyAUD: "AUD", CurrencyNZD: "NZD",
	CurrencyCNY: "CNY", CurrencyINR: "INR", CurrencyBRL: "BRL", CurrencyMXN: "MXN",
	CurrencyKRW: "KRW", CurrencySGD: "SGD", CurrencyHKD: "HKD", CurrencySEK: "SEK",
	CurrencyNOK: "NOK", CurrencyDKK: "DKK", CurrencyPLN: "PLN", CurrencyRUB: "RUB",
	CurrencyZAR: "ZAR", CurrencyTRY: "TRY", CurrencyTHB: "THB", CurrencyMYR: "MYR",
	CurrencyIDR: "IDR", CurrencyPHP: "PHP", CurrencyVND: "VND", CurrencyAED: "AED",
	CurrencySAR: "SAR", CurrencyILS: "ILS", CurrencyCZK: "CZK", CurrencyHUF: "HUF",
	CurrencyRON: "RON", CurrencyBGN: "BGN", CurrencyHRK: "HRK", CurrencyISK: "ISK",
	CurrencyCLP: "CLP", CurrencyCOP: "COP", CurrencyPEN: "PEN", CurrencyARS: "ARS",
	CurrencyBTC: "BTC", CurrencyETH: "ETH",
}

// stringToCurrencyCode maps string to CurrencyCode.
var stringToCurrencyCode = map[string]CurrencyCode{
	"USD": CurrencyUSD, "EUR": CurrencyEUR, "GBP": CurrencyGBP, "JPY": CurrencyJPY,
	"CHF": CurrencyCHF, "CAD": CurrencyCAD, "AUD": CurrencyAUD, "NZD": CurrencyNZD,
	"CNY": CurrencyCNY, "INR": CurrencyINR, "BRL": CurrencyBRL, "MXN": CurrencyMXN,
	"KRW": CurrencyKRW, "SGD": CurrencySGD, "HKD": CurrencyHKD, "SEK": CurrencySEK,
	"NOK": CurrencyNOK, "DKK": CurrencyDKK, "PLN": CurrencyPLN, "RUB": CurrencyRUB,
	"ZAR": CurrencyZAR, "TRY": CurrencyTRY, "THB": CurrencyTHB, "MYR": CurrencyMYR,
	"IDR": CurrencyIDR, "PHP": CurrencyPHP, "VND": CurrencyVND, "AED": CurrencyAED,
	"SAR": CurrencySAR, "ILS": CurrencyILS, "CZK": CurrencyCZK, "HUF": CurrencyHUF,
	"RON": CurrencyRON, "BGN": CurrencyBGN, "HRK": CurrencyHRK, "ISK": CurrencyISK,
	"CLP": CurrencyCLP, "COP": CurrencyCOP, "PEN": CurrencyPEN, "ARS": CurrencyARS,
	"BTC": CurrencyBTC, "ETH": CurrencyETH,
}

// String returns the ISO 4217 code string.
func (c CurrencyCode) String() string {
	if s, ok := currencyCodeStrings[c]; ok {
		return s
	}
	return "UNKNOWN"
}

// Decimals returns the number of decimal places for this currency.
func (c CurrencyCode) Decimals() uint8 {
	switch c {
	case CurrencyJPY, CurrencyKRW, CurrencyVND:
		return 0
	case CurrencyBTC, CurrencyETH:
		return 8
	default:
		return 2
	}
}

// Symbol returns the currency symbol.
func (c CurrencyCode) Symbol() string {
	switch c {
	case CurrencyUSD, CurrencyCAD, CurrencyAUD, CurrencyNZD, CurrencyHKD, CurrencySGD, CurrencyMXN:
		return "$"
	case CurrencyEUR:
		return "€"
	case CurrencyGBP:
		return "£"
	case CurrencyJPY, CurrencyCNY:
		return "¥"
	case CurrencyCHF:
		return "Fr"
	case CurrencyINR:
		return "₹"
	case CurrencyKRW:
		return "₩"
	case CurrencyRUB:
		return "₽"
	case CurrencyBTC:
		return "₿"
	case CurrencyETH:
		return "Ξ"
	case CurrencyBRL:
		return "R$"
	case CurrencyTRY:
		return "₺"
	case CurrencyTHB:
		return "฿"
	case CurrencyPLN:
		return "zł"
	case CurrencyILS:
		return "₪"
	default:
		return ""
	}
}

// Name returns the currency name.
func (c CurrencyCode) Name() string {
	switch c {
	case CurrencyUSD:
		return "US Dollar"
	case CurrencyEUR:
		return "Euro"
	case CurrencyGBP:
		return "British Pound"
	case CurrencyJPY:
		return "Japanese Yen"
	case CurrencyCHF:
		return "Swiss Franc"
	case CurrencyCAD:
		return "Canadian Dollar"
	case CurrencyAUD:
		return "Australian Dollar"
	case CurrencyCNY:
		return "Chinese Yuan"
	case CurrencyINR:
		return "Indian Rupee"
	case CurrencyBTC:
		return "Bitcoin"
	case CurrencyETH:
		return "Ethereum"
	default:
		return "Currency"
	}
}

// ParseCurrencyCode parses a currency code from string.
// Returns CurrencyUnknown if not recognized.
func ParseCurrencyCode(s string) CurrencyCode {
	if code, ok := stringToCurrencyCode[strings.ToUpper(s)]; ok {
		return code
	}
	return CurrencyUnknown
}

// ParseCurrencyCodeStrict parses a currency code from string.
// Returns an error if not recognized.
func ParseCurrencyCodeStrict(s string) (CurrencyCode, error) {
	code := ParseCurrencyCode(s)
	if code == CurrencyUnknown {
		return CurrencyUnknown, ErrUnknownCurrencyCode
	}
	return code, nil
}

// IsValidCurrencyCode checks if a string is a valid currency code.
func IsValidCurrencyCode(s string) bool {
	return ParseCurrencyCode(s) != CurrencyUnknown
}

// Money represents a type-safe monetary value.
type Money struct {
	minorUnits int64
	currency   CurrencyCode
}

// NewMoneyFromMajor creates a Money from major units (dollars, euros, etc.).
func NewMoneyFromMajor(amount int64, currency CurrencyCode) Money {
	multiplier := int64(1)
	for i := uint8(0); i < currency.Decimals(); i++ {
		multiplier *= 10
	}
	return Money{
		minorUnits: amount * multiplier,
		currency:   currency,
	}
}

// NewMoneyFromMinor creates a Money from minor units (cents, satoshis, etc.).
func NewMoneyFromMinor(amount int64, currency CurrencyCode) Money {
	return Money{
		minorUnits: amount,
		currency:   currency,
	}
}

// MoneyZero creates a zero amount in the given currency.
func MoneyZero(currency CurrencyCode) Money {
	return Money{
		minorUnits: 0,
		currency:   currency,
	}
}

// Currency returns the currency code.
func (m Money) Currency() CurrencyCode {
	return m.currency
}

// Minor returns the amount in minor units.
func (m Money) Minor() int64 {
	return m.minorUnits
}

// Major returns the amount in major units (truncated).
func (m Money) Major() int64 {
	divisor := int64(1)
	for i := uint8(0); i < m.currency.Decimals(); i++ {
		divisor *= 10
	}
	return m.minorUnits / divisor
}

// IsZero checks if the amount is zero.
func (m Money) IsZero() bool {
	return m.minorUnits == 0
}

// IsPositive checks if the amount is positive.
func (m Money) IsPositive() bool {
	return m.minorUnits > 0
}

// IsNegative checks if the amount is negative.
func (m Money) IsNegative() bool {
	return m.minorUnits < 0
}

// Add adds two monetary values.
// Returns an error if currencies do not match.
func (m Money) Add(other Money) (Money, error) {
	if m.currency != other.currency {
		return Money{}, ErrCurrencyMismatch
	}
	return Money{
		minorUnits: m.minorUnits + other.minorUnits,
		currency:   m.currency,
	}, nil
}

// Sub subtracts two monetary values.
// Returns an error if currencies do not match.
func (m Money) Sub(other Money) (Money, error) {
	if m.currency != other.currency {
		return Money{}, ErrCurrencyMismatch
	}
	return Money{
		minorUnits: m.minorUnits - other.minorUnits,
		currency:   m.currency,
	}, nil
}

// Mul multiplies by a scalar.
func (m Money) Mul(scalar int64) Money {
	return Money{
		minorUnits: m.minorUnits * scalar,
		currency:   m.currency,
	}
}

// Div divides by a scalar.
// Returns an error if scalar is zero.
func (m Money) Div(scalar int64) (Money, error) {
	if scalar == 0 {
		return Money{}, ErrCurrencyDivisionByZero
	}
	return Money{
		minorUnits: m.minorUnits / scalar,
		currency:   m.currency,
	}, nil
}

// Abs returns the absolute value.
func (m Money) Abs() Money {
	if m.minorUnits < 0 {
		return Money{
			minorUnits: -m.minorUnits,
			currency:   m.currency,
		}
	}
	return m
}

// Negate returns the negated value.
func (m Money) Negate() Money {
	return Money{
		minorUnits: -m.minorUnits,
		currency:   m.currency,
	}
}

// String formats the money as a string with symbol.
func (m Money) String() string {
	dec := m.currency.Decimals()
	divisor := int64(1)
	for i := uint8(0); i < dec; i++ {
		divisor *= 10
	}

	absUnits := m.minorUnits
	sign := ""
	if absUnits < 0 {
		sign = "-"
		absUnits = -absUnits
	}

	major := absUnits / divisor
	minor := absUnits % divisor

	if dec == 0 {
		return fmt.Sprintf("%s%s%d", sign, m.currency.Symbol(), major)
	}
	return fmt.Sprintf("%s%s%d.%0*d", sign, m.currency.Symbol(), major, dec, minor)
}

// FormatMoney formats a Money value as a string.
func FormatMoney(m Money) string {
	return m.String()
}
