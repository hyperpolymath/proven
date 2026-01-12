// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

module proven

import strings

// ISO 4217 currency codes.
pub enum CurrencyCode {
	usd // US Dollar
	eur // Euro
	gbp // British Pound
	jpy // Japanese Yen
	chf // Swiss Franc
	cad // Canadian Dollar
	aud // Australian Dollar
	nzd // New Zealand Dollar
	cny // Chinese Yuan
	inr // Indian Rupee
	brl // Brazilian Real
	mxn // Mexican Peso
	krw // South Korean Won
	sgd // Singapore Dollar
	hkd // Hong Kong Dollar
	sek // Swedish Krona
	nok // Norwegian Krone
	dkk // Danish Krone
	pln // Polish Zloty
	rub // Russian Ruble
	zar // South African Rand
	try_ // Turkish Lira (try is a keyword)
	thb // Thai Baht
	myr // Malaysian Ringgit
	idr // Indonesian Rupiah
	php // Philippine Peso
	vnd // Vietnamese Dong
	aed // UAE Dirham
	sar // Saudi Riyal
	ils // Israeli Shekel
	czk // Czech Koruna
	huf // Hungarian Forint
	ron // Romanian Leu
	bgn // Bulgarian Lev
	hrk // Croatian Kuna
	isk // Icelandic Krona
	clp // Chilean Peso
	cop // Colombian Peso
	pen // Peruvian Sol
	ars // Argentine Peso
	btc // Bitcoin
	eth // Ethereum
}

// Get number of decimal places for currency.
pub fn (code CurrencyCode) decimals() u8 {
	return match code {
		.jpy, .krw, .vnd { 0 }
		.btc, .eth { 8 }
		else { 2 }
	}
}

// Get currency symbol.
pub fn (code CurrencyCode) symbol() string {
	return match code {
		.usd, .cad, .aud, .nzd, .sgd, .hkd, .mxn { '\$' }
		.eur { '€' }
		.gbp { '£' }
		.jpy, .cny { '¥' }
		.chf { 'Fr' }
		.inr { '₹' }
		.krw { '₩' }
		.rub { '₽' }
		.btc { '₿' }
		.eth { 'Ξ' }
		.brl { 'R\$' }
		.zar { 'R' }
		.try_ { '₺' }
		.thb { '฿' }
		.ils { '₪' }
		.pln { 'zł' }
		else { '' }
	}
}

// Get currency name.
pub fn (code CurrencyCode) name() string {
	return match code {
		.usd { 'US Dollar' }
		.eur { 'Euro' }
		.gbp { 'British Pound' }
		.jpy { 'Japanese Yen' }
		.chf { 'Swiss Franc' }
		.cad { 'Canadian Dollar' }
		.aud { 'Australian Dollar' }
		.nzd { 'New Zealand Dollar' }
		.cny { 'Chinese Yuan' }
		.inr { 'Indian Rupee' }
		.brl { 'Brazilian Real' }
		.mxn { 'Mexican Peso' }
		.krw { 'South Korean Won' }
		.sgd { 'Singapore Dollar' }
		.hkd { 'Hong Kong Dollar' }
		.rub { 'Russian Ruble' }
		.btc { 'Bitcoin' }
		.eth { 'Ethereum' }
		else { 'Currency' }
	}
}

// Get ISO 4217 code string.
pub fn (code CurrencyCode) code_str() string {
	return match code {
		.usd { 'USD' }
		.eur { 'EUR' }
		.gbp { 'GBP' }
		.jpy { 'JPY' }
		.chf { 'CHF' }
		.cad { 'CAD' }
		.aud { 'AUD' }
		.nzd { 'NZD' }
		.cny { 'CNY' }
		.inr { 'INR' }
		.brl { 'BRL' }
		.mxn { 'MXN' }
		.krw { 'KRW' }
		.sgd { 'SGD' }
		.hkd { 'HKD' }
		.sek { 'SEK' }
		.nok { 'NOK' }
		.dkk { 'DKK' }
		.pln { 'PLN' }
		.rub { 'RUB' }
		.zar { 'ZAR' }
		.try_ { 'TRY' }
		.thb { 'THB' }
		.myr { 'MYR' }
		.idr { 'IDR' }
		.php { 'PHP' }
		.vnd { 'VND' }
		.aed { 'AED' }
		.sar { 'SAR' }
		.ils { 'ILS' }
		.czk { 'CZK' }
		.huf { 'HUF' }
		.ron { 'RON' }
		.bgn { 'BGN' }
		.hrk { 'HRK' }
		.isk { 'ISK' }
		.clp { 'CLP' }
		.cop { 'COP' }
		.pen { 'PEN' }
		.ars { 'ARS' }
		.btc { 'BTC' }
		.eth { 'ETH' }
	}
}

// Parse currency code from string.
pub fn parse_currency_code(input string) ?CurrencyCode {
	code := input.to_upper()
	return match code {
		'USD' { .usd }
		'EUR' { .eur }
		'GBP' { .gbp }
		'JPY' { .jpy }
		'CHF' { .chf }
		'CAD' { .cad }
		'AUD' { .aud }
		'NZD' { .nzd }
		'CNY' { .cny }
		'INR' { .inr }
		'BRL' { .brl }
		'MXN' { .mxn }
		'KRW' { .krw }
		'SGD' { .sgd }
		'HKD' { .hkd }
		'SEK' { .sek }
		'NOK' { .nok }
		'DKK' { .dkk }
		'PLN' { .pln }
		'RUB' { .rub }
		'ZAR' { .zar }
		'TRY' { .try_ }
		'THB' { .thb }
		'MYR' { .myr }
		'IDR' { .idr }
		'PHP' { .php }
		'VND' { .vnd }
		'AED' { .aed }
		'SAR' { .sar }
		'ILS' { .ils }
		'CZK' { .czk }
		'HUF' { .huf }
		'RON' { .ron }
		'BGN' { .bgn }
		'HRK' { .hrk }
		'ISK' { .isk }
		'CLP' { .clp }
		'COP' { .cop }
		'PEN' { .pen }
		'ARS' { .ars }
		'BTC' { .btc }
		'ETH' { .eth }
		else { none }
	}
}

// Check if string is a valid currency code.
pub fn is_valid_currency_code(input string) bool {
	_ := parse_currency_code(input) or { return false }
	return true
}

// Type-safe monetary value using minor units.
pub struct Money {
pub:
	minor_units i64          // Amount in minor units (cents, satoshis, etc.)
	currency    CurrencyCode // Currency code
}

// Create money from major units (dollars, euros, etc.).
pub fn money_from_major(amount i64, currency CurrencyCode) Money {
	multiplier := currency_multiplier(currency)
	return Money{
		minor_units: amount * multiplier
		currency: currency
	}
}

// Create money from minor units (cents, satoshis, etc.).
pub fn money_from_minor(amount i64, currency CurrencyCode) Money {
	return Money{
		minor_units: amount
		currency: currency
	}
}

// Create zero amount for currency.
pub fn money_zero(currency CurrencyCode) Money {
	return Money{
		minor_units: 0
		currency: currency
	}
}

// Get multiplier for currency based on decimals.
fn currency_multiplier(currency CurrencyCode) i64 {
	decimals := currency.decimals()
	mut result := i64(1)
	for _ in 0 .. decimals {
		result *= 10
	}
	return result
}

// Get major units (truncated).
pub fn (m Money) major() i64 {
	divisor := currency_multiplier(m.currency)
	return m.minor_units / divisor
}

// Check if zero.
pub fn (m Money) is_zero() bool {
	return m.minor_units == 0
}

// Check if positive.
pub fn (m Money) is_positive() bool {
	return m.minor_units > 0
}

// Check if negative.
pub fn (m Money) is_negative() bool {
	return m.minor_units < 0
}

// Get absolute value.
pub fn (m Money) abs() Money {
	return Money{
		minor_units: if m.minor_units < 0 { -m.minor_units } else { m.minor_units }
		currency: m.currency
	}
}

// Negate the money value.
pub fn (m Money) negate() Money {
	return Money{
		minor_units: -m.minor_units
		currency: m.currency
	}
}

// Add two monetary values (must have same currency).
pub fn (m Money) add(other Money) ?Money {
	if m.currency != other.currency {
		return none
	}
	return Money{
		minor_units: m.minor_units + other.minor_units
		currency: m.currency
	}
}

// Subtract two monetary values (must have same currency).
pub fn (m Money) sub(other Money) ?Money {
	if m.currency != other.currency {
		return none
	}
	return Money{
		minor_units: m.minor_units - other.minor_units
		currency: m.currency
	}
}

// Multiply by scalar.
pub fn (m Money) mul(scalar i64) Money {
	return Money{
		minor_units: m.minor_units * scalar
		currency: m.currency
	}
}

// Divide by scalar (returns none for division by zero).
pub fn (m Money) div(scalar i64) ?Money {
	if scalar == 0 {
		return none
	}
	return Money{
		minor_units: m.minor_units / scalar
		currency: m.currency
	}
}

// Compare two monetary values (must have same currency).
pub fn (m Money) compare(other Money) ?int {
	if m.currency != other.currency {
		return none
	}
	if m.minor_units < other.minor_units {
		return -1
	}
	if m.minor_units > other.minor_units {
		return 1
	}
	return 0
}

// Check equality with another Money value.
pub fn (m Money) equals(other Money) bool {
	return m.currency == other.currency && m.minor_units == other.minor_units
}

// Format as display string with symbol.
pub fn (m Money) str() string {
	dec := m.currency.decimals()
	divisor := currency_multiplier(m.currency)
	abs_units := if m.minor_units < 0 { -m.minor_units } else { m.minor_units }
	major_part := abs_units / divisor
	minor_part := abs_units % divisor
	sign := if m.minor_units < 0 { '-' } else { '' }
	symbol := m.currency.symbol()

	if dec == 0 {
		return '${sign}${symbol}${major_part}'
	}

	// Format minor part with leading zeros
	minor_str := format_minor_units(minor_part, dec)
	return '${sign}${symbol}${major_part}.${minor_str}'
}

// Format as string with currency code.
pub fn (m Money) to_string_with_code() string {
	dec := m.currency.decimals()
	divisor := currency_multiplier(m.currency)
	abs_units := if m.minor_units < 0 { -m.minor_units } else { m.minor_units }
	major_part := abs_units / divisor
	minor_part := abs_units % divisor
	sign := if m.minor_units < 0 { '-' } else { '' }
	code := m.currency.code_str()

	if dec == 0 {
		return '${sign}${major_part} ${code}'
	}

	minor_str := format_minor_units(minor_part, dec)
	return '${sign}${major_part}.${minor_str} ${code}'
}

// Helper: format minor units with leading zeros.
fn format_minor_units(value i64, decimals u8) string {
	mut result := '${value}'
	for result.len < int(decimals) {
		result = '0${result}'
	}
	return result
}

// Allocate money proportionally (returns array of Money).
pub fn (m Money) allocate(ratios []int) []Money {
	if ratios.len == 0 {
		return []Money{}
	}

	mut total_ratio := 0
	for r in ratios {
		total_ratio += r
	}

	if total_ratio == 0 {
		return []Money{len: ratios.len, init: money_zero(m.currency)}
	}

	mut allocated := []Money{cap: ratios.len}
	mut remainder := m.minor_units

	for i, ratio in ratios {
		if i == ratios.len - 1 {
			// Last allocation gets the remainder
			allocated << Money{
				minor_units: remainder
				currency: m.currency
			}
		} else {
			share := m.minor_units * i64(ratio) / i64(total_ratio)
			allocated << Money{
				minor_units: share
				currency: m.currency
			}
			remainder -= share
		}
	}

	return allocated
}

// Split money evenly (returns array of Money).
pub fn (m Money) split(parts int) []Money {
	if parts <= 0 {
		return []Money{}
	}

	base_amount := m.minor_units / i64(parts)
	remainder := m.minor_units % i64(parts)

	mut result := []Money{cap: parts}
	for i in 0 .. parts {
		extra := if i64(i) < remainder { i64(1) } else { i64(0) }
		result << Money{
			minor_units: base_amount + extra
			currency: m.currency
		}
	}

	return result
}
