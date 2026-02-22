// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeCurrency provides monetary value operations with ISO 4217 codes via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// ParsedCurrency represents a parsed monetary value.
type ParsedCurrency struct {
	AmountMinor   int64
	CurrencyCode  [3]byte
	DecimalPlaces uint8
}

// CurrencyParse parses a currency amount string (e.g., "USD 123.45" or "123.45 EUR").
func CurrencyParse(input string) (*ParsedCurrency, error) {
	cs, length := cString(input)
	defer unsafeFree(cs)

	result := C.proven_currency_parse(cs, length)
	if int(result.status) != StatusOK {
		return nil, newError(int(result.status))
	}

	return &ParsedCurrency{
		AmountMinor: int64(result.amount_minor),
		CurrencyCode: [3]byte{
			byte(result.currency_code[0]),
			byte(result.currency_code[1]),
			byte(result.currency_code[2]),
		},
		DecimalPlaces: uint8(result.decimal_places),
	}, nil
}

// CurrencyFormat formats a currency amount as a string.
func CurrencyFormat(amountMinor int64, code [3]byte, decimalPlaces uint8) (string, error) {
	var cCode [3]C.uint8_t
	cCode[0] = C.uint8_t(code[0])
	cCode[1] = C.uint8_t(code[1])
	cCode[2] = C.uint8_t(code[2])
	return goStringResult(C.proven_currency_format(
		C.int64_t(amountMinor),
		cCode,
		C.uint8_t(decimalPlaces),
	))
}
