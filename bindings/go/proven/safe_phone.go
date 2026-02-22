// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafePhone provides E.164 phone number handling via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// ParsedPhone represents a parsed phone number.
type ParsedPhone struct {
	CountryCode    uint16
	NationalNumber uint64
	IsValid        bool
}

// PhoneParse parses a phone number string to E.164 components.
func PhoneParse(input string) (*ParsedPhone, error) {
	cs, length := cString(input)
	defer unsafeFree(cs)

	result := C.proven_phone_parse(cs, length)
	if int(result.status) != StatusOK {
		return nil, newError(int(result.status))
	}

	return &ParsedPhone{
		CountryCode:    uint16(result.country_code),
		NationalNumber: uint64(result.national_number),
		IsValid:        bool(result.is_valid),
	}, nil
}

// PhoneFormatE164 formats a phone number as an E.164 string.
func PhoneFormatE164(countryCode uint16, nationalNumber uint64) (string, error) {
	return goStringResult(C.proven_phone_format_e164(
		C.uint16_t(countryCode),
		C.uint64_t(nationalNumber),
	))
}
