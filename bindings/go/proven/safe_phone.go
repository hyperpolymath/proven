// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"errors"
	"fmt"
	"strings"
)

// CountryCode represents telephone country calling codes.
type CountryCode uint16

const (
	CountryCodeUnknown CountryCode = 0
	CountryCodeUS      CountryCode = 1  // USA, Canada
	CountryCodeRU      CountryCode = 7  // Russia
	CountryCodeEG      CountryCode = 20 // Egypt
	CountryCodeZA      CountryCode = 27 // South Africa
	CountryCodeGR      CountryCode = 30 // Greece
	CountryCodeNL      CountryCode = 31 // Netherlands
	CountryCodeBE      CountryCode = 32 // Belgium
	CountryCodeFR      CountryCode = 33 // France
	CountryCodeES      CountryCode = 34 // Spain
	CountryCodeHU      CountryCode = 36 // Hungary
	CountryCodeIT      CountryCode = 39 // Italy
	CountryCodeRO      CountryCode = 40 // Romania
	CountryCodeCH      CountryCode = 41 // Switzerland
	CountryCodeAT      CountryCode = 43 // Austria
	CountryCodeUK      CountryCode = 44 // United Kingdom
	CountryCodeDK      CountryCode = 45 // Denmark
	CountryCodeSE      CountryCode = 46 // Sweden
	CountryCodeNO      CountryCode = 47 // Norway
	CountryCodePL      CountryCode = 48 // Poland
	CountryCodeDE      CountryCode = 49 // Germany
	CountryCodeMX      CountryCode = 52 // Mexico
	CountryCodeBR      CountryCode = 55 // Brazil
	CountryCodeAU      CountryCode = 61 // Australia
	CountryCodeID      CountryCode = 62 // Indonesia
	CountryCodePH      CountryCode = 63 // Philippines
	CountryCodeNZ      CountryCode = 64 // New Zealand
	CountryCodeSG      CountryCode = 65 // Singapore
	CountryCodeTH      CountryCode = 66 // Thailand
	CountryCodeJP      CountryCode = 81 // Japan
	CountryCodeKR      CountryCode = 82 // South Korea
	CountryCodeVN      CountryCode = 84 // Vietnam
	CountryCodeCN      CountryCode = 86 // China
	CountryCodeTR      CountryCode = 90 // Turkey
	CountryCodeIN      CountryCode = 91 // India
	CountryCodePK      CountryCode = 92 // Pakistan
	CountryCodeAE      CountryCode = 971 // UAE
	CountryCodeSA      CountryCode = 966 // Saudi Arabia
	CountryCodeIL      CountryCode = 972 // Israel
)

var (
	// ErrPhoneEmpty indicates an empty phone number input.
	ErrPhoneEmpty = errors.New("phone number is empty")

	// ErrPhoneTooShort indicates a phone number that is too short.
	ErrPhoneTooShort = errors.New("phone number too short")

	// ErrPhoneTooLong indicates a phone number that is too long.
	ErrPhoneTooLong = errors.New("phone number too long")

	// ErrUnknownCountryCode indicates an unrecognized country code.
	ErrUnknownCountryCode = errors.New("unknown country code")

	// ErrNationalNumberTooShort indicates the national number portion is too short.
	ErrNationalNumberTooShort = errors.New("national number too short")
)

// knownCountryCodes maps numeric values to CountryCode constants.
var knownCountryCodes = map[uint16]CountryCode{
	1: CountryCodeUS, 7: CountryCodeRU, 20: CountryCodeEG, 27: CountryCodeZA,
	30: CountryCodeGR, 31: CountryCodeNL, 32: CountryCodeBE, 33: CountryCodeFR,
	34: CountryCodeES, 36: CountryCodeHU, 39: CountryCodeIT, 40: CountryCodeRO,
	41: CountryCodeCH, 43: CountryCodeAT, 44: CountryCodeUK, 45: CountryCodeDK,
	46: CountryCodeSE, 47: CountryCodeNO, 48: CountryCodePL, 49: CountryCodeDE,
	52: CountryCodeMX, 55: CountryCodeBR, 61: CountryCodeAU, 62: CountryCodeID,
	63: CountryCodePH, 64: CountryCodeNZ, 65: CountryCodeSG, 66: CountryCodeTH,
	81: CountryCodeJP, 82: CountryCodeKR, 84: CountryCodeVN, 86: CountryCodeCN,
	90: CountryCodeTR, 91: CountryCodeIN, 92: CountryCodePK, 966: CountryCodeSA,
	971: CountryCodeAE, 972: CountryCodeIL,
}

// Value returns the numeric value of the country code.
func (c CountryCode) Value() uint16 {
	return uint16(c)
}

// String returns the string representation of the country code.
func (c CountryCode) String() string {
	return fmt.Sprintf("+%d", c)
}

// CountryName returns the country name for this code.
func (c CountryCode) CountryName() string {
	switch c {
	case CountryCodeUS:
		return "United States/Canada"
	case CountryCodeRU:
		return "Russia"
	case CountryCodeFR:
		return "France"
	case CountryCodeUK:
		return "United Kingdom"
	case CountryCodeDE:
		return "Germany"
	case CountryCodeJP:
		return "Japan"
	case CountryCodeCN:
		return "China"
	case CountryCodeIN:
		return "India"
	case CountryCodeBR:
		return "Brazil"
	case CountryCodeAU:
		return "Australia"
	default:
		return "Unknown"
	}
}

// CountryCodeFromValue converts a numeric value to a CountryCode.
// Returns CountryCodeUnknown if not recognized.
func CountryCodeFromValue(value uint16) CountryCode {
	if cc, ok := knownCountryCodes[value]; ok {
		return cc
	}
	return CountryCodeUnknown
}

// PhoneNumber represents a validated phone number.
type PhoneNumber struct {
	countryCode    CountryCode
	nationalNumber string
}

// CountryCode returns the country calling code.
func (p PhoneNumber) CountryCode() CountryCode {
	return p.countryCode
}

// NationalNumber returns the national number portion.
func (p PhoneNumber) NationalNumber() string {
	return p.nationalNumber
}

// ToE164 formats the phone number in E.164 format.
func (p PhoneNumber) ToE164() string {
	return fmt.Sprintf("+%d%s", p.countryCode, p.nationalNumber)
}

// ToInternational formats the phone number with spaces.
func (p PhoneNumber) ToInternational() string {
	cc := p.countryCode
	nat := p.nationalNumber
	natLen := len(nat)

	if natLen <= 4 {
		return fmt.Sprintf("+%d %s", cc, nat)
	} else if natLen <= 7 {
		return fmt.Sprintf("+%d %s %s", cc, nat[:3], nat[3:])
	} else if natLen <= 10 {
		return fmt.Sprintf("+%d %s %s %s", cc, nat[:3], nat[3:6], nat[6:])
	}
	return fmt.Sprintf("+%d %s", cc, nat)
}

// ToNational formats the phone number in national format (without country code).
func (p PhoneNumber) ToNational() string {
	nat := p.nationalNumber
	natLen := len(nat)

	if natLen <= 4 {
		return nat
	} else if natLen <= 7 {
		return fmt.Sprintf("%s-%s", nat[:3], nat[3:])
	} else if natLen == 10 {
		return fmt.Sprintf("(%s) %s-%s", nat[:3], nat[3:6], nat[6:])
	}
	return nat
}

// DigitCount returns the total number of digits.
func (p PhoneNumber) DigitCount() int {
	ccDigits := 1
	if p.countryCode >= 100 {
		ccDigits = 3
	} else if p.countryCode >= 10 {
		ccDigits = 2
	}
	return ccDigits + len(p.nationalNumber)
}

// String returns the E.164 formatted phone number.
func (p PhoneNumber) String() string {
	return p.ToE164()
}

// ParsePhoneNumber parses a phone number from string.
// Returns nil if the format is invalid.
func ParsePhoneNumber(input string) *PhoneNumber {
	phone, err := ParsePhoneNumberStrict(input)
	if err != nil {
		return nil
	}
	return phone
}

// ParsePhoneNumberStrict parses a phone number from string.
// Returns an error if the format is invalid.
func ParsePhoneNumberStrict(input string) (*PhoneNumber, error) {
	trimmed := strings.TrimSpace(input)
	if len(trimmed) == 0 {
		return nil, ErrPhoneEmpty
	}

	// Extract digits only
	var digits strings.Builder
	for _, c := range trimmed {
		if c >= '0' && c <= '9' {
			digits.WriteRune(c)
		}
	}

	digitStr := digits.String()
	if len(digitStr) < 7 {
		return nil, ErrPhoneTooShort
	}
	if len(digitStr) > 15 {
		return nil, ErrPhoneTooLong
	}

	// Try to parse country code (3, 2, then 1 digit)
	cc, nationalStart, err := parseCountryCodeFromDigits(digitStr)
	if err != nil {
		return nil, err
	}

	nationalNumber := digitStr[nationalStart:]
	if len(nationalNumber) < 4 {
		return nil, ErrNationalNumberTooShort
	}

	return &PhoneNumber{
		countryCode:    cc,
		nationalNumber: nationalNumber,
	}, nil
}

// parseCountryCodeFromDigits attempts to parse a country code from the beginning of a digit string.
func parseCountryCodeFromDigits(digits string) (CountryCode, int, error) {
	// Try 3-digit codes first, then 2, then 1
	for _, length := range []int{3, 2, 1} {
		if len(digits) >= length {
			valueStr := digits[:length]
			var value uint16
			for _, c := range valueStr {
				value = value*10 + uint16(c-'0')
			}
			cc := CountryCodeFromValue(value)
			if cc != CountryCodeUnknown {
				return cc, length, nil
			}
		}
	}
	return CountryCodeUnknown, 0, ErrUnknownCountryCode
}

// IsValidPhoneNumber checks if a string is a valid phone number.
func IsValidPhoneNumber(input string) bool {
	return ParsePhoneNumber(input) != nil
}

// FormatPhoneE164 formats a phone number in E.164 format.
func FormatPhoneE164(phone *PhoneNumber) string {
	if phone == nil {
		return ""
	}
	return phone.ToE164()
}

// FormatPhoneInternational formats a phone number in international format.
func FormatPhoneInternational(phone *PhoneNumber) string {
	if phone == nil {
		return ""
	}
	return phone.ToInternational()
}

// FormatPhoneNational formats a phone number in national format.
func FormatPhoneNational(phone *PhoneNumber) string {
	if phone == nil {
		return ""
	}
	return phone.ToNational()
}
