// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"crypto/subtle"
	"errors"
	"fmt"
	"strings"
)

var (
	// ErrHexOddLength indicates a hex string has an odd number of characters.
	ErrHexOddLength = errors.New("hex string has odd length")

	// ErrHexInvalidChar indicates an invalid hexadecimal character.
	ErrHexInvalidChar = errors.New("invalid hex character")

	// ErrHexInvalidNumber indicates an invalid hex number.
	ErrHexInvalidNumber = errors.New("invalid hex number")
)

// IsHexChar checks if a character is a valid hexadecimal digit.
func IsHexChar(c rune) bool {
	return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
}

// HexCharToNibble converts a hex character to its nibble value.
// Returns (value, true) on success, (0, false) on invalid input.
func HexCharToNibble(c rune) (byte, bool) {
	switch {
	case c >= '0' && c <= '9':
		return byte(c - '0'), true
	case c >= 'a' && c <= 'f':
		return byte(c - 'a' + 10), true
	case c >= 'A' && c <= 'F':
		return byte(c - 'A' + 10), true
	default:
		return 0, false
	}
}

// NibbleToHexChar converts a nibble (0-15) to a lowercase hex character.
func NibbleToHexChar(n byte) rune {
	if n < 10 {
		return rune('0' + n)
	}
	return rune('a' + n - 10)
}

// NibbleToHexCharUpper converts a nibble (0-15) to an uppercase hex character.
func NibbleToHexCharUpper(n byte) rune {
	if n < 10 {
		return rune('0' + n)
	}
	return rune('A' + n - 10)
}

// HexEncode encodes bytes to a lowercase hex string.
func HexEncode(data []byte) string {
	var result strings.Builder
	result.Grow(len(data) * 2)

	for _, b := range data {
		result.WriteRune(NibbleToHexChar(b >> 4))
		result.WriteRune(NibbleToHexChar(b & 0x0F))
	}

	return result.String()
}

// HexEncodeUpper encodes bytes to an uppercase hex string.
func HexEncodeUpper(data []byte) string {
	var result strings.Builder
	result.Grow(len(data) * 2)

	for _, b := range data {
		result.WriteRune(NibbleToHexCharUpper(b >> 4))
		result.WriteRune(NibbleToHexCharUpper(b & 0x0F))
	}

	return result.String()
}

// HexDecode decodes a hex string to bytes.
// Returns nil if the hex string is invalid.
func HexDecode(hex string) []byte {
	result, err := HexDecodeStrict(hex)
	if err != nil {
		return nil
	}
	return result
}

// HexDecodeStrict decodes a hex string to bytes.
// Returns an error if the hex string is invalid.
func HexDecodeStrict(hex string) ([]byte, error) {
	if len(hex)%2 != 0 {
		return nil, ErrHexOddLength
	}

	result := make([]byte, len(hex)/2)
	runes := []rune(hex)

	for i := 0; i < len(runes); i += 2 {
		high, ok := HexCharToNibble(runes[i])
		if !ok {
			return nil, ErrHexInvalidChar
		}
		low, ok := HexCharToNibble(runes[i+1])
		if !ok {
			return nil, ErrHexInvalidChar
		}
		result[i/2] = (high << 4) | low
	}

	return result, nil
}

// IsValidHex checks if a string contains only valid hex characters.
func IsValidHex(s string) bool {
	for _, c := range s {
		if !IsHexChar(c) {
			return false
		}
	}
	return true
}

// IsValidHexBytes checks if a string is valid hex with even length (represents complete bytes).
func IsValidHexBytes(s string) bool {
	return len(s)%2 == 0 && IsValidHex(s)
}

// HexFormatSpaced formats a hex string with spaces between bytes.
// Returns nil if the hex string has odd length.
func HexFormatSpaced(hex string) *string {
	result, err := HexFormatSpacedStrict(hex)
	if err != nil {
		return nil
	}
	return &result
}

// HexFormatSpacedStrict formats a hex string with spaces between bytes.
// Returns an error if the hex string has odd length.
func HexFormatSpacedStrict(hex string) (string, error) {
	if len(hex)%2 != 0 {
		return "", ErrHexOddLength
	}
	if len(hex) == 0 {
		return "", nil
	}

	runes := []rune(hex)
	var result strings.Builder
	result.Grow(len(hex) + len(hex)/2 - 1)

	for i := 0; i < len(runes); i += 2 {
		if i > 0 {
			result.WriteRune(' ')
		}
		result.WriteRune(runes[i])
		result.WriteRune(runes[i+1])
	}

	return result.String(), nil
}

// HexFormatColons formats a hex string with colons between bytes (like MAC addresses).
// Returns nil if the hex string has odd length.
func HexFormatColons(hex string) *string {
	result, err := HexFormatColonsStrict(hex)
	if err != nil {
		return nil
	}
	return &result
}

// HexFormatColonsStrict formats a hex string with colons between bytes.
// Returns an error if the hex string has odd length.
func HexFormatColonsStrict(hex string) (string, error) {
	if len(hex)%2 != 0 {
		return "", ErrHexOddLength
	}
	if len(hex) == 0 {
		return "", nil
	}

	runes := []rune(hex)
	var result strings.Builder
	result.Grow(len(hex) + len(hex)/2 - 1)

	for i := 0; i < len(runes); i += 2 {
		if i > 0 {
			result.WriteRune(':')
		}
		result.WriteRune(runes[i])
		result.WriteRune(runes[i+1])
	}

	return result.String(), nil
}

// HexConstantTimeEqual compares two hex strings in constant time.
// Returns true if they are equal (case-insensitive).
func HexConstantTimeEqual(a, b string) bool {
	if len(a) != len(b) {
		return false
	}

	aLower := strings.ToLower(a)
	bLower := strings.ToLower(b)

	return subtle.ConstantTimeCompare([]byte(aLower), []byte(bLower)) == 1
}

// HexConstantTimeEqualBytes compares two byte slices in constant time.
// This is a wrapper around crypto/subtle.ConstantTimeCompare.
func HexConstantTimeEqualBytes(a, b []byte) bool {
	return subtle.ConstantTimeCompare(a, b) == 1
}

// IntToHex converts an unsigned integer to a hex string with minimum width.
func IntToHex(value uint64, minWidth int) string {
	hex := fmt.Sprintf("%x", value)
	if len(hex) >= minWidth {
		return hex
	}
	return strings.Repeat("0", minWidth-len(hex)) + hex
}

// IntToHexUpper converts an unsigned integer to an uppercase hex string with minimum width.
func IntToHexUpper(value uint64, minWidth int) string {
	hex := fmt.Sprintf("%X", value)
	if len(hex) >= minWidth {
		return hex
	}
	return strings.Repeat("0", minWidth-len(hex)) + hex
}

// HexToInt parses a hex string to an unsigned integer.
// Returns (0, false) if the hex string is invalid.
func HexToInt(hex string) (uint64, bool) {
	value, err := HexToIntStrict(hex)
	if err != nil {
		return 0, false
	}
	return value, true
}

// HexToIntStrict parses a hex string to an unsigned integer.
// Returns an error if the hex string is invalid.
func HexToIntStrict(hex string) (uint64, error) {
	if len(hex) == 0 {
		return 0, ErrHexInvalidNumber
	}

	var result uint64
	for _, c := range hex {
		nibble, ok := HexCharToNibble(c)
		if !ok {
			return 0, ErrHexInvalidNumber
		}
		// Check for overflow
		if result > (^uint64(0))/16 {
			return 0, ErrHexInvalidNumber
		}
		result = result*16 + uint64(nibble)
	}

	return result, nil
}

// HexStripPrefix removes "0x" or "0X" prefix from a hex string if present.
func HexStripPrefix(hex string) string {
	if len(hex) >= 2 && (hex[:2] == "0x" || hex[:2] == "0X") {
		return hex[2:]
	}
	return hex
}

// HexAddPrefix adds "0x" prefix to a hex string if not present.
func HexAddPrefix(hex string) string {
	if len(hex) >= 2 && (hex[:2] == "0x" || hex[:2] == "0X") {
		return hex
	}
	return "0x" + hex
}
