// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"crypto/subtle"
)

// ConstantTimeCompare compares two byte slices in constant time to prevent timing attacks.
// Uses Go's crypto/subtle.ConstantTimeCompare for correctness.
func ConstantTimeCompare(a, b []byte) bool {
	return subtle.ConstantTimeCompare(a, b) == 1
}

// ConstantTimeCompareString compares two strings in constant time to prevent timing attacks.
func ConstantTimeCompareString(a, b string) bool {
	return ConstantTimeCompare([]byte(a), []byte(b))
}

// SecureZero securely zeros out a byte slice to prevent data leakage.
func SecureZero(data []byte) {
	for i := range data {
		data[i] = 0
	}
}

// SecureZeroRunes securely zeros out a rune slice (for password handling).
func SecureZeroRunes(data []rune) {
	for i := range data {
		data[i] = 0
	}
}

// ConstantTimeSelect returns x if v is 1, or y if v is 0.
// Uses constant-time operations.
func ConstantTimeSelect(v int, x, y int) int {
	return subtle.ConstantTimeSelect(v, x, y)
}

// ConstantTimeByteEq returns 1 if x == y, 0 otherwise.
// Uses constant-time operations.
func ConstantTimeByteEq(x, y uint8) int {
	return subtle.ConstantTimeByteEq(x, y)
}
