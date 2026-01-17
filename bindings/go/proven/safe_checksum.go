// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

// CRC32 computes CRC-32 (IEEE polynomial).
func CRC32(data []byte) uint32 {
	var crc uint32 = 0xFFFFFFFF

	for _, b := range data {
		crc ^= uint32(b)
		for i := 0; i < 8; i++ {
			if crc&1 != 0 {
				crc = (crc >> 1) ^ 0xEDB88320
			} else {
				crc >>= 1
			}
		}
	}

	return ^crc
}

// CRC32String computes CRC-32 of a string.
func CRC32String(s string) uint32 {
	return CRC32([]byte(s))
}

// Adler32 computes Adler-32 checksum.
func Adler32(data []byte) uint32 {
	const mod = 65521
	var a, b uint32 = 1, 0

	for _, d := range data {
		a = (a + uint32(d)) % mod
		b = (b + a) % mod
	}

	return (b << 16) | a
}

// Adler32String computes Adler-32 of a string.
func Adler32String(s string) uint32 {
	return Adler32([]byte(s))
}

// FNV1a32 computes FNV-1a 32-bit hash.
func FNV1a32(data []byte) uint32 {
	const (
		offset32 = 2166136261
		prime32  = 16777619
	)

	hash := uint32(offset32)
	for _, b := range data {
		hash ^= uint32(b)
		hash *= prime32
	}
	return hash
}

// FNV1a32String computes FNV-1a 32-bit of a string.
func FNV1a32String(s string) uint32 {
	return FNV1a32([]byte(s))
}

// FNV1a64 computes FNV-1a 64-bit hash.
func FNV1a64(data []byte) uint64 {
	const (
		offset64 = 14695981039346656037
		prime64  = 1099511628211
	)

	hash := uint64(offset64)
	for _, b := range data {
		hash ^= uint64(b)
		hash *= prime64
	}
	return hash
}

// FNV1a64String computes FNV-1a 64-bit of a string.
func FNV1a64String(s string) uint64 {
	return FNV1a64([]byte(s))
}

// DJB2 computes djb2 hash.
func DJB2(data []byte) uint32 {
	hash := uint32(5381)
	for _, b := range data {
		hash = ((hash << 5) + hash) + uint32(b)
	}
	return hash
}

// DJB2String computes djb2 of a string.
func DJB2String(s string) uint32 {
	return DJB2([]byte(s))
}

// SDBM computes sdbm hash.
func SDBM(data []byte) uint32 {
	var hash uint32
	for _, b := range data {
		hash = uint32(b) + (hash << 6) + (hash << 16) - hash
	}
	return hash
}

// SDBMString computes sdbm of a string.
func SDBMString(s string) uint32 {
	return SDBM([]byte(s))
}

// Fletcher16 computes Fletcher-16 checksum.
func Fletcher16(data []byte) uint16 {
	var sum1, sum2 uint16

	for _, b := range data {
		sum1 = (sum1 + uint16(b)) % 255
		sum2 = (sum2 + sum1) % 255
	}

	return (sum2 << 8) | sum1
}

// Luhn validates a number using Luhn algorithm.
func Luhn(number string) bool {
	var sum int
	double := false

	for i := len(number) - 1; i >= 0; i-- {
		c := number[i]
		if c < '0' || c > '9' {
			return false
		}

		digit := int(c - '0')

		if double {
			digit *= 2
			if digit > 9 {
				digit -= 9
			}
		}

		sum += digit
		double = !double
	}

	return sum%10 == 0
}

// LuhnGenerate generates check digit for a number.
func LuhnGenerate(number string) (byte, bool) {
	for _, c := range number {
		if c < '0' || c > '9' {
			return 0, false
		}
	}

	for checkDigit := byte('0'); checkDigit <= '9'; checkDigit++ {
		if Luhn(number + string(checkDigit)) {
			return checkDigit, true
		}
	}

	return 0, false
}

// XOR computes XOR checksum.
func XOR(data []byte) byte {
	var result byte
	for _, b := range data {
		result ^= b
	}
	return result
}

// Parity computes parity bit (1 if odd number of 1s).
func Parity(data []byte) byte {
	var count int
	for _, b := range data {
		for b != 0 {
			count++
			b &= b - 1
		}
	}
	if count%2 == 1 {
		return 1
	}
	return 0
}

// ISBN10Check validates ISBN-10 check digit.
func ISBN10Check(isbn string) bool {
	if len(isbn) != 10 {
		return false
	}

	var sum int
	for i := 0; i < 9; i++ {
		c := isbn[i]
		if c < '0' || c > '9' {
			return false
		}
		sum += int(c-'0') * (10 - i)
	}

	lastChar := isbn[9]
	var last int
	if lastChar == 'X' || lastChar == 'x' {
		last = 10
	} else if lastChar >= '0' && lastChar <= '9' {
		last = int(lastChar - '0')
	} else {
		return false
	}

	sum += last
	return sum%11 == 0
}

// ISBN13Check validates ISBN-13 check digit.
func ISBN13Check(isbn string) bool {
	if len(isbn) != 13 {
		return false
	}

	var sum int
	for i := 0; i < 13; i++ {
		c := isbn[i]
		if c < '0' || c > '9' {
			return false
		}
		digit := int(c - '0')
		if i%2 == 0 {
			sum += digit
		} else {
			sum += digit * 3
		}
	}

	return sum%10 == 0
}
