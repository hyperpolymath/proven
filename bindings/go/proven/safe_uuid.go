// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"crypto/rand"
	"errors"
	"fmt"
)

// UuidVersion represents a UUID version type.
type UuidVersion int

const (
	UuidVersionNil UuidVersion = iota
	UuidVersionV1              // Time-based
	UuidVersionV2              // DCE Security
	UuidVersionV3              // Name-based (MD5)
	UuidVersionV4              // Random
	UuidVersionV5              // Name-based (SHA-1)
)

// String returns the string representation of the UUID version.
func (v UuidVersion) String() string {
	switch v {
	case UuidVersionV1:
		return "v1"
	case UuidVersionV2:
		return "v2"
	case UuidVersionV3:
		return "v3"
	case UuidVersionV4:
		return "v4"
	case UuidVersionV5:
		return "v5"
	default:
		return "nil"
	}
}

// UuidVariant represents a UUID variant type.
type UuidVariant int

const (
	UuidVariantNcs UuidVariant = iota
	UuidVariantRfc4122
	UuidVariantMicrosoft
	UuidVariantFuture
)

// String returns the string representation of the UUID variant.
func (v UuidVariant) String() string {
	switch v {
	case UuidVariantNcs:
		return "NCS"
	case UuidVariantRfc4122:
		return "RFC4122"
	case UuidVariantMicrosoft:
		return "Microsoft"
	case UuidVariantFuture:
		return "Future"
	default:
		return "Unknown"
	}
}

// UUID represents a validated UUID (128 bits).
type UUID [16]byte

var (
	// UuidNil is the nil UUID (all zeros).
	UuidNil = UUID{}

	// UuidNamespaceDNS is the DNS namespace UUID.
	UuidNamespaceDNS = UUID{
		0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
		0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
	}

	// UuidNamespaceURL is the URL namespace UUID.
	UuidNamespaceURL = UUID{
		0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
		0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
	}

	// ErrInvalidUuidFormat indicates an invalid UUID format.
	ErrInvalidUuidFormat = errors.New("invalid UUID format")

	// ErrInvalidUuidLength indicates an invalid UUID length.
	ErrInvalidUuidLength = errors.New("UUID must be 36 characters")

	// ErrInvalidHexCharacter indicates an invalid hex character in UUID.
	ErrInvalidHexCharacter = errors.New("invalid hex character in UUID")
)

// Version returns the UUID version.
func (uuid UUID) Version() UuidVersion {
	version := (uuid[6] >> 4) & 0x0F
	switch version {
	case 1:
		return UuidVersionV1
	case 2:
		return UuidVersionV2
	case 3:
		return UuidVersionV3
	case 4:
		return UuidVersionV4
	case 5:
		return UuidVersionV5
	default:
		return UuidVersionNil
	}
}

// Variant returns the UUID variant.
func (uuid UUID) Variant() UuidVariant {
	byteVal := uuid[8]
	if (byteVal >> 7) == 0 {
		return UuidVariantNcs
	} else if (byteVal >> 6) == 0b10 {
		return UuidVariantRfc4122
	} else if (byteVal >> 5) == 0b110 {
		return UuidVariantMicrosoft
	}
	return UuidVariantFuture
}

// IsNil checks if this is the nil UUID.
func (uuid UUID) IsNil() bool {
	for _, b := range uuid {
		if b != 0 {
			return false
		}
	}
	return true
}

// String formats the UUID as a canonical string.
func (uuid UUID) String() string {
	return fmt.Sprintf(
		"%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x",
		uuid[0], uuid[1], uuid[2], uuid[3],
		uuid[4], uuid[5],
		uuid[6], uuid[7],
		uuid[8], uuid[9],
		uuid[10], uuid[11], uuid[12], uuid[13], uuid[14], uuid[15],
	)
}

// ToURN formats the UUID as a URN.
func (uuid UUID) ToURN() string {
	return "urn:uuid:" + uuid.String()
}

// Bytes returns the UUID as a byte slice.
func (uuid UUID) Bytes() []byte {
	return uuid[:]
}

// ParseUUID parses a UUID from canonical string format.
// Returns nil if the format is invalid.
func ParseUUID(s string) *UUID {
	uuid, err := ParseUUIDStrict(s)
	if err != nil {
		return nil
	}
	return uuid
}

// ParseUUIDStrict parses a UUID from canonical string format.
// Returns an error if the format is invalid.
func ParseUUIDStrict(s string) (*UUID, error) {
	if len(s) != 36 {
		return nil, ErrInvalidUuidLength
	}

	if s[8] != '-' || s[13] != '-' || s[18] != '-' || s[23] != '-' {
		return nil, ErrInvalidUuidFormat
	}

	hexStr := make([]byte, 0, 32)
	for i, c := range s {
		if i == 8 || i == 13 || i == 18 || i == 23 {
			continue
		}
		hexStr = append(hexStr, byte(c))
	}

	if len(hexStr) != 32 {
		return nil, ErrInvalidUuidLength
	}

	var uuid UUID
	for i := 0; i < 16; i++ {
		high, ok := hexCharToNibble(hexStr[i*2])
		if !ok {
			return nil, ErrInvalidHexCharacter
		}
		low, ok := hexCharToNibble(hexStr[i*2+1])
		if !ok {
			return nil, ErrInvalidHexCharacter
		}
		uuid[i] = (high << 4) | low
	}

	return &uuid, nil
}

// hexCharToNibble converts a hex character to its nibble value.
func hexCharToNibble(c byte) (byte, bool) {
	switch {
	case c >= '0' && c <= '9':
		return c - '0', true
	case c >= 'a' && c <= 'f':
		return c - 'a' + 10, true
	case c >= 'A' && c <= 'F':
		return c - 'A' + 10, true
	default:
		return 0, false
	}
}

// IsValidUUID checks if a string is a valid UUID format.
func IsValidUUID(s string) bool {
	return ParseUUID(s) != nil
}

// NewUUIDv4 generates a v4 (random) UUID.
// Returns an error if random number generation fails.
func NewUUIDv4() (*UUID, error) {
	var randomBytes [16]byte
	_, err := rand.Read(randomBytes[:])
	if err != nil {
		return nil, err
	}
	return UUIDv4FromBytes(randomBytes), nil
}

// UUIDv4FromBytes creates a v4 UUID from provided random bytes.
func UUIDv4FromBytes(randomBytes [16]byte) *UUID {
	uuid := UUID(randomBytes)
	// Set version to 4
	uuid[6] = (uuid[6] & 0x0F) | 0x40
	// Set variant to RFC 4122
	uuid[8] = (uuid[8] & 0x3F) | 0x80
	return &uuid
}

// UUIDFromBytes creates a UUID from a byte array.
func UUIDFromBytes(bytes [16]byte) UUID {
	return UUID(bytes)
}

// FormatUUID formats a UUID as a string.
func FormatUUID(uuid *UUID) string {
	if uuid == nil {
		return ""
	}
	return uuid.String()
}

// FormatUUIDURN formats a UUID as a URN.
func FormatUUIDURN(uuid *UUID) string {
	if uuid == nil {
		return ""
	}
	return uuid.ToURN()
}
