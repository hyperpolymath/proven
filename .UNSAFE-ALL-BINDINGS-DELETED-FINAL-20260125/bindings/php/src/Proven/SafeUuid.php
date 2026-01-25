<?php
// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * UUID version types as defined by RFC 4122 and RFC 9562.
 */
enum UuidVersion: int
{
    case V1 = 1;  // Time-based
    case V3 = 3;  // Name-based (MD5)
    case V4 = 4;  // Random
    case V5 = 5;  // Name-based (SHA-1)
    case V6 = 6;  // Reordered time-based
    case V7 = 7;  // Unix epoch time-based
    case V8 = 8;  // Custom

    /**
     * Get a human-readable description of this version.
     *
     * @return string Description of the UUID version
     */
    public function description(): string
    {
        return match ($this) {
            self::V1 => 'Time-based (MAC address)',
            self::V3 => 'Name-based (MD5 hash)',
            self::V4 => 'Random',
            self::V5 => 'Name-based (SHA-1 hash)',
            self::V6 => 'Reordered time-based',
            self::V7 => 'Unix epoch time-based',
            self::V8 => 'Custom',
        };
    }
}

/**
 * UUID variant types as defined by RFC 4122.
 */
enum UuidVariant: int
{
    case NCS = 0;           // Reserved for NCS backward compatibility
    case RFC4122 = 1;       // RFC 4122 (standard)
    case Microsoft = 2;     // Reserved for Microsoft backward compatibility
    case Future = 3;        // Reserved for future definition

    /**
     * Get a human-readable description of this variant.
     *
     * @return string Description of the UUID variant
     */
    public function description(): string
    {
        return match ($this) {
            self::NCS => 'Reserved (NCS backward compatibility)',
            self::RFC4122 => 'RFC 4122 (standard)',
            self::Microsoft => 'Reserved (Microsoft backward compatibility)',
            self::Future => 'Reserved (future definition)',
        };
    }
}

/**
 * Immutable UUID value object with safe parsing and formatting.
 */
readonly class Uuid
{
    /** @var string The 16-byte binary representation */
    private string $bytes;

    /**
     * Create a UUID from 16 raw bytes.
     *
     * @param string $bytes 16-byte binary string
     * @throws \InvalidArgumentException If bytes is not exactly 16 bytes
     */
    private function __construct(string $bytes)
    {
        if (strlen($bytes) !== 16) {
            throw new \InvalidArgumentException('UUID must be exactly 16 bytes');
        }
        $this->bytes = $bytes;
    }

    /**
     * Parse a UUID from a string representation.
     *
     * Accepts formats:
     * - Standard: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
     * - Compact: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
     * - URN: urn:uuid:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
     * - Braced: {xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}
     *
     * @param string $uuidString The UUID string to parse
     * @return self|null The parsed UUID or null if invalid
     */
    public static function parse(string $uuidString): ?self
    {
        $input = trim($uuidString);

        // Handle URN format
        if (str_starts_with(strtolower($input), 'urn:uuid:')) {
            $input = substr($input, 9);
        }

        // Handle braced format
        if (str_starts_with($input, '{') && str_ends_with($input, '}')) {
            $input = substr($input, 1, -1);
        }

        // Remove hyphens for compact parsing
        $hex = str_replace('-', '', $input);

        // Validate hex string
        if (strlen($hex) !== 32 || !ctype_xdigit($hex)) {
            return null;
        }

        $bytes = hex2bin($hex);
        if ($bytes === false || strlen($bytes) !== 16) {
            return null;
        }

        return new self($bytes);
    }

    /**
     * Create a UUID from 16 raw bytes.
     *
     * @param string $bytes 16-byte binary string
     * @return self|null The UUID or null if invalid length
     */
    public static function fromBytes(string $bytes): ?self
    {
        if (strlen($bytes) !== 16) {
            return null;
        }
        return new self($bytes);
    }

    /**
     * Generate a random version 4 UUID.
     *
     * @return self A new random UUID
     */
    public static function v4(): self
    {
        $bytes = random_bytes(16);

        // Set version to 4 (random)
        $bytes[6] = chr((ord($bytes[6]) & 0x0F) | 0x40);

        // Set variant to RFC 4122
        $bytes[8] = chr((ord($bytes[8]) & 0x3F) | 0x80);

        return new self($bytes);
    }

    /**
     * Generate a version 7 UUID (Unix epoch time-based).
     *
     * @return self A new time-based UUID
     */
    public static function v7(): self
    {
        // Get current time in milliseconds
        $timestampMs = (int)(microtime(true) * 1000);

        // Pack timestamp into first 6 bytes (48 bits)
        $bytes = pack('J', $timestampMs);
        $bytes = substr($bytes, 2, 6); // Take last 6 bytes (48 bits)

        // Add 10 random bytes
        $bytes .= random_bytes(10);

        // Set version to 7
        $bytes[6] = chr((ord($bytes[6]) & 0x0F) | 0x70);

        // Set variant to RFC 4122
        $bytes[8] = chr((ord($bytes[8]) & 0x3F) | 0x80);

        return new self($bytes);
    }

    /**
     * Create the nil UUID (all zeros).
     *
     * @return self The nil UUID
     */
    public static function nil(): self
    {
        return new self(str_repeat("\x00", 16));
    }

    /**
     * Create the max UUID (all ones).
     *
     * @return self The max UUID
     */
    public static function max(): self
    {
        return new self(str_repeat("\xFF", 16));
    }

    /**
     * Format the UUID as a standard string.
     *
     * @return string The UUID in format xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
     */
    public function format(): string
    {
        $hex = bin2hex($this->bytes);
        return sprintf(
            '%s-%s-%s-%s-%s',
            substr($hex, 0, 8),
            substr($hex, 8, 4),
            substr($hex, 12, 4),
            substr($hex, 16, 4),
            substr($hex, 20, 12)
        );
    }

    /**
     * Format the UUID as a compact string (no hyphens).
     *
     * @return string The UUID as 32 hex characters
     */
    public function formatCompact(): string
    {
        return bin2hex($this->bytes);
    }

    /**
     * Format the UUID as a URN.
     *
     * @return string The UUID as urn:uuid:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
     */
    public function formatUrn(): string
    {
        return 'urn:uuid:' . $this->format();
    }

    /**
     * Format the UUID in Microsoft braced format.
     *
     * @return string The UUID as {xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}
     */
    public function formatBraced(): string
    {
        return '{' . $this->format() . '}';
    }

    /**
     * Get the raw 16-byte binary representation.
     *
     * @return string 16-byte binary string
     */
    public function toBytes(): string
    {
        return $this->bytes;
    }

    /**
     * Get the UUID version.
     *
     * @return UuidVersion|null The version or null if unknown
     */
    public function getVersion(): ?UuidVersion
    {
        $versionNibble = (ord($this->bytes[6]) >> 4) & 0x0F;
        return UuidVersion::tryFrom($versionNibble);
    }

    /**
     * Get the UUID variant.
     *
     * @return UuidVariant The variant
     */
    public function getVariant(): UuidVariant
    {
        $variantByte = ord($this->bytes[8]);

        if (($variantByte & 0x80) === 0x00) {
            return UuidVariant::NCS;
        }
        if (($variantByte & 0xC0) === 0x80) {
            return UuidVariant::RFC4122;
        }
        if (($variantByte & 0xE0) === 0xC0) {
            return UuidVariant::Microsoft;
        }
        return UuidVariant::Future;
    }

    /**
     * Check if this is the nil UUID.
     *
     * @return bool True if all bytes are zero
     */
    public function isNil(): bool
    {
        return $this->bytes === str_repeat("\x00", 16);
    }

    /**
     * Check if this is the max UUID.
     *
     * @return bool True if all bytes are 0xFF
     */
    public function isMax(): bool
    {
        return $this->bytes === str_repeat("\xFF", 16);
    }

    /**
     * Compare two UUIDs for equality.
     *
     * @param self $other The UUID to compare with
     * @return bool True if equal
     */
    public function equals(self $other): bool
    {
        return hash_equals($this->bytes, $other->bytes);
    }

    /**
     * Compare two UUIDs for ordering.
     *
     * @param self $other The UUID to compare with
     * @return int -1, 0, or 1
     */
    public function compare(self $other): int
    {
        return strcmp($this->bytes, $other->bytes) <=> 0;
    }

    /**
     * Get the string representation.
     *
     * @return string The formatted UUID
     */
    public function __toString(): string
    {
        return $this->format();
    }
}

/**
 * Safe UUID operations with validation and parsing utilities.
 */
class SafeUuid
{
    /**
     * Check if a string is a valid UUID.
     *
     * @param string $uuidString The string to validate
     * @return bool True if valid UUID
     */
    public static function isValid(string $uuidString): bool
    {
        return Uuid::parse($uuidString) !== null;
    }

    /**
     * Parse a UUID string, returning null on failure.
     *
     * @param string $uuidString The string to parse
     * @return Uuid|null The parsed UUID or null
     */
    public static function parse(string $uuidString): ?Uuid
    {
        return Uuid::parse($uuidString);
    }

    /**
     * Generate a new random UUID (v4).
     *
     * @return Uuid A new random UUID
     */
    public static function generate(): Uuid
    {
        return Uuid::v4();
    }

    /**
     * Generate a new time-based UUID (v7).
     *
     * @return Uuid A new time-based UUID
     */
    public static function generateV7(): Uuid
    {
        return Uuid::v7();
    }

    /**
     * Normalize a UUID string to standard format.
     *
     * @param string $uuidString The UUID string to normalize
     * @return string|null The normalized UUID or null if invalid
     */
    public static function normalize(string $uuidString): ?string
    {
        $uuid = Uuid::parse($uuidString);
        return $uuid?->format();
    }

    /**
     * Compare two UUID strings for equality (case-insensitive).
     *
     * @param string $uuidStringA First UUID string
     * @param string $uuidStringB Second UUID string
     * @return bool True if equal
     */
    public static function equals(string $uuidStringA, string $uuidStringB): bool
    {
        $uuidA = Uuid::parse($uuidStringA);
        $uuidB = Uuid::parse($uuidStringB);

        if ($uuidA === null || $uuidB === null) {
            return false;
        }

        return $uuidA->equals($uuidB);
    }
}
