// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Safe UUID operations following RFC 4122.
public enum SafeUUID {
    // MARK: - UUID Version

    /// UUID version enumeration (RFC 4122).
    public enum UUIDVersion: Int, Equatable, Hashable, CustomStringConvertible {
        case v1 = 1    // Time-based
        case v2 = 2    // DCE Security
        case v3 = 3    // Name-based (MD5)
        case v4 = 4    // Random
        case v5 = 5    // Name-based (SHA-1)
        case vNil = 0  // Nil UUID (all zeros)

        public var description: String {
            switch self {
            case .v1: return "v1"
            case .v2: return "v2"
            case .v3: return "v3"
            case .v4: return "v4"
            case .v5: return "v5"
            case .vNil: return "nil"
            }
        }
    }

    // MARK: - UUID Variant

    /// UUID variant enumeration (RFC 4122).
    public enum UUIDVariant: Equatable, Hashable, CustomStringConvertible {
        case ncs           // Reserved for NCS backward compatibility
        case rfc4122       // Standard RFC 4122 variant
        case microsoft     // Reserved for Microsoft backward compatibility
        case future        // Reserved for future definition

        public var description: String {
            switch self {
            case .ncs: return "NCS"
            case .rfc4122: return "RFC4122"
            case .microsoft: return "Microsoft"
            case .future: return "Future"
            }
        }
    }

    // MARK: - UUID Errors

    /// UUID parsing and validation errors.
    public enum UUIDError: Error, Equatable, CustomStringConvertible {
        case invalidLength(Int)
        case invalidCharacter(Character, Int)
        case invalidFormat(String)
        case invalidVersion(Int)
        case invalidVariant

        public var description: String {
            switch self {
            case .invalidLength(let length):
                return "Invalid UUID length: \(length) (expected 36)"
            case .invalidCharacter(let char, let position):
                return "Invalid character '\(char)' at position \(position)"
            case .invalidFormat(let message):
                return "Invalid UUID format: \(message)"
            case .invalidVersion(let version):
                return "Invalid UUID version: \(version)"
            case .invalidVariant:
                return "Invalid UUID variant"
            }
        }
    }

    // MARK: - UUID Structure

    /// A validated UUID with 128-bit value stored as 16 bytes.
    public struct UUID: Equatable, Hashable, CustomStringConvertible, Comparable {
        /// The 16 bytes of the UUID.
        public let bytes: [UInt8]

        /// Create UUID from exactly 16 bytes.
        public init?(bytes: [UInt8]) {
            guard bytes.count == 16 else { return nil }
            self.bytes = bytes
        }

        /// Create UUID from a validated byte array (internal use).
        internal init(validatedBytes: [UInt8]) {
            self.bytes = validatedBytes
        }

        public var description: String {
            format()
        }

        public static func < (lhs: UUID, rhs: UUID) -> Bool {
            for i in 0..<16 {
                if lhs.bytes[i] < rhs.bytes[i] { return true }
                if lhs.bytes[i] > rhs.bytes[i] { return false }
            }
            return false
        }
    }

    // MARK: - UUID Constants

    /// The nil UUID (all zeros).
    public static let nilUUID = UUID(validatedBytes: Array(repeating: 0, count: 16))

    /// DNS namespace UUID for v3/v5 UUIDs.
    public static let namespaceDNS = UUID(validatedBytes: [
        0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
    ])

    /// URL namespace UUID for v3/v5 UUIDs.
    public static let namespaceURL = UUID(validatedBytes: [
        0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
    ])

    /// OID namespace UUID for v3/v5 UUIDs.
    public static let namespaceOID = UUID(validatedBytes: [
        0x6b, 0xa7, 0xb8, 0x12, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
    ])

    /// X.500 namespace UUID for v3/v5 UUIDs.
    public static let namespaceX500 = UUID(validatedBytes: [
        0x6b, 0xa7, 0xb8, 0x14, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
    ])

    // MARK: - UUID Properties

    /// Extract the version from a UUID.
    public static func version(_ uuid: UUID) -> UUIDVersion {
        let versionByte = uuid.bytes[6]
        let versionValue = Int((versionByte >> 4) & 0x0F)
        return UUIDVersion(rawValue: versionValue) ?? .vNil
    }

    /// Extract the variant from a UUID.
    public static func variant(_ uuid: UUID) -> UUIDVariant {
        let variantByte = uuid.bytes[8]
        if (variantByte >> 7) == 0 {
            return .ncs
        } else if (variantByte >> 6) == 0b10 {
            return .rfc4122
        } else if (variantByte >> 5) == 0b110 {
            return .microsoft
        } else {
            return .future
        }
    }

    /// Check if UUID is the nil UUID (all zeros).
    public static func isNil(_ uuid: UUID) -> Bool {
        uuid.bytes.allSatisfy { $0 == 0 }
    }

    // MARK: - UUID Parsing

    /// Check if character is valid hexadecimal.
    private static func isHexChar(_ char: Character) -> Bool {
        char.isHexDigit
    }

    /// Parse hex character to value (0-15).
    private static func hexValue(_ char: Character) -> UInt8? {
        switch char {
        case "0"..."9":
            return UInt8(char.asciiValue! - Character("0").asciiValue!)
        case "a"..."f":
            return UInt8(char.asciiValue! - Character("a").asciiValue! + 10)
        case "A"..."F":
            return UInt8(char.asciiValue! - Character("A").asciiValue! + 10)
        default:
            return nil
        }
    }

    /// Parse two hex characters to a byte.
    private static func parseHexByte(_ high: Character, _ low: Character) -> UInt8? {
        guard let highValue = hexValue(high),
              let lowValue = hexValue(low) else {
            return nil
        }
        return (highValue << 4) | lowValue
    }

    /// Parse UUID from canonical format (8-4-4-4-12).
    /// Example: "550e8400-e29b-41d4-a716-446655440000"
    public static func parse(_ string: String) -> Result<UUID, UUIDError> {
        let chars = Array(string)

        guard chars.count == 36 else {
            return .failure(.invalidLength(chars.count))
        }

        // Check hyphen positions: 8, 13, 18, 23
        guard chars[8] == "-" && chars[13] == "-" && chars[18] == "-" && chars[23] == "-" else {
            return .failure(.invalidFormat("missing or misplaced hyphens"))
        }

        // Extract hex characters (skip hyphens)
        var hexChars: [Character] = []
        for (index, char) in chars.enumerated() {
            if index == 8 || index == 13 || index == 18 || index == 23 {
                continue
            }
            guard isHexChar(char) else {
                return .failure(.invalidCharacter(char, index))
            }
            hexChars.append(char)
        }

        guard hexChars.count == 32 else {
            return .failure(.invalidFormat("wrong hex character count"))
        }

        // Parse hex pairs to bytes
        var bytes: [UInt8] = []
        for i in stride(from: 0, to: 32, by: 2) {
            guard let byte = parseHexByte(hexChars[i], hexChars[i + 1]) else {
                return .failure(.invalidFormat("hex parse failed"))
            }
            bytes.append(byte)
        }

        return .success(UUID(validatedBytes: bytes))
    }

    /// Parse UUID from string, returning nil on failure.
    public static func parse(_ string: String) -> UUID? {
        switch parse(string) as Result<UUID, UUIDError> {
        case .success(let uuid): return uuid
        case .failure: return nil
        }
    }

    // MARK: - UUID Formatting

    /// Hex digit from nibble (0-15).
    private static func hexDigit(_ nibble: UInt8) -> Character {
        let chars: [Character] = ["0", "1", "2", "3", "4", "5", "6", "7",
                                   "8", "9", "a", "b", "c", "d", "e", "f"]
        return chars[Int(nibble & 0x0F)]
    }

    /// Format byte as two hex characters.
    private static func formatByte(_ byte: UInt8) -> String {
        String(hexDigit(byte >> 4)) + String(hexDigit(byte & 0x0F))
    }

    /// Format UUID in canonical form (8-4-4-4-12 with lowercase).
    public static func format(_ uuid: UUID) -> String {
        let hexBytes = uuid.bytes.map { formatByte($0) }
        return hexBytes[0...3].joined() + "-" +
               hexBytes[4...5].joined() + "-" +
               hexBytes[6...7].joined() + "-" +
               hexBytes[8...9].joined() + "-" +
               hexBytes[10...15].joined()
    }

    /// Format UUID as URN (urn:uuid:...).
    public static func formatURN(_ uuid: UUID) -> String {
        "urn:uuid:" + format(uuid)
    }

    /// Format UUID without hyphens.
    public static func formatCompact(_ uuid: UUID) -> String {
        uuid.bytes.map { formatByte($0) }.joined()
    }

    /// Format UUID with uppercase.
    public static func formatUppercase(_ uuid: UUID) -> String {
        format(uuid).uppercased()
    }

    // MARK: - UUID Validation

    /// Validate UUID string format.
    public static func isValid(_ string: String) -> Bool {
        switch parse(string) as Result<UUID, UUIDError> {
        case .success: return true
        case .failure: return false
        }
    }

    /// Quick check if string looks like a UUID (checks length and hyphen positions only).
    public static func looksLikeUUID(_ string: String) -> Bool {
        guard string.count == 36 else { return false }
        let chars = Array(string)
        return chars[8] == "-" && chars[13] == "-" && chars[18] == "-" && chars[23] == "-"
    }

    // MARK: - UUID Generation

    /// Create UUID v4 from 16 random bytes.
    /// Sets the version and variant bits appropriately.
    public static func v4FromBytes(_ randomBytes: [UInt8]) -> UUID? {
        guard randomBytes.count == 16 else { return nil }
        var bytes = randomBytes

        // Set version to 4 (bits 4-7 of byte 6)
        bytes[6] = (bytes[6] & 0x0F) | 0x40

        // Set variant to RFC 4122 (bits 6-7 of byte 8)
        bytes[8] = (bytes[8] & 0x3F) | 0x80

        return UUID(validatedBytes: bytes)
    }

    /// Generate a random v4 UUID using system random.
    public static func generateV4() -> UUID {
        var bytes = [UInt8](repeating: 0, count: 16)
        for i in 0..<16 {
            bytes[i] = UInt8.random(in: 0...255)
        }
        return v4FromBytes(bytes)!
    }
}

// MARK: - UUID Extension Methods

extension SafeUUID.UUID {
    /// Get the version of this UUID.
    public var version: SafeUUID.UUIDVersion {
        SafeUUID.version(self)
    }

    /// Get the variant of this UUID.
    public var variant: SafeUUID.UUIDVariant {
        SafeUUID.variant(self)
    }

    /// Check if this is the nil UUID.
    public var isNil: Bool {
        SafeUUID.isNil(self)
    }

    /// Format this UUID in canonical form.
    public func format() -> String {
        SafeUUID.format(self)
    }

    /// Format this UUID as URN.
    public func formatURN() -> String {
        SafeUUID.formatURN(self)
    }

    /// Format this UUID without hyphens.
    public func formatCompact() -> String {
        SafeUUID.formatCompact(self)
    }

    /// Format this UUID with uppercase.
    public func formatUppercase() -> String {
        SafeUUID.formatUppercase(self)
    }
}
