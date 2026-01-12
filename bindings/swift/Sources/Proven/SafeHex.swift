// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Safe hexadecimal encoding and decoding utilities.
public enum SafeHex {
    // MARK: - Hex Errors

    /// Hex encoding/decoding errors.
    public enum HexError: Error, Equatable, CustomStringConvertible {
        case invalidCharacter(Character, Int)  // Invalid char at position
        case oddLength(Int)                    // Hex string has odd length
        case emptyInput
        case decodingFailed(String)

        public var description: String {
            switch self {
            case .invalidCharacter(let char, let position):
                return "Invalid hex character '\(char)' at position \(position)"
            case .oddLength(let length):
                return "Hex string has odd length: \(length)"
            case .emptyInput:
                return "Empty input"
            case .decodingFailed(let message):
                return "Hex decoding failed: \(message)"
            }
        }
    }

    // MARK: - Hex Character Operations

    /// Check if character is valid hexadecimal (0-9, a-f, A-F).
    public static func isHexChar(_ char: Character) -> Bool {
        char.isHexDigit
    }

    /// Convert hex character to numeric value (0-15).
    public static func hexCharToNibble(_ char: Character) -> UInt8? {
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

    /// Convert nibble (0-15) to lowercase hex character.
    public static func nibbleToHexChar(_ nibble: UInt8) -> Character {
        let chars: [Character] = ["0", "1", "2", "3", "4", "5", "6", "7",
                                   "8", "9", "a", "b", "c", "d", "e", "f"]
        return chars[Int(nibble & 0x0F)]
    }

    /// Convert nibble (0-15) to uppercase hex character.
    public static func nibbleToHexCharUpper(_ nibble: UInt8) -> Character {
        let chars: [Character] = ["0", "1", "2", "3", "4", "5", "6", "7",
                                   "8", "9", "A", "B", "C", "D", "E", "F"]
        return chars[Int(nibble & 0x0F)]
    }

    // MARK: - Hex Validation

    /// Validate a string as hexadecimal.
    public static func validate(_ string: String) -> Result<String, HexError> {
        guard !string.isEmpty else {
            return .failure(.emptyInput)
        }

        for (index, char) in string.enumerated() {
            if !isHexChar(char) {
                return .failure(.invalidCharacter(char, index))
            }
        }

        return .success(string)
    }

    /// Validate hex string and check for even length (required for byte decoding).
    public static func validateBytes(_ string: String) -> Result<String, HexError> {
        guard string.count % 2 == 0 else {
            return .failure(.oddLength(string.count))
        }
        return validate(string)
    }

    /// Check if string is valid hex.
    public static func isValidHex(_ string: String) -> Bool {
        switch validate(string) {
        case .success: return true
        case .failure: return false
        }
    }

    /// Check if string is valid hex with even length.
    public static func isValidHexBytes(_ string: String) -> Bool {
        switch validateBytes(string) {
        case .success: return true
        case .failure: return false
        }
    }

    // MARK: - Hex Encoding

    /// Encode a single byte to two hex characters (lowercase).
    public static func encodeByte(_ byte: UInt8) -> String {
        String(nibbleToHexChar(byte >> 4)) + String(nibbleToHexChar(byte & 0x0F))
    }

    /// Encode a single byte to two hex characters (uppercase).
    public static func encodeByteUpper(_ byte: UInt8) -> String {
        String(nibbleToHexCharUpper(byte >> 4)) + String(nibbleToHexCharUpper(byte & 0x0F))
    }

    /// Encode bytes to hex string (lowercase).
    public static func encode(_ bytes: [UInt8]) -> String {
        bytes.map { encodeByte($0) }.joined()
    }

    /// Encode bytes to hex string (uppercase).
    public static func encodeUpper(_ bytes: [UInt8]) -> String {
        bytes.map { encodeByteUpper($0) }.joined()
    }

    /// Encode Data to hex string (lowercase).
    public static func encode(_ data: Data) -> String {
        encode([UInt8](data))
    }

    /// Encode Data to hex string (uppercase).
    public static func encodeUpper(_ data: Data) -> String {
        encodeUpper([UInt8](data))
    }

    // MARK: - Hex Decoding

    /// Decode two hex characters to a byte.
    public static func decodeHexByte(_ high: Character, _ low: Character) -> UInt8? {
        guard let highNibble = hexCharToNibble(high),
              let lowNibble = hexCharToNibble(low) else {
            return nil
        }
        return (highNibble << 4) | lowNibble
    }

    /// Decode hex string to bytes.
    public static func decode(_ hexString: String) -> Result<[UInt8], HexError> {
        guard !hexString.isEmpty else {
            return .failure(.emptyInput)
        }

        guard hexString.count % 2 == 0 else {
            return .failure(.oddLength(hexString.count))
        }

        var bytes: [UInt8] = []
        let chars = Array(hexString)

        for i in stride(from: 0, to: chars.count, by: 2) {
            guard let byte = decodeHexByte(chars[i], chars[i + 1]) else {
                return .failure(.decodingFailed("invalid hex characters at position \(i)"))
            }
            bytes.append(byte)
        }

        return .success(bytes)
    }

    /// Decode hex string to bytes, returning nil on failure.
    public static func decode(_ hexString: String) -> [UInt8]? {
        switch decode(hexString) as Result<[UInt8], HexError> {
        case .success(let bytes): return bytes
        case .failure: return nil
        }
    }

    /// Decode hex string to Data.
    public static func decodeToData(_ hexString: String) -> Result<Data, HexError> {
        switch decode(hexString) as Result<[UInt8], HexError> {
        case .success(let bytes):
            return .success(Data(bytes))
        case .failure(let error):
            return .failure(error)
        }
    }

    /// Decode hex string to Data, returning nil on failure.
    public static func decodeToData(_ hexString: String) -> Data? {
        guard let bytes: [UInt8] = decode(hexString) else {
            return nil
        }
        return Data(bytes)
    }

    // MARK: - Hex String Operations

    /// Get number of bytes represented by hex string (length / 2).
    public static func byteCount(_ hexString: String) -> Int {
        hexString.count / 2
    }

    /// Convert hex string to lowercase.
    public static func toLower(_ hexString: String) -> String {
        hexString.lowercased()
    }

    /// Convert hex string to uppercase.
    public static func toUpper(_ hexString: String) -> String {
        hexString.uppercased()
    }

    /// Take first n bytes from hex string (2n characters).
    public static func take(_ count: Int, from hexString: String) -> String {
        String(hexString.prefix(count * 2))
    }

    /// Drop first n bytes from hex string (2n characters).
    public static func drop(_ count: Int, from hexString: String) -> String {
        String(hexString.dropFirst(count * 2))
    }

    // MARK: - Hex Formatting

    /// Format hex string with spaces between bytes (e.g., "01 02 03").
    public static func formatSpaced(_ hexString: String) -> String {
        var result = ""
        let chars = Array(hexString)
        for i in stride(from: 0, to: chars.count, by: 2) {
            if !result.isEmpty {
                result += " "
            }
            if i + 1 < chars.count {
                result += String(chars[i]) + String(chars[i + 1])
            } else {
                result += String(chars[i])
            }
        }
        return result
    }

    /// Format hex string with colons between bytes (e.g., "01:02:03").
    public static func formatColons(_ hexString: String) -> String {
        var result = ""
        let chars = Array(hexString)
        for i in stride(from: 0, to: chars.count, by: 2) {
            if !result.isEmpty {
                result += ":"
            }
            if i + 1 < chars.count {
                result += String(chars[i]) + String(chars[i + 1])
            } else {
                result += String(chars[i])
            }
        }
        return result
    }

    /// Format hex string with prefix (e.g., "0x0102").
    public static func format0x(_ hexString: String) -> String {
        "0x" + hexString
    }

    /// Format hex string as C array literal.
    public static func formatCArray(_ hexString: String) -> String {
        guard let bytes: [UInt8] = decode(hexString) else {
            return "{ /* invalid */ }"
        }
        let formatted = bytes.map { "0x" + encodeByteUpper($0) }.joined(separator: ", ")
        return "{ \(formatted) }"
    }

    // MARK: - Hex Comparison

    /// Compare two hex strings (case-insensitive).
    public static func isEqual(_ lhs: String, _ rhs: String) -> Bool {
        lhs.lowercased() == rhs.lowercased()
    }

    /// Constant-time comparison of hex strings (timing-attack resistant).
    /// Both strings must be valid hex of the same length.
    public static func constantTimeEqual(_ lhs: String, _ rhs: String) -> Bool {
        let lhsLower = lhs.lowercased()
        let rhsLower = rhs.lowercased()

        guard lhsLower.count == rhsLower.count else {
            return false
        }

        var result: UInt8 = 0
        let lhsChars = Array(lhsLower.utf8)
        let rhsChars = Array(rhsLower.utf8)

        for i in 0..<lhsChars.count {
            result |= lhsChars[i] ^ rhsChars[i]
        }

        return result == 0
    }

    /// Constant-time comparison of byte arrays (timing-attack resistant).
    public static func constantTimeEqual(_ lhs: [UInt8], _ rhs: [UInt8]) -> Bool {
        guard lhs.count == rhs.count else {
            return false
        }

        guard !lhs.isEmpty else {
            return true
        }

        var result: UInt8 = 0
        for i in 0..<lhs.count {
            result |= lhs[i] ^ rhs[i]
        }

        return result == 0
    }

    /// Constant-time comparison of Data (timing-attack resistant).
    public static func constantTimeEqual(_ lhs: Data, _ rhs: Data) -> Bool {
        constantTimeEqual([UInt8](lhs), [UInt8](rhs))
    }

    // MARK: - Integer Conversion

    /// Convert integer to hex string with minimum width (zero-padded).
    public static func intToHex(_ value: Int64, minWidth: Int = 0) -> String {
        let absValue = abs(value)
        var hex = String(absValue, radix: 16, uppercase: false)

        // Pad to minimum width
        while hex.count < minWidth {
            hex = "0" + hex
        }

        return hex
    }

    /// Parse hex string to integer.
    public static func hexToInt(_ hexString: String) -> Int64? {
        Int64(hexString.lowercased(), radix: 16)
    }

    // MARK: - Utility Functions

    /// Check if hex string is empty.
    public static func isEmpty(_ hexString: String) -> Bool {
        hexString.isEmpty
    }

    /// Create empty hex string.
    public static var empty: String {
        ""
    }
}
