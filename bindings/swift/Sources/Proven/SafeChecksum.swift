// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Checksum and hash utilities.
public enum SafeChecksum {
    // MARK: - CRC-32

    private static let crc32Table: [UInt32] = {
        var table = [UInt32](repeating: 0, count: 256)
        for i in 0..<256 {
            var crc = UInt32(i)
            for _ in 0..<8 {
                if crc & 1 != 0 {
                    crc = (crc >> 1) ^ 0xEDB88320
                } else {
                    crc >>= 1
                }
            }
            table[i] = crc
        }
        return table
    }()

    /// Calculate CRC-32 checksum.
    public static func crc32(_ data: Data) -> UInt32 {
        var crc: UInt32 = 0xFFFFFFFF
        for byte in data {
            let index = Int((crc ^ UInt32(byte)) & 0xFF)
            crc = (crc >> 8) ^ crc32Table[index]
        }
        return crc ^ 0xFFFFFFFF
    }

    /// Calculate CRC-32 checksum for string.
    public static func crc32(_ string: String) -> UInt32 {
        crc32(Data(string.utf8))
    }

    /// Verify CRC-32 checksum.
    public static func verifyCrc32(_ data: Data, expected: UInt32) -> Bool {
        crc32(data) == expected
    }

    // MARK: - Adler-32

    /// Calculate Adler-32 checksum.
    public static func adler32(_ data: Data) -> UInt32 {
        let mod: UInt32 = 65521
        var a: UInt32 = 1
        var b: UInt32 = 0

        for byte in data {
            a = (a + UInt32(byte)) % mod
            b = (b + a) % mod
        }

        return (b << 16) | a
    }

    /// Calculate Adler-32 checksum for string.
    public static func adler32(_ string: String) -> UInt32 {
        adler32(Data(string.utf8))
    }

    /// Verify Adler-32 checksum.
    public static func verifyAdler32(_ data: Data, expected: UInt32) -> Bool {
        adler32(data) == expected
    }

    // MARK: - FNV-1a

    /// Calculate FNV-1a 32-bit hash.
    public static func fnv1a32(_ data: Data) -> UInt32 {
        var hash: UInt32 = 2166136261
        for byte in data {
            hash ^= UInt32(byte)
            hash = hash &* 16777619
        }
        return hash
    }

    /// Calculate FNV-1a 32-bit hash for string.
    public static func fnv1a32(_ string: String) -> UInt32 {
        fnv1a32(Data(string.utf8))
    }

    /// Calculate FNV-1a 64-bit hash.
    public static func fnv1a64(_ data: Data) -> UInt64 {
        var hash: UInt64 = 14695981039346656037
        for byte in data {
            hash ^= UInt64(byte)
            hash = hash &* 1099511628211
        }
        return hash
    }

    /// Calculate FNV-1a 64-bit hash for string.
    public static func fnv1a64(_ string: String) -> UInt64 {
        fnv1a64(Data(string.utf8))
    }

    // MARK: - Luhn Algorithm

    /// Validate a number using the Luhn algorithm (credit cards, etc.).
    public static func luhnValidate(_ digits: String) -> Bool {
        let chars = digits.filter { $0.isNumber }
        guard !chars.isEmpty else { return false }

        var sum = 0
        var alternate = false

        for char in chars.reversed() {
            guard let digit = Int(String(char)) else { return false }

            if alternate {
                let doubled = digit * 2
                sum += doubled > 9 ? doubled - 9 : doubled
            } else {
                sum += digit
            }

            alternate.toggle()
        }

        return sum % 10 == 0
    }

    /// Calculate Luhn check digit.
    public static func luhnCheckDigit(_ digits: String) -> Int? {
        let chars = digits.filter { $0.isNumber }
        guard !chars.isEmpty else { return nil }

        var sum = 0
        var alternate = true

        for char in chars.reversed() {
            guard let digit = Int(String(char)) else { return nil }

            if alternate {
                let doubled = digit * 2
                sum += doubled > 9 ? doubled - 9 : doubled
            } else {
                sum += digit
            }

            alternate.toggle()
        }

        return (10 - (sum % 10)) % 10
    }

    // MARK: - ISBN Validation

    /// Validate ISBN-10.
    public static func isbn10Validate(_ isbn: String) -> Bool {
        let chars = isbn.filter { $0.isNumber || $0 == "X" || $0 == "x" }
        guard chars.count == 10 else { return false }

        var sum = 0
        for (index, char) in chars.enumerated() {
            let value: Int
            if char == "X" || char == "x" {
                guard index == 9 else { return false }
                value = 10
            } else {
                guard let digit = Int(String(char)) else { return false }
                value = digit
            }
            sum += value * (10 - index)
        }

        return sum % 11 == 0
    }

    /// Validate ISBN-13.
    public static func isbn13Validate(_ isbn: String) -> Bool {
        let chars = isbn.filter { $0.isNumber }
        guard chars.count == 13 else { return false }

        var sum = 0
        for (index, char) in chars.enumerated() {
            guard let digit = Int(String(char)) else { return false }
            sum += digit * (index % 2 == 0 ? 1 : 3)
        }

        return sum % 10 == 0
    }

    // MARK: - Checksum8/16

    /// Calculate simple 8-bit checksum (sum of bytes).
    public static func checksum8(_ data: Data) -> UInt8 {
        var sum: UInt8 = 0
        for byte in data {
            sum = sum &+ byte
        }
        return sum
    }

    /// Calculate simple 16-bit checksum (sum of bytes).
    public static func checksum16(_ data: Data) -> UInt16 {
        var sum: UInt16 = 0
        for byte in data {
            sum = sum &+ UInt16(byte)
        }
        return sum
    }

    /// Calculate XOR checksum.
    public static func xorChecksum(_ data: Data) -> UInt8 {
        var result: UInt8 = 0
        for byte in data {
            result ^= byte
        }
        return result
    }

    // MARK: - Fletcher Checksum

    /// Calculate Fletcher-16 checksum.
    public static func fletcher16(_ data: Data) -> UInt16 {
        var sum1: UInt16 = 0
        var sum2: UInt16 = 0

        for byte in data {
            sum1 = (sum1 + UInt16(byte)) % 255
            sum2 = (sum2 + sum1) % 255
        }

        return (sum2 << 8) | sum1
    }

    /// Calculate Fletcher-32 checksum.
    public static func fletcher32(_ data: Data) -> UInt32 {
        var sum1: UInt32 = 0
        var sum2: UInt32 = 0

        // Process 16-bit words
        var index = 0
        while index + 1 < data.count {
            let word = UInt32(data[index]) | (UInt32(data[index + 1]) << 8)
            sum1 = (sum1 + word) % 65535
            sum2 = (sum2 + sum1) % 65535
            index += 2
        }

        // Handle odd byte
        if index < data.count {
            sum1 = (sum1 + UInt32(data[index])) % 65535
            sum2 = (sum2 + sum1) % 65535
        }

        return (sum2 << 16) | sum1
    }
}
