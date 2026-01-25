// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe checksum and hash operations.
//!
//! Provides CRC, hash, and integrity verification functions
//! with overflow protection.

/// CRC-32 checksum (IEEE polynomial).
pub fn crc32(data: &[u8]) -> u32 {
    const TABLE: [u32; 256] = generate_crc32_table();
    let mut crc = 0xFFFFFFFF_u32;
    for byte in data {
        let index = ((crc ^ (*byte as u32)) & 0xFF) as usize;
        crc = (crc >> 8) ^ TABLE[index];
    }
    !crc
}

const fn generate_crc32_table() -> [u32; 256] {
    let mut table = [0u32; 256];
    let mut i = 0;
    while i < 256 {
        let mut crc = i as u32;
        let mut j = 0;
        while j < 8 {
            if crc & 1 != 0 {
                crc = (crc >> 1) ^ 0xEDB88320;
            } else {
                crc >>= 1;
            }
            j += 1;
        }
        table[i] = crc;
        i += 1;
    }
    table
}

/// Adler-32 checksum.
pub fn adler32(data: &[u8]) -> u32 {
    const MOD_ADLER: u32 = 65521;
    let mut a: u32 = 1;
    let mut b: u32 = 0;

    for byte in data {
        a = (a + *byte as u32) % MOD_ADLER;
        b = (b + a) % MOD_ADLER;
    }

    (b << 16) | a
}

/// Fletcher-16 checksum.
pub fn fletcher16(data: &[u8]) -> u16 {
    let mut sum1: u16 = 0;
    let mut sum2: u16 = 0;

    for byte in data {
        sum1 = (sum1 + *byte as u16) % 255;
        sum2 = (sum2 + sum1) % 255;
    }

    (sum2 << 8) | sum1
}

/// Simple FNV-1a hash (64-bit).
pub fn fnv1a_64(data: &[u8]) -> u64 {
    const FNV_OFFSET: u64 = 0xcbf29ce484222325;
    const FNV_PRIME: u64 = 0x100000001b3;

    let mut hash = FNV_OFFSET;
    for byte in data {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(FNV_PRIME);
    }
    hash
}

/// Simple FNV-1a hash (32-bit).
pub fn fnv1a_32(data: &[u8]) -> u32 {
    const FNV_OFFSET: u32 = 0x811c9dc5;
    const FNV_PRIME: u32 = 0x01000193;

    let mut hash = FNV_OFFSET;
    for byte in data {
        hash ^= *byte as u32;
        hash = hash.wrapping_mul(FNV_PRIME);
    }
    hash
}

/// DJB2 hash algorithm.
pub fn djb2(data: &[u8]) -> u32 {
    let mut hash: u32 = 5381;
    for byte in data {
        hash = hash.wrapping_mul(33).wrapping_add(*byte as u32);
    }
    hash
}

/// SDBM hash algorithm.
pub fn sdbm(data: &[u8]) -> u32 {
    let mut hash: u32 = 0;
    for byte in data {
        hash = (*byte as u32)
            .wrapping_add(hash.wrapping_shl(6))
            .wrapping_add(hash.wrapping_shl(16))
            .wrapping_sub(hash);
    }
    hash
}

/// Luhn algorithm for credit card validation.
pub fn luhn_check(digits: &str) -> bool {
    let chars: Vec<char> = digits.chars().filter(|c| c.is_ascii_digit()).collect();
    if chars.is_empty() {
        return false;
    }

    let mut sum = 0u32;
    let mut double = false;

    for c in chars.iter().rev() {
        let mut digit = c.to_digit(10).unwrap();
        if double {
            digit *= 2;
            if digit > 9 {
                digit -= 9;
            }
        }
        sum += digit;
        double = !double;
    }

    sum % 10 == 0
}

/// Generate Luhn check digit.
pub fn luhn_digit(digits: &str) -> Option<char> {
    let chars: Vec<char> = digits.chars().filter(|c| c.is_ascii_digit()).collect();
    if chars.is_empty() {
        return None;
    }

    let mut sum = 0u32;
    let mut double = true;

    for c in chars.iter().rev() {
        let mut digit = c.to_digit(10).unwrap();
        if double {
            digit *= 2;
            if digit > 9 {
                digit -= 9;
            }
        }
        sum += digit;
        double = !double;
    }

    let check = (10 - (sum % 10)) % 10;
    char::from_digit(check, 10)
}

/// Verify data integrity with CRC-32.
pub fn verify_crc32(data: &[u8], expected: u32) -> bool {
    crc32(data) == expected
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_crc32() {
        let data = b"123456789";
        assert_eq!(crc32(data), 0xCBF43926);
    }

    #[test]
    fn test_adler32() {
        let data = b"Wikipedia";
        assert_eq!(adler32(data), 0x11E60398);
    }

    #[test]
    fn test_fnv1a() {
        let data = b"hello";
        assert_ne!(fnv1a_64(data), 0);
        assert_ne!(fnv1a_32(data), 0);
    }

    #[test]
    fn test_luhn() {
        // Valid Visa test number
        assert!(luhn_check("4532015112830366"));
        // Invalid
        assert!(!luhn_check("1234567890123456"));
    }

    #[test]
    fn test_luhn_digit() {
        assert_eq!(luhn_digit("453201511283036"), Some('6'));
    }
}
