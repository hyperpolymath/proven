// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe hexadecimal encoding and decoding.

use crate::core::{Error, Result};

/// Safe hex operations.
pub struct SafeHex;

impl SafeHex {
    /// Check if character is valid hex.
    pub fn is_hex_char(c: char) -> bool {
        c.is_ascii_hexdigit()
    }

    /// Convert hex char to nibble value.
    pub fn hex_char_to_nibble(c: char) -> Option<u8> {
        match c {
            '0'..='9' => Some(c as u8 - b'0'),
            'a'..='f' => Some(c as u8 - b'a' + 10),
            'A'..='F' => Some(c as u8 - b'A' + 10),
            _ => None,
        }
    }

    /// Convert nibble to lowercase hex char.
    pub fn nibble_to_hex_char(n: u8) -> char {
        if n < 10 {
            (b'0' + n) as char
        } else {
            (b'a' + n - 10) as char
        }
    }

    /// Convert nibble to uppercase hex char.
    pub fn nibble_to_hex_char_upper(n: u8) -> char {
        if n < 10 {
            (b'0' + n) as char
        } else {
            (b'A' + n - 10) as char
        }
    }

    /// Encode bytes to lowercase hex string.
    pub fn encode(bytes: &[u8]) -> String {
        let mut result = String::with_capacity(bytes.len() * 2);
        for &b in bytes {
            result.push(Self::nibble_to_hex_char(b >> 4));
            result.push(Self::nibble_to_hex_char(b & 0x0F));
        }
        result
    }

    /// Encode bytes to uppercase hex string.
    pub fn encode_upper(bytes: &[u8]) -> String {
        let mut result = String::with_capacity(bytes.len() * 2);
        for &b in bytes {
            result.push(Self::nibble_to_hex_char_upper(b >> 4));
            result.push(Self::nibble_to_hex_char_upper(b & 0x0F));
        }
        result
    }

    /// Decode hex string to bytes.
    pub fn decode(hex: &str) -> Result<Vec<u8>> {
        if hex.len() % 2 != 0 {
            return Err(Error::InvalidFormat("Hex string has odd length".into()));
        }

        let mut result = Vec::with_capacity(hex.len() / 2);
        let chars: Vec<char> = hex.chars().collect();

        for i in (0..chars.len()).step_by(2) {
            let high = Self::hex_char_to_nibble(chars[i])
                .ok_or_else(|| Error::InvalidFormat("Invalid hex character".into()))?;
            let low = Self::hex_char_to_nibble(chars[i + 1])
                .ok_or_else(|| Error::InvalidFormat("Invalid hex character".into()))?;
            result.push((high << 4) | low);
        }

        Ok(result)
    }

    /// Validate hex string.
    pub fn is_valid(s: &str) -> bool {
        s.chars().all(Self::is_hex_char)
    }

    /// Validate hex string with even length.
    pub fn is_valid_bytes(s: &str) -> bool {
        s.len() % 2 == 0 && Self::is_valid(s)
    }

    /// Format hex with spaces between bytes.
    pub fn format_spaced(hex: &str) -> Result<String> {
        if hex.len() % 2 != 0 {
            return Err(Error::InvalidFormat("Hex string has odd length".into()));
        }
        if hex.is_empty() {
            return Ok(String::new());
        }

        let chars: Vec<char> = hex.chars().collect();
        let mut result = String::with_capacity(hex.len() + hex.len() / 2 - 1);

        for (i, chunk) in chars.chunks(2).enumerate() {
            if i > 0 {
                result.push(' ');
            }
            result.push(chunk[0]);
            result.push(chunk[1]);
        }

        Ok(result)
    }

    /// Format hex with colons between bytes.
    pub fn format_colons(hex: &str) -> Result<String> {
        if hex.len() % 2 != 0 {
            return Err(Error::InvalidFormat("Hex string has odd length".into()));
        }
        if hex.is_empty() {
            return Ok(String::new());
        }

        let chars: Vec<char> = hex.chars().collect();
        let mut result = String::with_capacity(hex.len() + hex.len() / 2 - 1);

        for (i, chunk) in chars.chunks(2).enumerate() {
            if i > 0 {
                result.push(':');
            }
            result.push(chunk[0]);
            result.push(chunk[1]);
        }

        Ok(result)
    }

    /// Constant-time comparison of hex strings.
    pub fn constant_time_eq(a: &str, b: &str) -> bool {
        if a.len() != b.len() {
            return false;
        }

        let a_lower: Vec<char> = a.to_lowercase().chars().collect();
        let b_lower: Vec<char> = b.to_lowercase().chars().collect();

        let mut diff = 0u8;
        for (ca, cb) in a_lower.iter().zip(b_lower.iter()) {
            diff |= (*ca as u8) ^ (*cb as u8);
        }

        diff == 0
    }

    /// Convert integer to hex string with minimum width.
    pub fn int_to_hex(value: u64, min_width: usize) -> String {
        let hex = format!("{:x}", value);
        if hex.len() >= min_width {
            hex
        } else {
            format!("{:0>width$}", hex, width = min_width)
        }
    }

    /// Parse hex string to integer.
    pub fn hex_to_int(hex: &str) -> Result<u64> {
        u64::from_str_radix(hex, 16)
            .map_err(|_| Error::InvalidFormat("Invalid hex number".into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode() {
        assert_eq!(SafeHex::encode(&[0xFF, 0x00, 0xAB]), "ff00ab");
        assert_eq!(SafeHex::encode_upper(&[0xFF, 0x00, 0xAB]), "FF00AB");
    }

    #[test]
    fn test_decode() {
        assert_eq!(SafeHex::decode("ff00ab").unwrap(), vec![0xFF, 0x00, 0xAB]);
    }

    #[test]
    fn test_validation() {
        assert!(SafeHex::is_valid("abcdef0123456789"));
        assert!(!SafeHex::is_valid("xyz"));
        assert!(SafeHex::is_valid_bytes("aabb"));
        assert!(!SafeHex::is_valid_bytes("aab"));
    }

    #[test]
    fn test_format_spaced() {
        assert_eq!(SafeHex::format_spaced("aabbcc").unwrap(), "aa bb cc");
    }

    #[test]
    fn test_constant_time_eq() {
        assert!(SafeHex::constant_time_eq("aabb", "AABB"));
        assert!(!SafeHex::constant_time_eq("aabb", "aab0"));
    }

    #[test]
    fn test_int_to_hex() {
        assert_eq!(SafeHex::int_to_hex(255, 2), "ff");
        assert_eq!(SafeHex::int_to_hex(255, 4), "00ff");
    }
}
