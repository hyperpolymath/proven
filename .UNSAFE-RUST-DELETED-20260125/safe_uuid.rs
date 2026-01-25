// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe UUID generation and validation following RFC 4122.

use crate::core::{Error, Result};
use std::fmt;

/// UUID version types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UuidVersion {
    V1,  // Time-based
    V2,  // DCE Security
    V3,  // Name-based (MD5)
    V4,  // Random
    V5,  // Name-based (SHA-1)
    Nil, // Nil UUID
}

/// UUID variant types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UuidVariant {
    Ncs,
    Rfc4122,
    Microsoft,
    Future,
}

/// A validated UUID (128 bits).
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Uuid {
    bytes: [u8; 16],
}

impl Uuid {
    /// The nil UUID (all zeros).
    pub const NIL: Uuid = Uuid { bytes: [0; 16] };

    /// DNS namespace UUID.
    pub const NAMESPACE_DNS: Uuid = Uuid {
        bytes: [
            0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
            0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
        ],
    };

    /// URL namespace UUID.
    pub const NAMESPACE_URL: Uuid = Uuid {
        bytes: [
            0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
            0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
        ],
    };

    /// Create UUID from bytes.
    pub fn from_bytes(bytes: [u8; 16]) -> Self {
        Uuid { bytes }
    }

    /// Get the bytes.
    pub fn as_bytes(&self) -> &[u8; 16] {
        &self.bytes
    }

    /// Get the UUID version.
    pub fn version(&self) -> UuidVersion {
        let version = (self.bytes[6] >> 4) & 0x0F;
        match version {
            1 => UuidVersion::V1,
            2 => UuidVersion::V2,
            3 => UuidVersion::V3,
            4 => UuidVersion::V4,
            5 => UuidVersion::V5,
            _ => UuidVersion::Nil,
        }
    }

    /// Get the UUID variant.
    pub fn variant(&self) -> UuidVariant {
        let byte = self.bytes[8];
        if (byte >> 7) == 0 {
            UuidVariant::Ncs
        } else if (byte >> 6) == 0b10 {
            UuidVariant::Rfc4122
        } else if (byte >> 5) == 0b110 {
            UuidVariant::Microsoft
        } else {
            UuidVariant::Future
        }
    }

    /// Check if this is the nil UUID.
    pub fn is_nil(&self) -> bool {
        self.bytes.iter().all(|&b| b == 0)
    }

    /// Format as canonical string.
    pub fn to_string(&self) -> String {
        format!(
            "{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
            self.bytes[0], self.bytes[1], self.bytes[2], self.bytes[3],
            self.bytes[4], self.bytes[5],
            self.bytes[6], self.bytes[7],
            self.bytes[8], self.bytes[9],
            self.bytes[10], self.bytes[11], self.bytes[12], self.bytes[13], self.bytes[14], self.bytes[15]
        )
    }

    /// Format as URN.
    pub fn to_urn(&self) -> String {
        format!("urn:uuid:{}", self.to_string())
    }
}

impl fmt::Debug for Uuid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Uuid({})", self.to_string())
    }
}

impl fmt::Display for Uuid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

/// Safe UUID operations.
pub struct SafeUuid;

impl SafeUuid {
    /// Parse UUID from canonical string format.
    pub fn parse(s: &str) -> Result<Uuid> {
        if s.len() != 36 {
            return Err(Error::InvalidFormat(format!(
                "UUID must be 36 characters, got {}",
                s.len()
            )));
        }

        let chars: Vec<char> = s.chars().collect();
        if chars[8] != '-' || chars[13] != '-' || chars[18] != '-' || chars[23] != '-' {
            return Err(Error::InvalidFormat("Invalid UUID format".into()));
        }

        let hex_str: String = s.chars().filter(|&c| c != '-').collect();
        if hex_str.len() != 32 {
            return Err(Error::InvalidFormat("Invalid UUID length".into()));
        }

        let mut bytes = [0u8; 16];
        for i in 0..16 {
            let byte_str = &hex_str[i * 2..i * 2 + 2];
            bytes[i] = u8::from_str_radix(byte_str, 16)
                .map_err(|_| Error::InvalidFormat("Invalid hex character".into()))?;
        }

        Ok(Uuid::from_bytes(bytes))
    }

    /// Generate a v4 (random) UUID from provided random bytes.
    pub fn v4_from_bytes(random_bytes: [u8; 16]) -> Uuid {
        let mut bytes = random_bytes;
        // Set version to 4
        bytes[6] = (bytes[6] & 0x0F) | 0x40;
        // Set variant to RFC 4122
        bytes[8] = (bytes[8] & 0x3F) | 0x80;
        Uuid::from_bytes(bytes)
    }

    /// Check if string is valid UUID format.
    pub fn is_valid(s: &str) -> bool {
        Self::parse(s).is_ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_valid() {
        let uuid = SafeUuid::parse("550e8400-e29b-41d4-a716-446655440000").unwrap();
        assert!(!uuid.is_nil());
        assert_eq!(uuid.version(), UuidVersion::V4);
    }

    #[test]
    fn test_nil_uuid() {
        let uuid = SafeUuid::parse("00000000-0000-0000-0000-000000000000").unwrap();
        assert!(uuid.is_nil());
    }

    #[test]
    fn test_format() {
        let uuid = SafeUuid::parse("550e8400-e29b-41d4-a716-446655440000").unwrap();
        assert_eq!(uuid.to_string(), "550e8400-e29b-41d4-a716-446655440000");
    }

    #[test]
    fn test_invalid() {
        assert!(SafeUuid::parse("not-a-uuid").is_err());
        assert!(SafeUuid::parse("").is_err());
    }
}
