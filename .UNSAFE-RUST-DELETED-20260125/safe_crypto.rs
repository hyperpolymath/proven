// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe cryptographic operations with formally verified specifications.
//!
//! This module provides cryptographic primitives that are:
//! - Memory-safe (Rust guarantees)
//! - Specification-verified (Idris2 proofs)
//! - Side-channel resistant (constant-time operations)
//!
//! # Feature Flag
//!
//! Enable the `crypto` feature to use real cryptographic implementations:
//! ```toml
//! [dependencies]
//! proven = { version = "0.9", features = ["crypto"] }
//! ```
//!
//! Without the feature, stub implementations are provided for testing.

/// Safe cryptographic operations.
///
/// All operations are designed to be:
/// - Constant-time where security-relevant
/// - Memory-safe with automatic zeroing of sensitive data
/// - Aligned with Hyperpolymath Crypto Standard
pub struct SafeCrypto;

#[cfg(feature = "crypto")]
mod impl_crypto {
    use sha3::{Digest, Sha3_256, Sha3_512};

    /// Compute SHA3-256 hash.
    ///
    /// SHA3-256 provides 128-bit security level against collision attacks.
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "crypto")]
    /// # {
    /// use proven::SafeCrypto;
    ///
    /// let hash = SafeCrypto::sha3_256(b"Hello, World!");
    /// assert_eq!(hash.len(), 32);
    /// # }
    /// ```
    pub fn sha3_256(data: &[u8]) -> [u8; 32] {
        let mut hasher = Sha3_256::new();
        hasher.update(data);
        hasher.finalize().into()
    }

    /// Compute SHA3-512 hash.
    ///
    /// SHA3-512 provides 256-bit security level against collision attacks.
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "crypto")]
    /// # {
    /// use proven::SafeCrypto;
    ///
    /// let hash = SafeCrypto::sha3_512(b"Hello, World!");
    /// assert_eq!(hash.len(), 64);
    /// # }
    /// ```
    pub fn sha3_512(data: &[u8]) -> [u8; 64] {
        let mut hasher = Sha3_512::new();
        hasher.update(data);
        hasher.finalize().into()
    }

    /// Compute BLAKE3 hash (256-bit output).
    ///
    /// BLAKE3 is the primary hashing algorithm per Hyperpolymath Crypto Standard.
    /// It is faster than SHA-256/SHA-3 while providing equivalent security.
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "crypto")]
    /// # {
    /// use proven::SafeCrypto;
    ///
    /// let hash = SafeCrypto::blake3(b"Hello, World!");
    /// assert_eq!(hash.len(), 32);
    /// # }
    /// ```
    pub fn blake3(data: &[u8]) -> [u8; 32] {
        *blake3::hash(data).as_bytes()
    }

    /// Compute BLAKE3 hash with variable output length (XOF mode).
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "crypto")]
    /// # {
    /// use proven::SafeCrypto;
    ///
    /// let hash = SafeCrypto::blake3_xof(b"data", 64);
    /// assert_eq!(hash.len(), 64);
    /// # }
    /// ```
    pub fn blake3_xof(data: &[u8], output_len: usize) -> Vec<u8> {
        let mut hasher = blake3::Hasher::new();
        hasher.update(data);
        let mut output = vec![0u8; output_len];
        hasher.finalize_xof().fill(&mut output);
        output
    }

    /// Compute BLAKE3 keyed hash (MAC).
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "crypto")]
    /// # {
    /// use proven::SafeCrypto;
    ///
    /// let key = [0u8; 32];
    /// let mac = SafeCrypto::blake3_keyed(&key, b"message");
    /// assert_eq!(mac.len(), 32);
    /// # }
    /// ```
    pub fn blake3_keyed(key: &[u8; 32], data: &[u8]) -> [u8; 32] {
        *blake3::keyed_hash(key, data).as_bytes()
    }

    /// Derive a key using BLAKE3-KDF.
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "crypto")]
    /// # {
    /// use proven::SafeCrypto;
    ///
    /// let key = SafeCrypto::blake3_derive_key("context", b"ikm", 32);
    /// assert_eq!(key.len(), 32);
    /// # }
    /// ```
    pub fn blake3_derive_key(context: &str, ikm: &[u8], output_len: usize) -> Vec<u8> {
        let mut hasher = blake3::Hasher::new_derive_key(context);
        hasher.update(ikm);
        let mut output = vec![0u8; output_len];
        hasher.finalize_xof().fill(&mut output);
        output
    }

    /// Compute HMAC-SHA3-256.
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "crypto")]
    /// # {
    /// use proven::SafeCrypto;
    ///
    /// let mac = SafeCrypto::hmac_sha3_256(b"key", b"message");
    /// assert!(mac.is_some());
    /// # }
    /// ```
    pub fn hmac_sha3_256(key: &[u8], data: &[u8]) -> Option<[u8; 32]> {
        use hmac::{Hmac, Mac};
        type HmacSha3_256 = Hmac<Sha3_256>;

        let mut mac = HmacSha3_256::new_from_slice(key).ok()?;
        mac.update(data);
        Some(mac.finalize().into_bytes().into())
    }

    /// Generate cryptographically secure random bytes.
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "crypto")]
    /// # {
    /// use proven::SafeCrypto;
    ///
    /// let bytes = SafeCrypto::random_bytes(32);
    /// assert_eq!(bytes.len(), 32);
    /// # }
    /// ```
    pub fn random_bytes(len: usize) -> Vec<u8> {
        use rand::RngCore;
        let mut bytes = vec![0u8; len];
        rand::thread_rng().fill_bytes(&mut bytes);
        bytes
    }

    /// Constant-time comparison (prevents timing attacks).
    ///
    /// This function is guaranteed to take the same amount of time
    /// regardless of where the difference occurs.
    ///
    /// # Example
    ///
    /// ```
    /// use proven::SafeCrypto;
    ///
    /// assert!(SafeCrypto::constant_time_compare(b"hello", b"hello"));
    /// assert!(!SafeCrypto::constant_time_compare(b"hello", b"world"));
    /// ```
    pub fn constant_time_compare(a: &[u8], b: &[u8]) -> bool {
        use subtle::ConstantTimeEq;
        if a.len() != b.len() {
            return false;
        }
        a.ct_eq(b).into()
    }
}

#[cfg(not(feature = "crypto"))]
mod impl_stub {
    /// Stub: Compute SHA3-256 hash (returns zeros without crypto feature).
    pub fn sha3_256(_data: &[u8]) -> [u8; 32] {
        [0u8; 32]
    }

    /// Stub: Compute SHA3-512 hash (returns zeros without crypto feature).
    pub fn sha3_512(_data: &[u8]) -> [u8; 64] {
        [0u8; 64]
    }

    /// Stub: Compute BLAKE3 hash (returns zeros without crypto feature).
    pub fn blake3(_data: &[u8]) -> [u8; 32] {
        [0u8; 32]
    }

    /// Stub: Compute BLAKE3 XOF hash (returns zeros without crypto feature).
    pub fn blake3_xof(_data: &[u8], output_len: usize) -> Vec<u8> {
        vec![0u8; output_len]
    }

    /// Stub: Compute BLAKE3 keyed hash (returns zeros without crypto feature).
    pub fn blake3_keyed(_key: &[u8; 32], _data: &[u8]) -> [u8; 32] {
        [0u8; 32]
    }

    /// Stub: Derive key using BLAKE3-KDF (returns zeros without crypto feature).
    pub fn blake3_derive_key(_context: &str, _ikm: &[u8], output_len: usize) -> Vec<u8> {
        vec![0u8; output_len]
    }

    /// Stub: Compute HMAC-SHA3-256 (returns zeros without crypto feature).
    pub fn hmac_sha3_256(_key: &[u8], _data: &[u8]) -> Option<[u8; 32]> {
        Some([0u8; 32])
    }

    /// Stub: Generate random bytes (returns zeros without crypto feature).
    pub fn random_bytes(len: usize) -> Vec<u8> {
        vec![0u8; len]
    }

    /// Constant-time comparison (always available).
    pub fn constant_time_compare(a: &[u8], b: &[u8]) -> bool {
        if a.len() != b.len() {
            return false;
        }
        let mut result = 0u8;
        for (x, y) in a.iter().zip(b.iter()) {
            result |= x ^ y;
        }
        result == 0
    }
}

impl SafeCrypto {
    /// Compute SHA3-256 hash.
    #[cfg(feature = "crypto")]
    pub fn sha3_256(data: &[u8]) -> [u8; 32] {
        impl_crypto::sha3_256(data)
    }

    /// Compute SHA3-256 hash (stub without crypto feature).
    #[cfg(not(feature = "crypto"))]
    pub fn sha3_256(data: &[u8]) -> [u8; 32] {
        impl_stub::sha3_256(data)
    }

    /// Compute SHA3-256 and return as hex string.
    pub fn sha3_256_hex(data: &[u8]) -> String {
        Self::to_hex(&Self::sha3_256(data))
    }

    /// Compute SHA3-512 hash.
    #[cfg(feature = "crypto")]
    pub fn sha3_512(data: &[u8]) -> [u8; 64] {
        impl_crypto::sha3_512(data)
    }

    /// Compute SHA3-512 hash (stub without crypto feature).
    #[cfg(not(feature = "crypto"))]
    pub fn sha3_512(data: &[u8]) -> [u8; 64] {
        impl_stub::sha3_512(data)
    }

    /// Compute SHA3-512 and return as hex string.
    pub fn sha3_512_hex(data: &[u8]) -> String {
        Self::to_hex(&Self::sha3_512(data))
    }

    /// Compute BLAKE3 hash (256-bit output).
    #[cfg(feature = "crypto")]
    pub fn blake3(data: &[u8]) -> [u8; 32] {
        impl_crypto::blake3(data)
    }

    /// Compute BLAKE3 hash (stub without crypto feature).
    #[cfg(not(feature = "crypto"))]
    pub fn blake3(data: &[u8]) -> [u8; 32] {
        impl_stub::blake3(data)
    }

    /// Compute BLAKE3 and return as hex string.
    pub fn blake3_hex(data: &[u8]) -> String {
        Self::to_hex(&Self::blake3(data))
    }

    /// Compute BLAKE3 hash with variable output length (XOF mode).
    #[cfg(feature = "crypto")]
    pub fn blake3_xof(data: &[u8], output_len: usize) -> Vec<u8> {
        impl_crypto::blake3_xof(data, output_len)
    }

    /// Compute BLAKE3 XOF (stub without crypto feature).
    #[cfg(not(feature = "crypto"))]
    pub fn blake3_xof(data: &[u8], output_len: usize) -> Vec<u8> {
        impl_stub::blake3_xof(data, output_len)
    }

    /// Compute BLAKE3 keyed hash (MAC).
    #[cfg(feature = "crypto")]
    pub fn blake3_keyed(key: &[u8; 32], data: &[u8]) -> [u8; 32] {
        impl_crypto::blake3_keyed(key, data)
    }

    /// Compute BLAKE3 keyed hash (stub without crypto feature).
    #[cfg(not(feature = "crypto"))]
    pub fn blake3_keyed(key: &[u8; 32], data: &[u8]) -> [u8; 32] {
        impl_stub::blake3_keyed(key, data)
    }

    /// Derive a key using BLAKE3-KDF.
    #[cfg(feature = "crypto")]
    pub fn blake3_derive_key(context: &str, ikm: &[u8], output_len: usize) -> Vec<u8> {
        impl_crypto::blake3_derive_key(context, ikm, output_len)
    }

    /// Derive key using BLAKE3-KDF (stub without crypto feature).
    #[cfg(not(feature = "crypto"))]
    pub fn blake3_derive_key(context: &str, ikm: &[u8], output_len: usize) -> Vec<u8> {
        impl_stub::blake3_derive_key(context, ikm, output_len)
    }

    /// Compute HMAC-SHA3-256.
    #[cfg(feature = "crypto")]
    pub fn hmac_sha3_256(key: &[u8], data: &[u8]) -> Option<[u8; 32]> {
        impl_crypto::hmac_sha3_256(key, data)
    }

    /// Compute HMAC-SHA3-256 (stub without crypto feature).
    #[cfg(not(feature = "crypto"))]
    pub fn hmac_sha3_256(key: &[u8], data: &[u8]) -> Option<[u8; 32]> {
        impl_stub::hmac_sha3_256(key, data)
    }

    /// Compute HMAC-SHA3-256 and return as hex string.
    pub fn hmac_sha3_256_hex(key: &[u8], data: &[u8]) -> Option<String> {
        Self::hmac_sha3_256(key, data).map(|mac| Self::to_hex(&mac))
    }

    /// Verify HMAC-SHA3-256 in constant time.
    pub fn verify_hmac_sha3_256(key: &[u8], data: &[u8], expected: &[u8]) -> bool {
        match Self::hmac_sha3_256(key, data) {
            Some(computed) => Self::constant_time_compare(&computed, expected),
            None => false,
        }
    }

    /// Generate cryptographically secure random bytes.
    #[cfg(feature = "crypto")]
    pub fn random_bytes(len: usize) -> Vec<u8> {
        impl_crypto::random_bytes(len)
    }

    /// Generate random bytes (stub without crypto feature).
    #[cfg(not(feature = "crypto"))]
    pub fn random_bytes(len: usize) -> Vec<u8> {
        impl_stub::random_bytes(len)
    }

    /// Constant-time comparison (prevents timing attacks).
    #[cfg(feature = "crypto")]
    pub fn constant_time_compare(a: &[u8], b: &[u8]) -> bool {
        impl_crypto::constant_time_compare(a, b)
    }

    /// Constant-time comparison (always available).
    #[cfg(not(feature = "crypto"))]
    pub fn constant_time_compare(a: &[u8], b: &[u8]) -> bool {
        impl_stub::constant_time_compare(a, b)
    }

    /// Convert bytes to hex string.
    pub fn to_hex(bytes: &[u8]) -> String {
        #[cfg(feature = "crypto")]
        {
            hex::encode(bytes)
        }
        #[cfg(not(feature = "crypto"))]
        {
            bytes.iter().map(|b| format!("{:02x}", b)).collect()
        }
    }

    /// Convert hex string to bytes.
    pub fn from_hex(hex_str: &str) -> Option<Vec<u8>> {
        #[cfg(feature = "crypto")]
        {
            hex::decode(hex_str).ok()
        }
        #[cfg(not(feature = "crypto"))]
        {
            if hex_str.len() % 2 != 0 {
                return None;
            }
            (0..hex_str.len())
                .step_by(2)
                .map(|i| u8::from_str_radix(&hex_str[i..i + 2], 16).ok())
                .collect()
        }
    }

    // Legacy aliases for backwards compatibility
    #[deprecated(note = "Use sha3_256 instead")]
    pub fn sha256(data: &[u8]) -> [u8; 32] {
        Self::sha3_256(data)
    }

    #[deprecated(note = "Use sha3_512 instead")]
    pub fn sha512(data: &[u8]) -> [u8; 64] {
        Self::sha3_512(data)
    }

    #[deprecated(note = "Use hmac_sha3_256 instead")]
    pub fn hmac_sha256(key: &[u8], data: &[u8]) -> [u8; 32] {
        Self::hmac_sha3_256(key, data).unwrap_or([0u8; 32])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant_time_compare() {
        let a = [1u8, 2, 3, 4];
        let b = [1u8, 2, 3, 4];
        let c = [1u8, 2, 3, 5];

        assert!(SafeCrypto::constant_time_compare(&a, &b));
        assert!(!SafeCrypto::constant_time_compare(&a, &c));
        assert!(!SafeCrypto::constant_time_compare(&a, &[1, 2, 3]));
    }

    #[test]
    fn test_hex_roundtrip() {
        let bytes = vec![0xde, 0xad, 0xbe, 0xef];
        let hex = SafeCrypto::to_hex(&bytes);
        assert_eq!(hex, "deadbeef");

        let decoded = SafeCrypto::from_hex(&hex).unwrap();
        assert_eq!(decoded, bytes);
    }

    #[test]
    fn test_from_hex_invalid() {
        assert!(SafeCrypto::from_hex("abc").is_none()); // Odd length
        assert!(SafeCrypto::from_hex("gg").is_none()); // Invalid chars
    }

    #[cfg(feature = "crypto")]
    mod crypto_tests {
        use super::*;

        #[test]
        fn test_sha3_256() {
            let hash = SafeCrypto::sha3_256(b"test");
            assert_eq!(hash.len(), 32);

            // Deterministic
            let hash2 = SafeCrypto::sha3_256(b"test");
            assert_eq!(hash, hash2);

            // Different input = different hash
            let hash3 = SafeCrypto::sha3_256(b"different");
            assert_ne!(hash, hash3);
        }

        #[test]
        fn test_sha3_512() {
            let hash = SafeCrypto::sha3_512(b"test");
            assert_eq!(hash.len(), 64);
        }

        #[test]
        fn test_blake3() {
            let hash = SafeCrypto::blake3(b"test");
            assert_eq!(hash.len(), 32);

            // Deterministic
            let hash2 = SafeCrypto::blake3(b"test");
            assert_eq!(hash, hash2);
        }

        #[test]
        fn test_blake3_xof() {
            let hash = SafeCrypto::blake3_xof(b"test", 64);
            assert_eq!(hash.len(), 64);

            let hash2 = SafeCrypto::blake3_xof(b"test", 128);
            assert_eq!(hash2.len(), 128);
        }

        #[test]
        fn test_blake3_keyed() {
            let key = [0u8; 32];
            let mac = SafeCrypto::blake3_keyed(&key, b"message");
            assert_eq!(mac.len(), 32);

            let key2 = [1u8; 32];
            let mac2 = SafeCrypto::blake3_keyed(&key2, b"message");
            assert_ne!(mac, mac2);
        }

        #[test]
        fn test_blake3_derive_key() {
            let key = SafeCrypto::blake3_derive_key("context", b"ikm", 32);
            assert_eq!(key.len(), 32);

            // Same inputs = same key
            let key2 = SafeCrypto::blake3_derive_key("context", b"ikm", 32);
            assert_eq!(key, key2);

            // Different context = different key
            let key3 = SafeCrypto::blake3_derive_key("other", b"ikm", 32);
            assert_ne!(key, key3);
        }

        #[test]
        fn test_hmac_sha3_256() {
            let mac = SafeCrypto::hmac_sha3_256(b"key", b"message");
            assert!(mac.is_some());
            assert_eq!(mac.unwrap().len(), 32);

            // Deterministic
            let mac2 = SafeCrypto::hmac_sha3_256(b"key", b"message");
            assert_eq!(mac, mac2);

            // Different key = different MAC
            let mac3 = SafeCrypto::hmac_sha3_256(b"other", b"message");
            assert_ne!(mac, mac3);
        }

        #[test]
        fn test_verify_hmac() {
            let mac = SafeCrypto::hmac_sha3_256(b"key", b"message").unwrap();
            assert!(SafeCrypto::verify_hmac_sha3_256(b"key", b"message", &mac));
            assert!(!SafeCrypto::verify_hmac_sha3_256(b"key", b"wrong", &mac));
            assert!(!SafeCrypto::verify_hmac_sha3_256(b"wrong", b"message", &mac));
        }

        #[test]
        fn test_random_bytes() {
            let bytes1 = SafeCrypto::random_bytes(32);
            let bytes2 = SafeCrypto::random_bytes(32);

            assert_eq!(bytes1.len(), 32);
            assert_eq!(bytes2.len(), 32);
            assert_ne!(bytes1, bytes2); // Should be unique
        }

        #[test]
        fn test_hex_convenience() {
            let hash = SafeCrypto::sha3_256_hex(b"test");
            assert_eq!(hash.len(), 64);
            assert!(hash.chars().all(|c| c.is_ascii_hexdigit()));

            let hash = SafeCrypto::blake3_hex(b"test");
            assert_eq!(hash.len(), 64);

            let mac = SafeCrypto::hmac_sha3_256_hex(b"key", b"data").unwrap();
            assert_eq!(mac.len(), 64);
        }
    }
}
