// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! # Proven - Safe, Formally Verified Library
//!
//! Rust bindings for the proven library, providing formally verified
//! implementations for safe math, cryptography, parsing, and validation.
//! Calls Idris 2 verified code via Zig ABI.
//!
//! ## Features
//!
//! - **Safe Math**: Overflow detection, safe division, bounded integers
//! - **Safe Strings**: UTF-8 validation, injection-safe escaping
//! - **Safe JSON**: Exception-free parsing with type-safe access
//! - **Safe URLs**: RFC 3986 compliant parsing
//! - **Safe Email**: RFC 5321/5322 validation
//! - **Safe Paths**: Traversal prevention, glob matching
//! - **Safe Crypto**: Secure hashing, HMAC, random generation (stubs)
//! - **Safe Passwords**: Policy validation, strength analysis
//! - **Safe DateTime**: ISO 8601 parsing, timezone handling
//! - **Safe Network**: IPv4/IPv6 parsing, CIDR, ports
//! - **Safe UUID**: RFC 4122 UUID generation and validation
//! - **Safe Currency**: Type-safe monetary values with ISO 4217 codes
//! - **Safe Phone**: E.164 phone number parsing and formatting
//! - **Safe Hex**: Hexadecimal encoding and decoding
//! - **Safe Header**: HTTP header validation, CRLF injection prevention (v0.8.0)
//! - **Safe Cookie**: HTTP cookie validation, injection prevention (v0.8.0)
//! - **Safe ContentType**: MIME type validation, sniffing prevention (v0.8.0)
//! - **Safe Float**: NaN/Infinity prevention, safe division for f32/f64 (v0.9.0)
//! - **Safe Tensor**: Bounds-checked vector/matrix ops, shape validation (v0.9.0)
//! - **Safe ML**: Numerically stable softmax, loss functions, activations (v0.9.0)
//! - **Safe Calculator**: Expression evaluator with overflow/division protection (v1.0.0)
//! - **Safe Buffer**: Bounded buffers and ring buffers (v1.0.0)
//! - **Safe Color**: RGB/RGBA with WCAG contrast calculations (v1.0.0)
//! - **Safe Queue**: Bounded FIFO and priority queues (v1.0.0)
//! - **Safe Bloom**: Probabilistic set membership (v1.0.0)
//! - **Safe LRU**: Least-recently-used cache (v1.0.0)
//! - **Safe Rate Limiter**: Token bucket, sliding window rate limiting (v1.0.0)
//! - **Safe Circuit Breaker**: Fault tolerance pattern (v1.0.0)
//! - **Safe Retry**: Exponential backoff with jitter (v1.0.0)
//! - **Safe Version**: Semantic versioning parsing (v1.0.0)
//! - **Safe Geo**: Geographic coordinates, Haversine distance (v1.0.0)
//! - **Safe Angle**: Degree/radian conversions (v1.0.0)
//! - **Safe Checksum**: CRC-32, Adler-32, FNV, Luhn (v1.0.0)
//! - **Safe Probability**: Probability values clamped to [0,1] (v1.0.0)
//! - **Safe Unit**: Physical unit conversions (v1.0.0)
//! - **Safe Monotonic**: Monotonically increasing sequences (v1.0.0)
//! - **Safe State Machine**: Type-safe state transitions (v1.0.0)
//! - **Safe Graph**: Directed graph with cycle detection (v1.0.0)
//!
//! ## Example
//!
//! ```rust
//! use proven::{SafeMath, SafeString, Result};
//!
//! // Safe addition with overflow detection
//! let result = SafeMath::add(i64::MAX, 1);
//! assert!(result.is_err());
//!
//! // Safe HTML escaping
//! let escaped = SafeString::escape_html("<script>alert('xss')</script>");
//! assert!(!escaped.contains('<'));
//! ```

#![forbid(unsafe_code)]
#![warn(missing_docs)]
#![cfg_attr(docsrs, feature(doc_cfg))]

// Core module
pub mod core;

// Safety modules - Parsing & Validation
pub mod safe_calculator;
pub mod safe_content_type;
pub mod safe_cookie;
pub mod safe_crypto;
pub mod safe_currency;
pub mod safe_datetime;
pub mod safe_email;
pub mod safe_header;
pub mod safe_hex;
pub mod safe_json;
pub mod safe_math;
pub mod safe_network;
pub mod safe_password;
pub mod safe_path;
pub mod safe_phone;
pub mod safe_string;
pub mod safe_url;
pub mod safe_uuid;

// Safety modules - Numerical Computing
pub mod safe_float;
pub mod safe_ml;
pub mod safe_tensor;

// Safety modules - Extended Types
pub mod safe_angle;
pub mod safe_checksum;
pub mod safe_color;
pub mod safe_geo;
pub mod safe_probability;
pub mod safe_unit;
pub mod safe_version;

// Safety modules - Data Structures
pub mod safe_bloom;
pub mod safe_buffer;
pub mod safe_lru;
pub mod safe_queue;

// Safety modules - Resilience Patterns
pub mod safe_circuit_breaker;
pub mod safe_rate_limiter;
pub mod safe_retry;

// Safety modules - State Management
pub mod safe_graph;
pub mod safe_monotonic;
pub mod safe_state_machine;

// Re-exports for convenience
pub use crate::core::{Bounded, Error, NonEmpty, Result};
pub use safe_calculator::SafeCalculator;
pub use safe_content_type::{ContentType, MediaCategory, SafeContentType};
pub use safe_cookie::{Cookie, CookieAttributes, CookiePrefix, SafeCookie, SameSite};
pub use safe_crypto::SafeCrypto;
pub use safe_currency::{CurrencyCode, Money, SafeCurrency};
pub use safe_datetime::SafeDateTime;
pub use safe_email::SafeEmail;
pub use safe_header::{Header, SafeHeader};
pub use safe_hex::SafeHex;
pub use safe_json::SafeJson;
pub use safe_math::SafeMath;
pub use safe_network::SafeNetwork;
pub use safe_password::SafePassword;
pub use safe_path::SafePath;
pub use safe_phone::{PhoneNumber, SafePhone};
pub use safe_string::SafeString;
pub use safe_url::SafeUrl;
pub use safe_uuid::{SafeUuid, Uuid};
// ML and numerical computing modules (v0.9.0)
pub use safe_float::SafeFloat;
pub use safe_ml::SafeML;
pub use safe_tensor::SafeTensor;

// Extended types (v1.0.0)
pub use safe_angle::{deg_to_rad, lerp_angle_degrees, normalize_degrees, rad_to_deg, Degrees, Radians};
pub use safe_checksum::{adler32, crc32, djb2, fletcher16, fnv1a_32, fnv1a_64, luhn_check, sdbm};
pub use safe_color::{Rgb, Rgba};
pub use safe_geo::{bearing, destination, haversine_distance, BoundingBox, Coordinate};
pub use safe_probability::Probability;
pub use safe_unit::{
    convert_data, convert_length, convert_mass, convert_temperature, convert_time, DataUnit,
    LengthUnit, MassUnit, TemperatureUnit, TimeUnit,
};
pub use safe_version::Version;

// Data structures (v1.0.0)
pub use safe_bloom::BloomFilter;
pub use safe_buffer::{BoundedBuffer, RingBuffer};
pub use safe_lru::LruCache;
pub use safe_queue::{BoundedQueue, PriorityQueue};

// Resilience patterns (v1.0.0)
pub use safe_circuit_breaker::{CircuitBreaker, CircuitConfig, CircuitState};
pub use safe_rate_limiter::{FixedWindow, RateLimitResult, SlidingWindow, TokenBucket};
pub use safe_retry::{equal_jitter, exponential_backoff, full_jitter, RetryConfig, RetryState};

// State management (v1.0.0)
pub use safe_graph::Graph;
pub use safe_monotonic::{EpochGenerator, HighWaterMark, MonotonicCounter, SequenceGenerator};
pub use safe_state_machine::{StateMachine, StateMachineBuilder};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safe_add_overflow() {
        let result = SafeMath::add(i64::MAX, 1);
        assert!(result.is_err());
    }

    #[test]
    fn test_safe_add_normal() {
        let result = SafeMath::add(1, 2);
        assert_eq!(result.unwrap(), 3);
    }

    #[test]
    fn test_safe_div_by_zero() {
        let result = SafeMath::div(10, 0);
        assert!(result.is_err());
    }

    #[test]
    fn test_html_escape() {
        let escaped = SafeString::escape_html("<script>");
        assert_eq!(escaped, "&lt;script&gt;");
    }

    #[test]
    fn test_sql_escape() {
        let escaped = SafeString::escape_sql("O'Brien");
        assert_eq!(escaped, "O''Brien");
    }
}
