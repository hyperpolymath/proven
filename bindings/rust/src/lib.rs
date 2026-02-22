// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! # Proven - Safe, Formally Verified Library (Rust FFI Bindings)
//!
//! This crate provides Rust bindings to the `libproven` shared library.
//! All computation is performed in Idris 2 (with dependent types and
//! totality checking) via a Zig FFI bridge. This Rust crate is a thin
//! wrapper that:
//!
//! 1. Declares `extern "C"` FFI bindings for all 103+ exported functions
//! 2. Provides safe Rust wrappers around the unsafe FFI calls
//! 3. Handles memory management (calls `proven_free_string` for allocated strings)
//! 4. Maps ProvenStatus error codes to Rust `Result` types
//!
//! **No logic is reimplemented in Rust.** Every function calls through to
//! the formally verified Idris 2 implementation.
//!
//! ## Quick Start
//!
//! ```ignore
//! use proven::{SafeMath, SafeString, lifecycle};
//!
//! // Initialize the runtime
//! lifecycle::init().unwrap();
//!
//! // Safe addition with overflow detection
//! let result = SafeMath::add(i64::MAX, 1);
//! assert!(result.is_err());
//!
//! // Safe HTML escaping
//! let escaped = SafeString::escape_html("<script>alert('xss')</script>");
//! assert!(escaped.is_ok());
//!
//! // Cleanup
//! lifecycle::deinit();
//! ```
//!
//! ## Architecture
//!
//! ```text
//! Rust (this crate)  -->  libproven.so (Zig FFI)  -->  Idris 2 RefC
//!    safe wrappers           C ABI bridge              formal proofs
//! ```
//!
//! ## Modules
//!
//! - **Parsing & Validation**: SafeString, SafePath, SafeUrl, SafeEmail,
//!   SafeJson, SafeHeader, SafeCookie, SafeContentType, SafeHex, SafeUuid,
//!   SafeCurrency, SafePhone, SafeDateTime, SafePassword, SafeVersion,
//!   SafeRegistry, SafeDigest, SafeHttp
//! - **Numerical Computing**: SafeMath, SafeFloat, SafeTensor, SafeML,
//!   SafeCalculator, SafeProbability
//! - **Cryptography**: SafeCrypto, SafeChecksum
//! - **Extended Types**: SafeColor, SafeAngle, SafeUnit, SafeGeo
//! - **Data Structures**: SafeBuffer, SafeQueue, SafeBloom, SafeLRU, SafeGraph
//! - **Resilience Patterns**: SafeRateLimiter, SafeCircuitBreaker, SafeRetry
//! - **State Management**: SafeMonotonic, SafeStateMachine
//! - **Infrastructure**: lifecycle, callbacks, version

#![warn(missing_docs)]

// ============================================================================
// Core modules
// ============================================================================

/// Raw FFI declarations for libproven (unsafe).
pub mod ffi;

/// Core error types and result conversion utilities.
pub mod core;

/// Runtime lifecycle management (init/deinit).
pub mod lifecycle;

/// Callback infrastructure for bidirectional FFI.
pub mod callbacks;

/// Library version information.
pub mod version;

// ============================================================================
// Safety modules - Parsing & Validation
// ============================================================================

/// Safe string operations (UTF-8 validation, injection-safe escaping).
pub mod safe_string;

/// Safe filesystem path operations (traversal prevention).
pub mod safe_path;

/// Safe URL parsing and validation (RFC 3986).
pub mod safe_url;

/// Safe email validation (RFC 5321/5322).
pub mod safe_email;

/// Safe JSON validation and type detection.
pub mod safe_json;

/// Safe HTTP header operations (CRLF injection prevention).
pub mod safe_header;

/// Safe HTTP cookie operations (injection prevention).
pub mod safe_cookie;

/// Safe Content-Type operations (MIME sniffing prevention).
pub mod safe_content_type;

/// Safe hexadecimal encoding and decoding.
pub mod safe_hex;

/// Safe UUID generation and parsing (RFC 4122).
pub mod safe_uuid;

/// Safe currency operations (ISO 4217).
pub mod safe_currency;

/// Safe phone number operations (E.164).
pub mod safe_phone;

/// Safe date/time operations (ISO 8601).
pub mod safe_datetime;

/// Safe password validation and strength analysis.
pub mod safe_password;

/// Safe semantic versioning.
pub mod safe_version;

/// Safe OCI image reference parsing.
pub mod safe_registry;

/// Safe cryptographic digest operations.
pub mod safe_digest;

/// Safe HTTP utilities (URL encoding, auth headers).
pub mod safe_http;

// ============================================================================
// Safety modules - Numerical Computing
// ============================================================================

/// Safe arithmetic operations (overflow/underflow detection).
pub mod safe_math;

/// Safe floating-point operations (NaN/Infinity prevention).
pub mod safe_float;

/// Safe tensor (matrix) operations with bounds checking.
pub mod safe_tensor;

/// Safe machine learning utilities (numerically stable).
pub mod safe_ml;

/// Safe expression evaluator.
pub mod safe_calculator;

/// Safe probability values clamped to [0, 1].
pub mod safe_probability;

// ============================================================================
// Safety modules - Cryptography
// ============================================================================

/// Safe cryptographic primitives (constant-time comparison, CSPRNG).
pub mod safe_crypto;

/// Safe checksum operations (CRC-32, Adler-32, etc.).
pub mod safe_checksum;

// ============================================================================
// Safety modules - Extended Types
// ============================================================================

/// Safe color operations (RGB, HSL, hex parsing).
pub mod safe_color;

/// Safe angle operations (degree/radian conversion).
pub mod safe_angle;

/// Safe physical unit conversions.
pub mod safe_unit;

/// Safe geographic coordinate operations.
pub mod safe_geo;

// ============================================================================
// Safety modules - Data Structures
// ============================================================================

/// Safe bounded buffers and ring buffers.
pub mod safe_buffer;

/// Safe bounded FIFO and priority queues.
pub mod safe_queue;

/// Safe Bloom filter (probabilistic set membership).
pub mod safe_bloom;

/// Safe LRU cache (least-recently-used eviction).
pub mod safe_lru;

/// Safe directed graph operations.
pub mod safe_graph;

// ============================================================================
// Safety modules - Resilience Patterns
// ============================================================================

/// Safe rate limiting (token bucket, sliding window).
pub mod safe_rate_limiter;

/// Safe circuit breaker pattern.
pub mod safe_circuit_breaker;

/// Safe retry with exponential backoff.
pub mod safe_retry;

// ============================================================================
// Safety modules - State Management
// ============================================================================

/// Safe monotonically increasing sequences.
pub mod safe_monotonic;

/// Safe type-safe state machine.
pub mod safe_state_machine;

// ============================================================================
// Re-exports for convenience
// ============================================================================

// Core types
pub use crate::core::{Bounded, Error, NonEmpty, Result};

// Parsing & Validation
pub use safe_calculator::SafeCalculator;
pub use safe_content_type::{ContentType, SafeContentType};
pub use safe_content_type::{Charset, MediaCategory};
pub use safe_cookie::{CookieAttributes, CookiePrefix, SafeCookie, SameSite};
pub use safe_cookie::Cookie;
pub use safe_crypto::SafeCrypto;
pub use safe_currency::{CurrencyCode, Money, SafeCurrency};
pub use safe_datetime::SafeDateTime;
pub use safe_digest::SafeDigest;
pub use safe_email::SafeEmail;
pub use safe_header::{Header, SafeHeader};
pub use safe_hex::SafeHex;
pub use safe_http::SafeHttp;
pub use safe_json::SafeJson;
pub use safe_math::SafeMath;
pub use safe_network::SafeNetwork;
pub use safe_password::SafePassword;
pub use safe_path::SafePath;
pub use safe_phone::{PhoneNumber, SafePhone};
pub use safe_registry::SafeRegistry;
pub use safe_string::SafeString;
pub use safe_url::SafeUrl;
pub use safe_uuid::{SafeUuid, Uuid};
pub use safe_version::Version;

// Numerical Computing
pub use safe_float::SafeFloat;
pub use safe_ml::SafeML;
pub use safe_probability::Probability;
pub use safe_tensor::SafeTensor;

// Extended Types
pub use safe_angle::{deg_to_rad, lerp_angle_degrees, normalize_degrees, rad_to_deg};
pub use safe_angle::{Degrees, Radians};
pub use safe_checksum::{adler32, crc32, djb2, fletcher16, fnv1a_32, fnv1a_64, luhn_check, sdbm};
pub use safe_color::{Rgb, Rgba};
pub use safe_geo::{bearing, destination, haversine_distance, BoundingBox, Coordinate};
pub use safe_unit::{
    convert_data, convert_length, convert_mass, convert_temperature, convert_time, DataUnit,
    LengthUnit, MassUnit, TemperatureUnit, TimeUnit,
};

// Data Structures
pub use safe_bloom::BloomFilter;
pub use safe_buffer::{BoundedBuffer, RingBuffer};
pub use safe_lru::LruCache;
pub use safe_queue::{BoundedQueue, PriorityQueue};

// Resilience Patterns
pub use safe_circuit_breaker::{CircuitBreaker, CircuitConfig, CircuitState};
pub use safe_rate_limiter::{FixedWindow, RateLimitResult, SlidingWindow, TokenBucket};
pub use safe_retry::{equal_jitter, exponential_backoff, full_jitter, RetryConfig, RetryState};

// State Management
pub use safe_graph::Graph;
pub use safe_monotonic::{EpochGenerator, HighWaterMark, MonotonicCounter, SequenceGenerator};
pub use safe_state_machine::{StateMachine, StateMachineBuilder};

// Network (used in re-exports but also needs its own module declaration)
/// Safe network address operations (IPv4 parsing, classification).
pub mod safe_network;
