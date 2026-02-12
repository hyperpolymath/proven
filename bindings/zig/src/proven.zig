// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Proven - Safety-first utility functions with formal verification guarantees.
//!
//! Provides 87 modules of safe operations that cannot crash:
//!
//! Core Modules (18):
//! - Math: Division, modulo, checked arithmetic with overflow detection
//! - Strings: HTML/SQL/JS escaping, safe operations
//! - Paths: Traversal detection and filename sanitization
//! - Email: Validation, parsing, normalization
//! - URL: RFC 3986 compliant parsing
//! - Network: IPv4/IPv6 parsing, CIDR notation
//! - Crypto: Constant-time comparison, secure zeroing
//! - UUID: RFC 4122 UUID generation and validation
//! - Currency: Type-safe monetary values with ISO 4217 codes
//! - Phone: E.164 phone number parsing and formatting
//! - Hex: Hexadecimal encoding and decoding
//! - Env: Environment variable handling with validation
//! - File: Safe file operations with path validation
//! - FiniteField: Finite field arithmetic (modular arithmetic)
//! - Args: Command-line argument parsing with validation
//! - DNS: DNS record parsing and validation
//! - Docker: Docker image/container name validation
//! - Decimal: Arbitrary precision decimal arithmetic
//!
//! Encoding Modules (5):
//! - Base64: RFC 4648 Base64 encoding/decoding
//! - HTML: HTML entity escaping and validation
//! - XML: XML escaping and validation
//! - JSON: Exception-free parsing with validation
//! - Markdown: Markdown parsing and safe rendering
//!
//! Data Format Modules (11):
//! - DateTime: ISO 8601 parsing, date validation
//! - Float: Safe floating-point operations with NaN/Inf handling
//! - Version: Semantic versioning parsing and comparison
//! - Color: Color space conversions and validation
//! - Angle: Degree/radian conversions with normalization
//! - Unit: Physical unit conversions with dimension checking
//! - TOML: TOML configuration parsing
//! - YAML: Safe YAML subset parsing with attack prevention
//! - CSV: RFC 4180 compliant CSV parsing with escaping
//! - Cron: Cron expression parsing and validation
//! - BibTeX: BibTeX entry parsing and validation
//!
//! Numeric Modules (5):
//! - Complex: Complex number arithmetic with overflow protection
//! - Matrix: Dense matrix operations with dimension checking
//! - Rational: Rational number arithmetic
//! - Interval: Interval arithmetic operations
//! - Probability: Probability values clamped to [0, 1]
//!
//! Data Structure Modules (10):
//! - Buffer: Bounded buffer operations with overflow protection
//! - Queue: Bounded queue with capacity guarantees
//! - Bloom: Probabilistic set membership with tunable false positives
//! - LRU: Least-recently-used cache with bounded capacity
//! - Graph: Safe graph operations with cycle detection
//! - BitSet: Fixed-size bitset operations with bounds checking
//! - Heap: Binary heap with guaranteed properties
//! - Tree: Tree operations with safe traversal
//! - Set: Set operations with membership tracking
//! - UnionFind: Disjoint set with path compression and union by rank
//!
//! Resilience Modules (6):
//! - RateLimiter: Token bucket rate limiting
//! - CircuitBreaker: Fault tolerance with automatic recovery
//! - Retry: Exponential backoff with jitter
//! - Monotonic: Monotonically increasing sequence generation
//! - Consensus: Distributed consensus quorum calculations (Raft, BFT, Paxos)
//! - Semaphore: Counting semaphore for resource limiting
//!
//! State Modules (3):
//! - StateMachine: Type-safe state machine transitions
//! - Calculator: Expression parser and calculator
//! - Transaction: ACID transaction state management
//!
//! Algorithm Modules (5):
//! - Geo: Geographic coordinate validation and operations
//! - Checksum: CRC, hash, and integrity verification
//! - Tensor: Tensor/matrix operations
//! - Ordering: Total ordering and comparison utilities
//! - Log: Structured logging with levels
//!
//! Security Modules (9):
//! - Password: Password validation and generation
//! - ML: Machine learning utilities
//! - Cert: X.509 certificate validation and parsing
//! - Command: Safe command execution with sanitization
//! - Capability: Capability-based security tokens
//! - OAuth: OAuth 2.0 token and scope validation
//! - JWT: JSON Web Token creation and validation
//! - Policy: Policy evaluation and enforcement
//! - Regex: Regular expression validation and safe matching
//!
//! Infrastructure Modules (8):
//! - Git: Git object and ref validation
//! - SSH: SSH key and config validation
//! - Template: Template string interpolation
//! - Archive: Archive format handling (tar, zip)
//! - Resource: Resource lifecycle management
//! - Schema: JSON Schema validation
//! - Provenance: Data provenance tracking
//! - I18n: Internationalization utilities
//!
//! HTTP/Web Modules (5):
//! - Header: HTTP header validation
//! - Cookie: HTTP cookie handling
//! - ContentType: MIME type handling
//! - HTTP: HTTP request/response handling
//! - Webhook: Webhook URL and signature validation
//!
//! Protocol Modules (2):
//! - MCP: Model Context Protocol message validation
//! - SQL: SQL query validation and escaping

const std = @import("std");

/// Library version
pub const VERSION = "0.5.0";

/// Total module count
pub const MODULE_COUNT = 87;

// ============================================================================
// Core Modules (18)
// ============================================================================

pub const safe_math = @import("safe_math.zig");
pub const safe_string = @import("safe_string.zig");
pub const safe_path = @import("safe_path.zig");
pub const safe_email = @import("safe_email.zig");
pub const safe_url = @import("safe_url.zig");
pub const safe_network = @import("safe_network.zig");
pub const safe_crypto = @import("safe_crypto.zig");
pub const safe_uuid = @import("safe_uuid.zig");
pub const safe_currency = @import("safe_currency.zig");
pub const safe_phone = @import("safe_phone.zig");
pub const safe_hex = @import("safe_hex.zig");
pub const safe_env = @import("safe_env.zig");
pub const safe_file = @import("safe_file.zig");
pub const safe_finite_field = @import("safe_finite_field.zig");
pub const safe_args = @import("safe_args.zig");
pub const safe_dns = @import("safe_dns.zig");
pub const safe_docker = @import("safe_docker.zig");
pub const safe_decimal = @import("safe_decimal.zig");

// ============================================================================
// Encoding Modules (5)
// ============================================================================

pub const safe_base64 = @import("safe_base64.zig");
pub const safe_html = @import("safe_html.zig");
pub const safe_xml = @import("safe_xml.zig");
pub const safe_json = @import("safe_json.zig");
pub const safe_markdown = @import("safe_markdown.zig");

// ============================================================================
// Data Format Modules (11)
// ============================================================================

pub const safe_datetime = @import("safe_datetime.zig");
pub const safe_float = @import("safe_float.zig");
pub const safe_version = @import("safe_version.zig");
pub const safe_color = @import("safe_color.zig");
pub const safe_angle = @import("safe_angle.zig");
pub const safe_unit = @import("safe_unit.zig");
pub const safe_toml = @import("safe_toml.zig");
pub const safe_yaml = @import("safe_yaml.zig");
pub const safe_csv = @import("safe_csv.zig");
pub const safe_cron = @import("safe_cron.zig");
pub const safe_bibtex = @import("safe_bibtex.zig");

// ============================================================================
// Numeric Modules (5)
// ============================================================================

pub const safe_complex = @import("safe_complex.zig");
pub const safe_matrix = @import("safe_matrix.zig");
pub const safe_rational = @import("safe_rational.zig");
pub const safe_interval = @import("safe_interval.zig");
pub const safe_probability = @import("safe_probability.zig");

// ============================================================================
// Data Structure Modules (10)
// ============================================================================

pub const safe_buffer = @import("safe_buffer.zig");
pub const safe_queue = @import("safe_queue.zig");
pub const safe_bloom = @import("safe_bloom.zig");
pub const safe_lru = @import("safe_lru.zig");
pub const safe_graph = @import("safe_graph.zig");
pub const safe_bitset = @import("safe_bitset.zig");
pub const safe_heap = @import("safe_heap.zig");
pub const safe_tree = @import("safe_tree.zig");
pub const safe_set = @import("safe_set.zig");
pub const safe_union_find = @import("safe_union_find.zig");

// ============================================================================
// Resilience Modules (6)
// ============================================================================

pub const safe_rate_limiter = @import("safe_rate_limiter.zig");
pub const safe_circuit_breaker = @import("safe_circuit_breaker.zig");
pub const safe_retry = @import("safe_retry.zig");
pub const safe_monotonic = @import("safe_monotonic.zig");
pub const safe_consensus = @import("safe_consensus.zig");
pub const safe_semaphore = @import("safe_semaphore.zig");

// ============================================================================
// State Modules (3)
// ============================================================================

pub const safe_state_machine = @import("safe_state_machine.zig");
pub const safe_calculator = @import("safe_calculator.zig");
pub const safe_transaction = @import("safe_transaction.zig");

// ============================================================================
// Algorithm Modules (5)
// ============================================================================

pub const safe_geo = @import("safe_geo.zig");
pub const safe_checksum = @import("safe_checksum.zig");
pub const safe_tensor = @import("safe_tensor.zig");
pub const safe_ordering = @import("safe_ordering.zig");
pub const safe_log = @import("safe_log.zig");

// ============================================================================
// Security Modules (9)
// ============================================================================

pub const safe_password = @import("safe_password.zig");
pub const safe_ml = @import("safe_ml.zig");
pub const safe_cert = @import("safe_cert.zig");
pub const safe_command = @import("safe_command.zig");
pub const safe_capability = @import("safe_capability.zig");
pub const safe_oauth = @import("safe_oauth.zig");
pub const safe_jwt = @import("safe_jwt.zig");
pub const safe_policy = @import("safe_policy.zig");
pub const safe_regex = @import("safe_regex.zig");

// ============================================================================
// Infrastructure Modules (8)
// ============================================================================

pub const safe_git = @import("safe_git.zig");
pub const safe_ssh = @import("safe_ssh.zig");
pub const safe_template = @import("safe_template.zig");
pub const safe_archive = @import("safe_archive.zig");
pub const safe_resource = @import("safe_resource.zig");
pub const safe_schema = @import("safe_schema.zig");
pub const safe_provenance = @import("safe_provenance.zig");
pub const safe_i18n = @import("safe_i18n.zig");

// ============================================================================
// HTTP/Web Modules (5)
// ============================================================================

pub const safe_header = @import("safe_header.zig");
pub const safe_cookie = @import("safe_cookie.zig");
pub const safe_content_type = @import("safe_content_type.zig");
pub const safe_http = @import("safe_http.zig");
pub const safe_webhook = @import("safe_webhook.zig");

// ============================================================================
// Protocol Modules (2)
// ============================================================================

pub const safe_mcp = @import("safe_mcp.zig");
pub const safe_sql = @import("safe_sql.zig");

test {
    std.testing.refAllDecls(@This());
}
