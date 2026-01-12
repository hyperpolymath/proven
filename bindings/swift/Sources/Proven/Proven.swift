// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Proven - Safety-first utility functions with formal verification guarantees.
///
/// Provides safe operations for:
/// - Math: Division, modulo, checked arithmetic with overflow detection
/// - Strings: HTML/SQL/JS/URL escaping, safe truncation
/// - Paths: Traversal detection and filename sanitization
/// - Email: Validation, parsing, normalization
/// - URLs: Parsing with full component extraction
/// - Network: IPv4 parsing, private/loopback/public classification
/// - Crypto: Constant-time comparison, secure zeroing

// Re-export all modules
@_exported import struct Foundation.Data
