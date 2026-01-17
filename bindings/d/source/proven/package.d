// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Proven - Safe, validated operations library for D.
 *
 * Provides 38 type-safe modules for common operations with overflow checking,
 * XSS prevention, path traversal protection, email validation, IP classification,
 * cryptographic operations, UUID handling, currency/money operations,
 * phone number parsing, hexadecimal encoding, and much more.
 *
 * Version: 0.4.0
 * Module Count: 38
 */
module proven;

/// Library version
enum VERSION = "0.4.0";

/// Total number of modules in this library
enum MODULE_COUNT = 38;

// Core modules (11)
public import proven.safe_math;
public import proven.safe_string;
public import proven.safe_path;
public import proven.safe_email;
public import proven.safe_url;
public import proven.safe_network;
public import proven.safe_crypto;
public import proven.safe_uuid;
public import proven.safe_currency;
public import proven.safe_phone;
public import proven.safe_hex;

// Data modules (7)
public import proven.safe_json;
public import proven.safe_datetime;
public import proven.safe_float;
public import proven.safe_version;
public import proven.safe_color;
public import proven.safe_angle;
public import proven.safe_unit;

// Data structure modules (5)
public import proven.safe_buffer;
public import proven.safe_queue;
public import proven.safe_bloom;
public import proven.safe_lru;
public import proven.safe_graph;

// Resilience modules (4)
public import proven.safe_rate_limiter;
public import proven.safe_circuit_breaker;
public import proven.safe_retry;
public import proven.safe_monotonic;

// State modules (2)
public import proven.safe_state_machine;
public import proven.safe_calculator;

// Algorithm modules (4)
public import proven.safe_geo;
public import proven.safe_probability;
public import proven.safe_checksum;
public import proven.safe_tensor;

// Security modules (2)
public import proven.safe_password;
public import proven.safe_ml;

// HTTP modules (3)
public import proven.safe_header;
public import proven.safe_cookie;
public import proven.safe_content_type;
