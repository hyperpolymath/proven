// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Proven - Safe, validated operations library for D.
 *
 * Provides type-safe operations with overflow checking, XSS prevention,
 * path traversal protection, email validation, IP classification,
 * cryptographic operations, UUID handling, currency/money operations,
 * phone number parsing, and hexadecimal encoding.
 */
module proven;

public import proven.safe_math;
public import proven.safe_string;
public import proven.safe_path;
public import proven.safe_email;
public import proven.safe_network;
public import proven.safe_crypto;
public import proven.safe_uuid;
public import proven.safe_currency;
public import proven.safe_phone;
public import proven.safe_hex;
