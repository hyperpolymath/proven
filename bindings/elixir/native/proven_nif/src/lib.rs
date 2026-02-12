// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Rustler NIF bindings for Proven safety library.
//!
//! This module provides NIF (Native Implemented Functions) for Elixir
//! that wrap the Proven Rust library's safety-first operations.

use rustler::{Atom, NifResult, NifStruct, NifUnitEnum};

mod atoms {
    rustler::atoms! {
        ok,
        error,
        overflow,
        underflow,
        division_by_zero,
        invalid_input,
        out_of_range,
        empty_input,
        validation_error,
        too_long,
        invalid_format,
    }
}

// Re-export all NIF functions
rustler::init!("Elixir.Proven.Native");

// ============================================================================
// SafeMath NIFs
// ============================================================================

#[rustler::nif]
fn math_add(a: i64, b: i64) -> NifResult<(Atom, i64)> {
    match a.checked_add(b) {
        Some(result) => Ok((atoms::ok(), result)),
        None => Err(rustler::Error::Term(Box::new(atoms::overflow()))),
    }
}

#[rustler::nif]
fn math_sub(a: i64, b: i64) -> NifResult<(Atom, i64)> {
    match a.checked_sub(b) {
        Some(result) => Ok((atoms::ok(), result)),
        None => Err(rustler::Error::Term(Box::new(atoms::underflow()))),
    }
}

#[rustler::nif]
fn math_mul(a: i64, b: i64) -> NifResult<(Atom, i64)> {
    match a.checked_mul(b) {
        Some(result) => Ok((atoms::ok(), result)),
        None => Err(rustler::Error::Term(Box::new(atoms::overflow()))),
    }
}

#[rustler::nif]
fn math_div(a: i64, b: i64) -> NifResult<(Atom, i64)> {
    if b == 0 {
        Err(rustler::Error::Term(Box::new(atoms::division_by_zero())))
    } else {
        match a.checked_div(b) {
            Some(result) => Ok((atoms::ok(), result)),
            None => Err(rustler::Error::Term(Box::new(atoms::overflow()))),
        }
    }
}

#[rustler::nif]
fn math_mod(a: i64, b: i64) -> NifResult<(Atom, i64)> {
    if b == 0 {
        Err(rustler::Error::Term(Box::new(atoms::division_by_zero())))
    } else {
        match a.checked_rem(b) {
            Some(result) => Ok((atoms::ok(), result)),
            None => Err(rustler::Error::Term(Box::new(atoms::overflow()))),
        }
    }
}

// ============================================================================
// SafeFloat NIFs
// ============================================================================

#[rustler::nif]
fn float_is_finite(value: f64) -> bool {
    value.is_finite()
}

#[rustler::nif]
fn float_is_nan(value: f64) -> bool {
    value.is_nan()
}

#[rustler::nif]
fn float_clamp(value: f64, min: f64, max: f64) -> f64 {
    value.clamp(min, max)
}

#[rustler::nif]
fn float_approximately_equal(a: f64, b: f64, epsilon: f64) -> bool {
    (a - b).abs() < epsilon
}

// ============================================================================
// SafeChecksum NIFs
// ============================================================================

#[rustler::nif]
fn checksum_crc32(data: rustler::Binary) -> u32 {
    const TABLE: [u32; 256] = generate_crc32_table();
    let mut crc = 0xFFFFFFFF_u32;
    for byte in data.as_slice() {
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

#[rustler::nif]
fn checksum_adler32(data: rustler::Binary) -> u32 {
    const MOD_ADLER: u32 = 65521;
    let mut a: u32 = 1;
    let mut b: u32 = 0;

    for byte in data.as_slice() {
        a = (a + *byte as u32) % MOD_ADLER;
        b = (b + a) % MOD_ADLER;
    }

    (b << 16) | a
}

#[rustler::nif]
fn checksum_fnv1a_64(data: rustler::Binary) -> u64 {
    const FNV_OFFSET: u64 = 0xcbf29ce484222325;
    const FNV_PRIME: u64 = 0x100000001b3;

    let mut hash = FNV_OFFSET;
    for byte in data.as_slice() {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(FNV_PRIME);
    }
    hash
}

#[rustler::nif]
fn checksum_luhn_check(digits: &str) -> bool {
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

// ============================================================================
// SafeGeo NIFs
// ============================================================================

use std::f64::consts::PI;

const EARTH_RADIUS_KM: f64 = 6371.0;

#[rustler::nif]
fn geo_validate_coordinate(lat: f64, lon: f64) -> NifResult<(Atom, (f64, f64))> {
    if !(-90.0..=90.0).contains(&lat) {
        return Err(rustler::Error::Term(Box::new(atoms::out_of_range())));
    }
    if !(-180.0..=180.0).contains(&lon) {
        return Err(rustler::Error::Term(Box::new(atoms::out_of_range())));
    }
    if lat.is_nan() || lon.is_nan() {
        return Err(rustler::Error::Term(Box::new(atoms::invalid_input())));
    }
    Ok((atoms::ok(), (lat, lon)))
}

#[rustler::nif]
fn geo_haversine_distance(lat1: f64, lon1: f64, lat2: f64, lon2: f64) -> f64 {
    let lat1_rad = lat1 * PI / 180.0;
    let lat2_rad = lat2 * PI / 180.0;
    let dlat = (lat2 - lat1) * PI / 180.0;
    let dlon = (lon2 - lon1) * PI / 180.0;

    let a = (dlat / 2.0).sin().powi(2) + lat1_rad.cos() * lat2_rad.cos() * (dlon / 2.0).sin().powi(2);
    let c = 2.0 * a.sqrt().asin();

    EARTH_RADIUS_KM * c
}

#[rustler::nif]
fn geo_bearing(lat1: f64, lon1: f64, lat2: f64, lon2: f64) -> f64 {
    let lat1_rad = lat1 * PI / 180.0;
    let lat2_rad = lat2 * PI / 180.0;
    let dlon = (lon2 - lon1) * PI / 180.0;

    let y = dlon.sin() * lat2_rad.cos();
    let x = lat1_rad.cos() * lat2_rad.sin() - lat1_rad.sin() * lat2_rad.cos() * dlon.cos();

    let bearing = y.atan2(x) * 180.0 / PI;
    (bearing + 360.0) % 360.0
}

// ============================================================================
// SafeProbability NIFs
// ============================================================================

#[rustler::nif]
fn probability_clamp(value: f64) -> f64 {
    value.clamp(0.0, 1.0)
}

#[rustler::nif]
fn probability_validate(value: f64) -> NifResult<(Atom, f64)> {
    if value.is_nan() {
        return Err(rustler::Error::Term(Box::new(atoms::invalid_input())));
    }
    if !(0.0..=1.0).contains(&value) {
        return Err(rustler::Error::Term(Box::new(atoms::out_of_range())));
    }
    Ok((atoms::ok(), value))
}

#[rustler::nif]
fn probability_complement(value: f64) -> f64 {
    1.0 - value.clamp(0.0, 1.0)
}

#[rustler::nif]
fn probability_and(a: f64, b: f64) -> f64 {
    (a.clamp(0.0, 1.0)) * (b.clamp(0.0, 1.0))
}

#[rustler::nif]
fn probability_or(a: f64, b: f64) -> f64 {
    let a_clamped = a.clamp(0.0, 1.0);
    let b_clamped = b.clamp(0.0, 1.0);
    (a_clamped + b_clamped - a_clamped * b_clamped).clamp(0.0, 1.0)
}

// ============================================================================
// SafeTensor NIFs
// ============================================================================

#[rustler::nif]
fn tensor_dot(a: Vec<f64>, b: Vec<f64>) -> NifResult<(Atom, f64)> {
    if a.len() != b.len() {
        return Err(rustler::Error::Term(Box::new(atoms::validation_error())));
    }
    let result: f64 = a.iter().zip(b.iter()).map(|(x, y)| x * y).sum();
    Ok((atoms::ok(), result))
}

#[rustler::nif]
fn tensor_add(a: Vec<f64>, b: Vec<f64>) -> NifResult<(Atom, Vec<f64>)> {
    if a.len() != b.len() {
        return Err(rustler::Error::Term(Box::new(atoms::validation_error())));
    }
    let result: Vec<f64> = a.iter().zip(b.iter()).map(|(x, y)| x + y).collect();
    Ok((atoms::ok(), result))
}

#[rustler::nif]
fn tensor_scale(v: Vec<f64>, scalar: f64) -> Vec<f64> {
    v.iter().map(|x| x * scalar).collect()
}

#[rustler::nif]
fn tensor_argmax(v: Vec<f64>) -> NifResult<(Atom, usize)> {
    if v.is_empty() {
        return Err(rustler::Error::Term(Box::new(atoms::empty_input())));
    }
    let (idx, _) = v
        .iter()
        .enumerate()
        .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        .unwrap();
    Ok((atoms::ok(), idx))
}

// ============================================================================
// SafeML NIFs
// ============================================================================

#[rustler::nif]
fn ml_softmax(logits: Vec<f32>) -> NifResult<(Atom, Vec<f32>)> {
    if logits.is_empty() {
        return Err(rustler::Error::Term(Box::new(atoms::empty_input())));
    }

    let max = logits.iter().copied().fold(f32::NEG_INFINITY, f32::max);
    let exps: Vec<f32> = logits.iter().map(|&x| (x - max).exp()).collect();
    let sum: f32 = exps.iter().sum();

    if sum < f32::EPSILON {
        let uniform = 1.0 / logits.len() as f32;
        return Ok((atoms::ok(), vec![uniform; logits.len()]));
    }

    Ok((atoms::ok(), exps.iter().map(|&e| e / sum).collect()))
}

#[rustler::nif]
fn ml_relu(x: f32) -> f32 {
    x.max(0.0)
}

#[rustler::nif]
fn ml_sigmoid(x: f32) -> f32 {
    let x_clamped = x.clamp(-88.0, 88.0);
    1.0 / (1.0 + (-x_clamped).exp())
}

#[rustler::nif]
fn ml_cross_entropy(predictions: Vec<f32>, targets: Vec<f32>) -> NifResult<(Atom, f32)> {
    if predictions.is_empty() || targets.is_empty() {
        return Err(rustler::Error::Term(Box::new(atoms::empty_input())));
    }
    if predictions.len() != targets.len() {
        return Err(rustler::Error::Term(Box::new(atoms::validation_error())));
    }

    const EPSILON: f32 = 1e-7;
    let mut loss = 0.0f32;

    for (&p, &t) in predictions.iter().zip(targets.iter()) {
        let p_safe = p.max(EPSILON);
        loss -= t * p_safe.ln();
    }

    if !loss.is_finite() {
        return Err(rustler::Error::Term(Box::new(atoms::overflow())));
    }

    Ok((atoms::ok(), loss))
}

// ============================================================================
// SafePassword NIFs
// ============================================================================

#[derive(NifUnitEnum, Clone, Copy)]
pub enum PasswordStrength {
    VeryWeak,
    Weak,
    Fair,
    Strong,
    VeryStrong,
}

#[rustler::nif]
fn password_strength(password: &str) -> PasswordStrength {
    let len = password.len();
    let has_upper = password.chars().any(|c| c.is_uppercase());
    let has_lower = password.chars().any(|c| c.is_lowercase());
    let has_digit = password.chars().any(|c| c.is_ascii_digit());
    let has_special = password.chars().any(|c| !c.is_alphanumeric() && !c.is_whitespace());

    let variety = [has_upper, has_lower, has_digit, has_special]
        .iter()
        .filter(|&&b| b)
        .count();

    match (len, variety) {
        (0..=5, _) => PasswordStrength::VeryWeak,
        (6..=7, 0..=1) => PasswordStrength::VeryWeak,
        (6..=7, _) => PasswordStrength::Weak,
        (8..=11, 0..=2) => PasswordStrength::Weak,
        (8..=11, _) => PasswordStrength::Fair,
        (12..=15, 0..=2) => PasswordStrength::Fair,
        (12..=15, _) => PasswordStrength::Strong,
        (_, 0..=2) => PasswordStrength::Fair,
        (_, 3) => PasswordStrength::Strong,
        _ => PasswordStrength::VeryStrong,
    }
}

#[rustler::nif]
fn password_has_common_pattern(password: &str) -> bool {
    let lower = password.to_lowercase();
    let common = [
        "password", "123456", "qwerty", "admin", "letmein", "welcome", "monkey", "dragon",
        "master", "login",
    ];
    common.iter().any(|&p| lower.contains(p))
}

// ============================================================================
// SafeHeader NIFs
// ============================================================================

#[rustler::nif]
fn header_has_crlf(s: &str) -> bool {
    s.contains('\r') || s.contains('\n')
}

#[rustler::nif]
fn header_is_valid_name(name: &str) -> bool {
    if name.is_empty() || name.len() > 256 {
        return false;
    }
    name.chars().all(|c| matches!(c,
        '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' |
        '^' | '_' | '`' | '|' | '~' |
        'a'..='z' | 'A'..='Z' | '0'..='9'
    ))
}

#[rustler::nif]
fn header_is_dangerous(name: &str) -> bool {
    const DANGEROUS_HEADERS: &[&str] = &[
        "proxy-authorization",
        "proxy-authenticate",
        "proxy-connection",
        "transfer-encoding",
        "content-length",
        "host",
        "connection",
        "keep-alive",
        "upgrade",
        "te",
        "trailer",
    ];
    let lower = name.to_lowercase();
    DANGEROUS_HEADERS.contains(&lower.as_str())
}

// ============================================================================
// SafeCookie NIFs
// ============================================================================

#[rustler::nif]
fn cookie_has_injection(s: &str) -> bool {
    s.contains(';') || s.contains('\r') || s.contains('\n')
}

#[derive(NifUnitEnum, Clone, Copy)]
pub enum CookiePrefix {
    NoPrefix,
    SecurePrefix,
    HostPrefix,
}

#[rustler::nif]
fn cookie_get_prefix(name: &str) -> CookiePrefix {
    if name.starts_with("__Host-") {
        CookiePrefix::HostPrefix
    } else if name.starts_with("__Secure-") {
        CookiePrefix::SecurePrefix
    } else {
        CookiePrefix::NoPrefix
    }
}

// ============================================================================
// SafeContentType NIFs
// ============================================================================

#[rustler::nif]
fn content_type_can_sniff_to_dangerous(s: &str) -> bool {
    const SNIFFABLE: &[&str] = &[
        "text/plain",
        "application/octet-stream",
        "application/x-unknown",
        "unknown/unknown",
    ];
    SNIFFABLE.contains(&s.to_lowercase().as_str())
}

#[rustler::nif]
fn content_type_is_json(media_type: &str, subtype: &str, suffix: Option<String>) -> bool {
    subtype == "json" || suffix.as_deref() == Some("json")
}

#[rustler::nif]
fn content_type_is_xml(subtype: &str, suffix: Option<String>) -> bool {
    subtype == "xml" || suffix.as_deref() == Some("xml")
}
