// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe X.509 certificate validation and parsing operations.
//!
//! Provides certificate validation functions that cannot crash and always
//! return explicit error types. Handles common certificate parsing tasks
//! including validity period checks, issuer/subject extraction, and
//! basic constraint validation.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for certificate operations.
pub const CertError = error{
    InvalidFormat,
    InvalidPem,
    InvalidDer,
    ExpiredCertificate,
    NotYetValid,
    InvalidSignature,
    InvalidIssuer,
    InvalidSubject,
    MissingRequiredField,
    UnsupportedVersion,
    ChainValidationFailed,
    SelfSignedNotAllowed,
    OutOfMemory,
};

/// Certificate validity period.
pub const ValidityPeriod = struct {
    not_before: i64,
    not_after: i64,

    /// Check if the certificate is currently valid.
    pub fn isValid(self: ValidityPeriod, current_time: i64) bool {
        return current_time >= self.not_before and current_time <= self.not_after;
    }

    /// Check if the certificate has expired.
    pub fn isExpired(self: ValidityPeriod, current_time: i64) bool {
        return current_time > self.not_after;
    }

    /// Check if the certificate is not yet valid.
    pub fn isNotYetValid(self: ValidityPeriod, current_time: i64) bool {
        return current_time < self.not_before;
    }

    /// Get remaining validity in seconds (negative if expired).
    pub fn remainingSeconds(self: ValidityPeriod, current_time: i64) i64 {
        return self.not_after - current_time;
    }
};

/// Distinguished Name (DN) components.
pub const DistinguishedName = struct {
    common_name: ?[]const u8 = null,
    organization: ?[]const u8 = null,
    organizational_unit: ?[]const u8 = null,
    country: ?[]const u8 = null,
    state: ?[]const u8 = null,
    locality: ?[]const u8 = null,
};

/// Basic constraints extension.
pub const BasicConstraints = struct {
    is_ca: bool,
    path_len_constraint: ?u32,
};

/// Parsed certificate information (subset of X.509 fields).
pub const CertificateInfo = struct {
    version: u8,
    serial_number: []const u8,
    issuer: DistinguishedName,
    subject: DistinguishedName,
    validity: ValidityPeriod,
    basic_constraints: ?BasicConstraints,
    is_self_signed: bool,
};

/// Check if data appears to be PEM encoded.
pub fn isPemEncoded(data: []const u8) bool {
    return std.mem.startsWith(u8, data, "-----BEGIN ");
}

/// Check if data appears to be DER encoded.
pub fn isDerEncoded(data: []const u8) bool {
    // DER-encoded certificates start with SEQUENCE tag (0x30)
    return data.len >= 2 and data[0] == 0x30;
}

/// Validate PEM format structure (does not validate certificate contents).
pub fn validatePemFormat(pem: []const u8) CertError!void {
    const begin_marker = "-----BEGIN CERTIFICATE-----";
    const end_marker = "-----END CERTIFICATE-----";

    const begin_pos = std.mem.indexOf(u8, pem, begin_marker) orelse return error.InvalidPem;
    const end_pos = std.mem.indexOf(u8, pem, end_marker) orelse return error.InvalidPem;

    if (begin_pos >= end_pos) return error.InvalidPem;

    // Check that there's content between markers
    const content_start = begin_pos + begin_marker.len;
    if (content_start >= end_pos) return error.InvalidPem;
}

/// Extract the base64 content from a PEM-encoded certificate.
pub fn extractPemContent(allocator: Allocator, pem: []const u8) CertError![]u8 {
    const begin_marker = "-----BEGIN CERTIFICATE-----";
    const end_marker = "-----END CERTIFICATE-----";

    const begin_pos = std.mem.indexOf(u8, pem, begin_marker) orelse return error.InvalidPem;
    const end_pos = std.mem.indexOf(u8, pem, end_marker) orelse return error.InvalidPem;

    const content_start = begin_pos + begin_marker.len;
    if (content_start >= end_pos) return error.InvalidPem;

    const content = pem[content_start..end_pos];

    // Strip whitespace and newlines
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (content) |c| {
        if (c != '\n' and c != '\r' and c != ' ' and c != '\t') {
            result.append(c) catch return error.OutOfMemory;
        }
    }

    return result.toOwnedSlice() catch error.OutOfMemory;
}

/// Validate certificate validity period against current time.
pub fn validateValidityPeriod(validity: ValidityPeriod, current_time: i64) CertError!void {
    if (validity.isNotYetValid(current_time)) {
        return error.NotYetValid;
    }
    if (validity.isExpired(current_time)) {
        return error.ExpiredCertificate;
    }
}

/// Check if a certificate chain depth is acceptable.
pub fn validateChainDepth(depth: usize, max_depth: usize) CertError!void {
    if (depth > max_depth) {
        return error.ChainValidationFailed;
    }
}

/// Validate basic constraints for CA certificates.
pub fn validateBasicConstraints(constraints: ?BasicConstraints, is_ca_expected: bool) CertError!void {
    if (is_ca_expected) {
        const bc = constraints orelse return error.MissingRequiredField;
        if (!bc.is_ca) {
            return error.ChainValidationFailed;
        }
    }
}

/// Safely compare two distinguished names for equality.
pub fn dnEquals(a: DistinguishedName, b: DistinguishedName) bool {
    return optionalStrEquals(a.common_name, b.common_name) and
        optionalStrEquals(a.organization, b.organization) and
        optionalStrEquals(a.organizational_unit, b.organizational_unit) and
        optionalStrEquals(a.country, b.country) and
        optionalStrEquals(a.state, b.state) and
        optionalStrEquals(a.locality, b.locality);
}

/// Helper to compare optional strings.
fn optionalStrEquals(a: ?[]const u8, b: ?[]const u8) bool {
    if (a == null and b == null) return true;
    if (a == null or b == null) return false;
    return std.mem.eql(u8, a.?, b.?);
}

/// Check if a certificate appears to be self-signed (issuer == subject).
pub fn isSelfSigned(issuer: DistinguishedName, subject: DistinguishedName) bool {
    return dnEquals(issuer, subject);
}

/// Validate that a certificate is not self-signed (for chain validation).
pub fn validateNotSelfSigned(issuer: DistinguishedName, subject: DistinguishedName) CertError!void {
    if (isSelfSigned(issuer, subject)) {
        return error.SelfSignedNotAllowed;
    }
}

/// Count the number of certificates in a PEM bundle.
pub fn countCertificatesInBundle(pem_bundle: []const u8) usize {
    const marker = "-----BEGIN CERTIFICATE-----";
    var count: usize = 0;
    var pos: usize = 0;

    while (std.mem.indexOfPos(u8, pem_bundle, pos, marker)) |idx| {
        count += 1;
        pos = idx + marker.len;
    }

    return count;
}

/// Validate certificate version (X.509 v1, v2, or v3).
pub fn validateVersion(version: u8) CertError!void {
    if (version < 1 or version > 3) {
        return error.UnsupportedVersion;
    }
}

test "isPemEncoded" {
    try std.testing.expect(isPemEncoded("-----BEGIN CERTIFICATE-----\nMIIB...\n-----END CERTIFICATE-----"));
    try std.testing.expect(!isPemEncoded("MIIB..."));
}

test "isDerEncoded" {
    try std.testing.expect(isDerEncoded(&[_]u8{ 0x30, 0x82, 0x01, 0x22 }));
    try std.testing.expect(!isDerEncoded("-----BEGIN"));
}

test "validatePemFormat" {
    try validatePemFormat("-----BEGIN CERTIFICATE-----\nMIIB...\n-----END CERTIFICATE-----");
    try std.testing.expectError(error.InvalidPem, validatePemFormat("not a certificate"));
    try std.testing.expectError(error.InvalidPem, validatePemFormat("-----BEGIN CERTIFICATE----------END CERTIFICATE-----"));
}

test "ValidityPeriod" {
    const validity = ValidityPeriod{
        .not_before = 1000,
        .not_after = 2000,
    };

    try std.testing.expect(validity.isValid(1500));
    try std.testing.expect(!validity.isValid(500));
    try std.testing.expect(!validity.isValid(2500));
    try std.testing.expect(validity.isExpired(2500));
    try std.testing.expect(validity.isNotYetValid(500));
    try std.testing.expectEqual(@as(i64, 500), validity.remainingSeconds(1500));
}

test "validateValidityPeriod" {
    const validity = ValidityPeriod{
        .not_before = 1000,
        .not_after = 2000,
    };

    try validateValidityPeriod(validity, 1500);
    try std.testing.expectError(error.NotYetValid, validateValidityPeriod(validity, 500));
    try std.testing.expectError(error.ExpiredCertificate, validateValidityPeriod(validity, 2500));
}

test "dnEquals" {
    const dn1 = DistinguishedName{
        .common_name = "example.com",
        .organization = "Example Inc",
    };
    const dn2 = DistinguishedName{
        .common_name = "example.com",
        .organization = "Example Inc",
    };
    const dn3 = DistinguishedName{
        .common_name = "other.com",
        .organization = "Example Inc",
    };

    try std.testing.expect(dnEquals(dn1, dn2));
    try std.testing.expect(!dnEquals(dn1, dn3));
}

test "isSelfSigned" {
    const dn = DistinguishedName{
        .common_name = "Root CA",
        .organization = "CA Org",
    };

    try std.testing.expect(isSelfSigned(dn, dn));
}

test "countCertificatesInBundle" {
    const bundle =
        \\-----BEGIN CERTIFICATE-----
        \\MIIB...
        \\-----END CERTIFICATE-----
        \\-----BEGIN CERTIFICATE-----
        \\MIIC...
        \\-----END CERTIFICATE-----
    ;
    try std.testing.expectEqual(@as(usize, 2), countCertificatesInBundle(bundle));
}

test "validateVersion" {
    try validateVersion(1);
    try validateVersion(2);
    try validateVersion(3);
    try std.testing.expectError(error.UnsupportedVersion, validateVersion(0));
    try std.testing.expectError(error.UnsupportedVersion, validateVersion(4));
}

test "extractPemContent" {
    const allocator = std.testing.allocator;
    const pem =
        \\-----BEGIN CERTIFICATE-----
        \\MIIB
        \\AAAA
        \\-----END CERTIFICATE-----
    ;
    const content = try extractPemContent(allocator, pem);
    defer allocator.free(content);
    try std.testing.expectEqualStrings("MIIBAAAA", content);
}
