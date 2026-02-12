// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe DNS record parsing and validation that cannot crash.
//! Provides validation for domain names, DNS record types, and record data.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for DNS operations.
pub const DnsError = error{
    InvalidDomainName,
    InvalidLabel,
    LabelTooLong,
    DomainTooLong,
    InvalidRecordType,
    InvalidRecordData,
    InvalidTtl,
    InvalidIpAddress,
    InvalidMxPriority,
    OutOfMemory,
};

/// Maximum length of a domain name (253 characters per RFC 1035).
pub const MAX_DOMAIN_LENGTH: usize = 253;

/// Maximum length of a single label (63 characters per RFC 1035).
pub const MAX_LABEL_LENGTH: usize = 63;

/// DNS record types.
pub const RecordType = enum(u16) {
    A = 1,
    NS = 2,
    CNAME = 5,
    SOA = 6,
    PTR = 12,
    MX = 15,
    TXT = 16,
    AAAA = 28,
    SRV = 33,
    NAPTR = 35,
    CAA = 257,

    /// Get record type from string.
    pub fn fromString(str: []const u8) ?RecordType {
        const types = std.StaticStringMap(RecordType).initComptime(.{
            .{ "A", .A },
            .{ "NS", .NS },
            .{ "CNAME", .CNAME },
            .{ "SOA", .SOA },
            .{ "PTR", .PTR },
            .{ "MX", .MX },
            .{ "TXT", .TXT },
            .{ "AAAA", .AAAA },
            .{ "SRV", .SRV },
            .{ "NAPTR", .NAPTR },
            .{ "CAA", .CAA },
        });
        return types.get(str);
    }

    /// Get string representation of record type.
    pub fn toString(self: RecordType) []const u8 {
        return switch (self) {
            .A => "A",
            .NS => "NS",
            .CNAME => "CNAME",
            .SOA => "SOA",
            .PTR => "PTR",
            .MX => "MX",
            .TXT => "TXT",
            .AAAA => "AAAA",
            .SRV => "SRV",
            .NAPTR => "NAPTR",
            .CAA => "CAA",
        };
    }

    /// Get the numeric value.
    pub fn value(self: RecordType) u16 {
        return @intFromEnum(self);
    }
};

/// DNS record class.
pub const RecordClass = enum(u16) {
    IN = 1, // Internet
    CS = 2, // CSNET (obsolete)
    CH = 3, // CHAOS
    HS = 4, // Hesiod

    pub fn fromString(str: []const u8) ?RecordClass {
        const classes = std.StaticStringMap(RecordClass).initComptime(.{
            .{ "IN", .IN },
            .{ "CS", .CS },
            .{ "CH", .CH },
            .{ "HS", .HS },
        });
        return classes.get(str);
    }

    pub fn toString(self: RecordClass) []const u8 {
        return switch (self) {
            .IN => "IN",
            .CS => "CS",
            .CH => "CH",
            .HS => "HS",
        };
    }
};

/// Parsed domain name with validation.
pub const DomainName = struct {
    labels: []const []const u8,
    raw: []const u8,

    /// Check if this is a fully qualified domain name (ends with dot).
    pub fn isFqdn(self: DomainName) bool {
        return self.raw.len > 0 and self.raw[self.raw.len - 1] == '.';
    }

    /// Get the number of labels.
    pub fn labelCount(self: DomainName) usize {
        return self.labels.len;
    }

    /// Check if this is a subdomain of another domain.
    pub fn isSubdomainOf(self: DomainName, parent: DomainName) bool {
        if (self.labels.len <= parent.labels.len) return false;

        const offset = self.labels.len - parent.labels.len;
        for (parent.labels, 0..) |parent_label, i| {
            if (!std.ascii.eqlIgnoreCase(self.labels[offset + i], parent_label)) {
                return false;
            }
        }
        return true;
    }

    /// Get the top-level domain.
    pub fn tld(self: DomainName) ?[]const u8 {
        if (self.labels.len == 0) return null;
        return self.labels[self.labels.len - 1];
    }

    /// Get the second-level domain (e.g., "example" from "www.example.com").
    pub fn sld(self: DomainName) ?[]const u8 {
        if (self.labels.len < 2) return null;
        return self.labels[self.labels.len - 2];
    }
};

/// MX record data.
pub const MxRecord = struct {
    priority: u16,
    exchange: []const u8,
};

/// SRV record data.
pub const SrvRecord = struct {
    priority: u16,
    weight: u16,
    port: u16,
    target: []const u8,
};

/// SOA record data.
pub const SoaRecord = struct {
    mname: []const u8, // Primary nameserver
    rname: []const u8, // Responsible person email
    serial: u32,
    refresh: u32,
    retry: u32,
    expire: u32,
    minimum: u32,
};

/// CAA record data.
pub const CaaRecord = struct {
    flags: u8,
    tag: []const u8,
    value: []const u8,
};

/// Generic DNS record.
pub const DnsRecord = struct {
    name: []const u8,
    record_type: RecordType,
    class: RecordClass,
    ttl: u32,
    data: []const u8,
};

/// Validate a domain name label.
pub fn isValidLabel(label: []const u8) bool {
    if (label.len == 0 or label.len > MAX_LABEL_LENGTH) return false;

    // Must start with alphanumeric
    if (!std.ascii.isAlphanumeric(label[0])) return false;

    // Must end with alphanumeric
    if (!std.ascii.isAlphanumeric(label[label.len - 1])) return false;

    // Check all characters
    for (label) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '-') {
            return false;
        }
    }

    // Cannot have consecutive hyphens at positions 2 and 3 (IDN restriction)
    if (label.len >= 4 and label[2] == '-' and label[3] == '-') {
        // Exception for punycode (xn--)
        if (label.len >= 4 and std.mem.startsWith(u8, label, "xn--")) {
            return true;
        }
        return false;
    }

    return true;
}

/// Validate a full domain name.
pub fn isValidDomainName(domain: []const u8) bool {
    if (domain.len == 0 or domain.len > MAX_DOMAIN_LENGTH) return false;

    // Remove trailing dot for validation
    const name = if (domain[domain.len - 1] == '.') domain[0 .. domain.len - 1] else domain;

    if (name.len == 0) return false;

    var it = std.mem.splitScalar(u8, name, '.');
    var label_count: usize = 0;

    while (it.next()) |label| {
        if (!isValidLabel(label)) return false;
        label_count += 1;
    }

    return label_count >= 1;
}

/// Parse a domain name into labels.
pub fn parseDomainName(allocator: Allocator, domain: []const u8) DnsError!DomainName {
    if (!isValidDomainName(domain)) return error.InvalidDomainName;

    const name = if (domain.len > 0 and domain[domain.len - 1] == '.')
        domain[0 .. domain.len - 1]
    else
        domain;

    var labels = std.array_list.Managed([]const u8).init(allocator);
    errdefer labels.deinit();

    var it = std.mem.splitScalar(u8, name, '.');
    while (it.next()) |label| {
        labels.append(label) catch return error.OutOfMemory;
    }

    return DomainName{
        .labels = labels.toOwnedSlice() catch return error.OutOfMemory,
        .raw = domain,
    };
}

/// Validate an IPv4 address for A record.
pub fn isValidIPv4(address: []const u8) bool {
    var parts: usize = 0;
    var current_value: u16 = 0;
    var has_digit = false;

    for (address) |c| {
        if (c == '.') {
            if (!has_digit or parts >= 3) return false;
            if (current_value > 255) return false;
            parts += 1;
            current_value = 0;
            has_digit = false;
        } else if (c >= '0' and c <= '9') {
            current_value = current_value * 10 + (c - '0');
            has_digit = true;
            if (current_value > 255) return false;
        } else {
            return false;
        }
    }

    return has_digit and parts == 3 and current_value <= 255;
}

/// Validate an IPv6 address for AAAA record.
pub fn isValidIPv6(address: []const u8) bool {
    if (address.len == 0 or address.len > 45) return false;

    var groups: usize = 0;
    var double_colon = false;
    var i: usize = 0;

    while (i < address.len) {
        // Check for double colon
        if (i + 1 < address.len and address[i] == ':' and address[i + 1] == ':') {
            if (double_colon) return false; // Only one :: allowed
            double_colon = true;
            i += 2;
            if (i >= address.len) break;
            continue;
        }

        // Skip single colon between groups
        if (address[i] == ':') {
            i += 1;
            continue;
        }

        // Parse hex group
        var group_len: usize = 0;
        while (i < address.len and address[i] != ':') {
            const c = address[i];
            if (!std.ascii.isHex(c)) return false;
            group_len += 1;
            if (group_len > 4) return false;
            i += 1;
        }

        if (group_len == 0) return false;
        groups += 1;
    }

    // Must have exactly 8 groups, or fewer with ::
    if (double_colon) {
        return groups <= 7;
    } else {
        return groups == 8;
    }
}

/// Parse an MX record value (priority + exchange).
pub fn parseMxRecord(data: []const u8) DnsError!MxRecord {
    // Format: "priority exchange"
    const space_pos = std.mem.indexOfScalar(u8, data, ' ') orelse return error.InvalidRecordData;

    const priority_str = data[0..space_pos];
    const exchange = std.mem.trim(u8, data[space_pos + 1 ..], " ");

    const priority = std.fmt.parseInt(u16, priority_str, 10) catch return error.InvalidMxPriority;

    if (!isValidDomainName(exchange)) return error.InvalidRecordData;

    return MxRecord{
        .priority = priority,
        .exchange = exchange,
    };
}

/// Parse an SRV record value.
pub fn parseSrvRecord(data: []const u8) DnsError!SrvRecord {
    // Format: "priority weight port target"
    var it = std.mem.splitScalar(u8, data, ' ');

    const priority_str = it.next() orelse return error.InvalidRecordData;
    const weight_str = it.next() orelse return error.InvalidRecordData;
    const port_str = it.next() orelse return error.InvalidRecordData;
    const target = it.rest();

    const priority = std.fmt.parseInt(u16, priority_str, 10) catch return error.InvalidRecordData;
    const weight = std.fmt.parseInt(u16, weight_str, 10) catch return error.InvalidRecordData;
    const port = std.fmt.parseInt(u16, port_str, 10) catch return error.InvalidRecordData;

    if (target.len > 0 and !isValidDomainName(target)) {
        return error.InvalidRecordData;
    }

    return SrvRecord{
        .priority = priority,
        .weight = weight,
        .port = port,
        .target = target,
    };
}

/// Validate TXT record data.
pub fn isValidTxtRecord(data: []const u8) bool {
    // TXT records can contain any printable ASCII or quoted strings
    // Maximum length per string is 255 characters
    if (data.len > 65535) return false;

    var in_quote = false;
    var segment_len: usize = 0;

    for (data) |c| {
        if (c == '"') {
            in_quote = !in_quote;
            segment_len = 0;
        } else if (in_quote) {
            segment_len += 1;
            if (segment_len > 255) return false;
        } else {
            // Outside quotes, only allow spaces between segments
            if (c != ' ') return false;
        }
    }

    return !in_quote; // Must have balanced quotes
}

/// Validate a TTL value.
pub fn isValidTtl(ttl: u32) bool {
    // TTL must be between 0 and 2147483647 (max signed 32-bit)
    return ttl <= 2147483647;
}

/// Parse a TTL string (supports suffixes like 1h, 1d, 1w).
pub fn parseTtl(str: []const u8) DnsError!u32 {
    if (str.len == 0) return error.InvalidTtl;

    var value: u64 = 0;
    var i: usize = 0;

    // Parse numeric part
    while (i < str.len and str[i] >= '0' and str[i] <= '9') : (i += 1) {
        value = value * 10 + (str[i] - '0');
        if (value > 2147483647) return error.InvalidTtl;
    }

    if (i == 0) return error.InvalidTtl;

    // Parse optional suffix
    if (i < str.len) {
        const multiplier: u64 = switch (str[i]) {
            's', 'S' => 1, // seconds
            'm', 'M' => 60, // minutes
            'h', 'H' => 3600, // hours
            'd', 'D' => 86400, // days
            'w', 'W' => 604800, // weeks
            else => return error.InvalidTtl,
        };
        value *= multiplier;
        if (value > 2147483647) return error.InvalidTtl;
    }

    return @intCast(value);
}

/// Normalize a domain name (lowercase, remove trailing dot).
pub fn normalizeDomainName(allocator: Allocator, domain: []const u8) DnsError![]u8 {
    if (!isValidDomainName(domain)) return error.InvalidDomainName;

    const len = if (domain.len > 0 and domain[domain.len - 1] == '.')
        domain.len - 1
    else
        domain.len;

    var result = allocator.alloc(u8, len) catch return error.OutOfMemory;
    for (domain[0..len], 0..) |c, i| {
        result[i] = std.ascii.toLower(c);
    }

    return result;
}

test "isValidLabel" {
    try std.testing.expect(isValidLabel("example"));
    try std.testing.expect(isValidLabel("my-domain"));
    try std.testing.expect(isValidLabel("xn--nxasmq5a")); // punycode
    try std.testing.expect(!isValidLabel("")); // empty
    try std.testing.expect(!isValidLabel("-invalid")); // starts with hyphen
    try std.testing.expect(!isValidLabel("invalid-")); // ends with hyphen
}

test "isValidDomainName" {
    try std.testing.expect(isValidDomainName("example.com"));
    try std.testing.expect(isValidDomainName("sub.example.com"));
    try std.testing.expect(isValidDomainName("example.com.")); // FQDN
    try std.testing.expect(!isValidDomainName("")); // empty
    try std.testing.expect(!isValidDomainName(".example.com")); // starts with dot
    try std.testing.expect(!isValidDomainName("example..com")); // double dot
}

test "isValidIPv4" {
    try std.testing.expect(isValidIPv4("192.168.1.1"));
    try std.testing.expect(isValidIPv4("0.0.0.0"));
    try std.testing.expect(isValidIPv4("255.255.255.255"));
    try std.testing.expect(!isValidIPv4("256.1.1.1")); // octet > 255
    try std.testing.expect(!isValidIPv4("1.1.1")); // missing octet
    try std.testing.expect(!isValidIPv4("1.1.1.1.1")); // too many octets
}

test "isValidIPv6" {
    try std.testing.expect(isValidIPv6("2001:0db8:85a3:0000:0000:8a2e:0370:7334"));
    try std.testing.expect(isValidIPv6("2001:db8:85a3::8a2e:370:7334"));
    try std.testing.expect(isValidIPv6("::1")); // loopback
    try std.testing.expect(isValidIPv6("::")); // any
    try std.testing.expect(!isValidIPv6("2001::85a3::7334")); // double ::
}

test "parseMxRecord" {
    const mx = try parseMxRecord("10 mail.example.com");
    try std.testing.expectEqual(@as(u16, 10), mx.priority);
    try std.testing.expectEqualStrings("mail.example.com", mx.exchange);
}

test "parseTtl" {
    try std.testing.expectEqual(@as(u32, 3600), try parseTtl("3600"));
    try std.testing.expectEqual(@as(u32, 3600), try parseTtl("1h"));
    try std.testing.expectEqual(@as(u32, 86400), try parseTtl("1d"));
    try std.testing.expectEqual(@as(u32, 604800), try parseTtl("1w"));
}

test "RecordType" {
    try std.testing.expect(RecordType.fromString("A") == .A);
    try std.testing.expect(RecordType.fromString("MX") == .MX);
    try std.testing.expectEqualStrings("A", RecordType.A.toString());
    try std.testing.expectEqual(@as(u16, 1), RecordType.A.value());
}
