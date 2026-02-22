// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeContentType - FFI bindings to libproven MIME type operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for content type operations.
pub const ContentTypeError = error{
    ParseFailure,
    FormatError,
    ProvenError,
};

/// Media type category.
pub const MediaCategory = enum(c_int) {
    text = c.PROVEN_MEDIA_TEXT,
    image = c.PROVEN_MEDIA_IMAGE,
    audio = c.PROVEN_MEDIA_AUDIO,
    video = c.PROVEN_MEDIA_VIDEO,
    application = c.PROVEN_MEDIA_APPLICATION,
    multipart = c.PROVEN_MEDIA_MULTIPART,
    message = c.PROVEN_MEDIA_MESSAGE,
    font = c.PROVEN_MEDIA_FONT,
    model = c.PROVEN_MEDIA_MODEL,
    custom = c.PROVEN_MEDIA_CUSTOM,
};

/// Charset encoding.
pub const Charset = enum(c_int) {
    utf8 = c.PROVEN_CHARSET_UTF8,
    utf16le = c.PROVEN_CHARSET_UTF16LE,
    utf16be = c.PROVEN_CHARSET_UTF16BE,
    iso8859_1 = c.PROVEN_CHARSET_ISO8859_1,
    ascii = c.PROVEN_CHARSET_ASCII,
    windows1252 = c.PROVEN_CHARSET_WINDOWS1252,
    other = c.PROVEN_CHARSET_OTHER,
};

/// Parsed content type.
pub const ContentType = struct {
    raw: c.ProvenContentTypeResult,

    pub fn mediaType(self: ContentType) []const u8 {
        if (self.raw.media_type == null) return "";
        return @as([*]const u8, @ptrCast(self.raw.media_type))[0..self.raw.media_type_len];
    }

    pub fn subtype(self: ContentType) []const u8 {
        if (self.raw.subtype == null) return "";
        return @as([*]const u8, @ptrCast(self.raw.subtype))[0..self.raw.subtype_len];
    }

    pub fn category(self: ContentType) MediaCategory {
        return @enumFromInt(self.raw.category);
    }

    pub fn charset(self: ContentType) ?Charset {
        if (!self.raw.has_charset) return null;
        return @enumFromInt(self.raw.charset);
    }

    pub fn deinit(self: *ContentType) void {
        c.proven_content_type_free(&self.raw);
    }
};

/// Managed string from libproven.
pub const ProvenString = struct {
    ptr: [*]u8,
    len: usize,

    pub fn slice(self: ProvenString) []const u8 {
        return self.ptr[0..self.len];
    }

    pub fn deinit(self: ProvenString) void {
        c.proven_free_string(self.ptr);
    }
};

/// Parse Content-Type header via libproven.
/// Caller must call deinit() on the result.
pub fn parse(input: []const u8) ContentTypeError!ContentType {
    const result = c.proven_content_type_parse(input.ptr, input.len);
    if (result.status != c.PROVEN_OK) return error.ParseFailure;
    return ContentType{ .raw = result };
}

/// Check if content type can be sniffed to something dangerous via libproven.
pub fn canSniffDangerous(input: []const u8) bool {
    const result = c.proven_content_type_can_sniff_dangerous(input.ptr, input.len);
    return result.status == c.PROVEN_OK and result.value;
}

/// Check if content type is JSON via libproven.
pub fn isJson(subtype_str: []const u8, suffix: []const u8) bool {
    const result = c.proven_content_type_is_json(
        subtype_str.ptr,
        subtype_str.len,
        suffix.ptr,
        suffix.len,
    );
    return result.status == c.PROVEN_OK and result.value;
}

/// Check if content type is XML via libproven.
pub fn isXml(subtype_str: []const u8, suffix: []const u8) bool {
    const result = c.proven_content_type_is_xml(
        subtype_str.ptr,
        subtype_str.len,
        suffix.ptr,
        suffix.len,
    );
    return result.status == c.PROVEN_OK and result.value;
}

test "parse" {
    var ct = try parse("application/json; charset=utf-8");
    defer ct.deinit();
    try std.testing.expectEqualStrings("application", ct.mediaType());
    try std.testing.expectEqualStrings("json", ct.subtype());
}

test "isJson" {
    try std.testing.expect(isJson("json", ""));
}
