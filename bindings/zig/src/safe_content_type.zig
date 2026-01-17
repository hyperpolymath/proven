// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe MIME content type handling that cannot crash.

const std = @import("std");

/// Error types for content type operations.
pub const ContentTypeError = error{
    InvalidFormat,
    InvalidMediaType,
    InvalidParameter,
    OutOfMemory,
};

/// A parsed Content-Type header
pub const ContentType = struct {
    type_: []const u8,        // e.g., "text"
    subtype: []const u8,      // e.g., "html"
    charset: ?[]const u8 = null,
    boundary: ?[]const u8 = null,
    parameters: ?std.StringHashMap([]const u8) = null,

    /// Get full media type (type/subtype)
    pub fn mediaType(self: ContentType, buffer: []u8) []u8 {
        const len = self.type_.len + 1 + self.subtype.len;
        if (buffer.len < len) return "";

        @memcpy(buffer[0..self.type_.len], self.type_);
        buffer[self.type_.len] = '/';
        @memcpy(buffer[self.type_.len + 1 ..][0..self.subtype.len], self.subtype);

        return buffer[0..len];
    }

    /// Check if this is a text type
    pub fn isText(self: ContentType) bool {
        return std.mem.eql(u8, self.type_, "text");
    }

    /// Check if this is an image type
    pub fn isImage(self: ContentType) bool {
        return std.mem.eql(u8, self.type_, "image");
    }

    /// Check if this is an audio type
    pub fn isAudio(self: ContentType) bool {
        return std.mem.eql(u8, self.type_, "audio");
    }

    /// Check if this is a video type
    pub fn isVideo(self: ContentType) bool {
        return std.mem.eql(u8, self.type_, "video");
    }

    /// Check if this is an application type
    pub fn isApplication(self: ContentType) bool {
        return std.mem.eql(u8, self.type_, "application");
    }

    /// Check if this is JSON
    pub fn isJson(self: ContentType) bool {
        return std.mem.eql(u8, self.type_, "application") and
            (std.mem.eql(u8, self.subtype, "json") or
            std.mem.endsWith(u8, self.subtype, "+json"));
    }

    /// Check if this is XML
    pub fn isXml(self: ContentType) bool {
        return (std.mem.eql(u8, self.type_, "application") or std.mem.eql(u8, self.type_, "text")) and
            (std.mem.eql(u8, self.subtype, "xml") or
            std.mem.endsWith(u8, self.subtype, "+xml"));
    }

    /// Check if this is HTML
    pub fn isHtml(self: ContentType) bool {
        return std.mem.eql(u8, self.type_, "text") and std.mem.eql(u8, self.subtype, "html");
    }

    /// Check if this is multipart
    pub fn isMultipart(self: ContentType) bool {
        return std.mem.eql(u8, self.type_, "multipart");
    }

    /// Format as Content-Type header value
    pub fn format(self: ContentType, allocator: std.mem.Allocator) ![]u8 {
        var list = std.ArrayList(u8).init(allocator);
        errdefer list.deinit();

        try list.appendSlice(self.type_);
        try list.append('/');
        try list.appendSlice(self.subtype);

        if (self.charset) |cs| {
            try list.appendSlice("; charset=");
            try list.appendSlice(cs);
        }

        if (self.boundary) |b| {
            try list.appendSlice("; boundary=");
            try list.appendSlice(b);
        }

        return list.toOwnedSlice();
    }
};

/// Common MIME types
pub const MimeTypes = struct {
    // Text
    pub const TEXT_PLAIN = "text/plain";
    pub const TEXT_HTML = "text/html";
    pub const TEXT_CSS = "text/css";
    pub const TEXT_JAVASCRIPT = "text/javascript";
    pub const TEXT_CSV = "text/csv";

    // Application
    pub const APPLICATION_JSON = "application/json";
    pub const APPLICATION_XML = "application/xml";
    pub const APPLICATION_PDF = "application/pdf";
    pub const APPLICATION_ZIP = "application/zip";
    pub const APPLICATION_GZIP = "application/gzip";
    pub const APPLICATION_OCTET_STREAM = "application/octet-stream";
    pub const APPLICATION_FORM_URLENCODED = "application/x-www-form-urlencoded";

    // Multipart
    pub const MULTIPART_FORM_DATA = "multipart/form-data";
    pub const MULTIPART_MIXED = "multipart/mixed";

    // Image
    pub const IMAGE_PNG = "image/png";
    pub const IMAGE_JPEG = "image/jpeg";
    pub const IMAGE_GIF = "image/gif";
    pub const IMAGE_WEBP = "image/webp";
    pub const IMAGE_SVG = "image/svg+xml";

    // Audio
    pub const AUDIO_MP3 = "audio/mpeg";
    pub const AUDIO_WAV = "audio/wav";
    pub const AUDIO_OGG = "audio/ogg";

    // Video
    pub const VIDEO_MP4 = "video/mp4";
    pub const VIDEO_WEBM = "video/webm";
};

/// Parse a Content-Type header
pub fn parse(header: []const u8) ContentTypeError!ContentType {
    var it = std.mem.splitScalar(u8, header, ';');

    // First part is the media type
    const media_type = std.mem.trim(u8, it.next() orelse return error.InvalidFormat, " ");
    const slash_pos = std.mem.indexOfScalar(u8, media_type, '/') orelse return error.InvalidMediaType;

    const type_ = media_type[0..slash_pos];
    const subtype = media_type[slash_pos + 1 ..];

    if (type_.len == 0 or subtype.len == 0) return error.InvalidMediaType;

    var ct = ContentType{ .type_ = type_, .subtype = subtype };

    // Parse parameters
    while (it.next()) |param| {
        const trimmed = std.mem.trim(u8, param, " ");
        if (trimmed.len == 0) continue;

        if (std.mem.indexOfScalar(u8, trimmed, '=')) |eq_pos| {
            const name = std.mem.trim(u8, trimmed[0..eq_pos], " ");
            var value = std.mem.trim(u8, trimmed[eq_pos + 1 ..], " ");

            // Remove quotes if present
            if (value.len >= 2 and value[0] == '"' and value[value.len - 1] == '"') {
                value = value[1 .. value.len - 1];
            }

            if (std.ascii.eqlIgnoreCase(name, "charset")) {
                ct.charset = value;
            } else if (std.ascii.eqlIgnoreCase(name, "boundary")) {
                ct.boundary = value;
            }
        }
    }

    return ct;
}

/// Get MIME type for a file extension
pub fn fromExtension(ext: []const u8) []const u8 {
    const extensions = std.ComptimeStringMap([]const u8, .{
        // Text
        .{ "txt", MimeTypes.TEXT_PLAIN },
        .{ "html", MimeTypes.TEXT_HTML },
        .{ "htm", MimeTypes.TEXT_HTML },
        .{ "css", MimeTypes.TEXT_CSS },
        .{ "js", MimeTypes.TEXT_JAVASCRIPT },
        .{ "mjs", MimeTypes.TEXT_JAVASCRIPT },
        .{ "csv", MimeTypes.TEXT_CSV },
        // Application
        .{ "json", MimeTypes.APPLICATION_JSON },
        .{ "xml", MimeTypes.APPLICATION_XML },
        .{ "pdf", MimeTypes.APPLICATION_PDF },
        .{ "zip", MimeTypes.APPLICATION_ZIP },
        .{ "gz", MimeTypes.APPLICATION_GZIP },
        // Image
        .{ "png", MimeTypes.IMAGE_PNG },
        .{ "jpg", MimeTypes.IMAGE_JPEG },
        .{ "jpeg", MimeTypes.IMAGE_JPEG },
        .{ "gif", MimeTypes.IMAGE_GIF },
        .{ "webp", MimeTypes.IMAGE_WEBP },
        .{ "svg", MimeTypes.IMAGE_SVG },
        // Audio
        .{ "mp3", MimeTypes.AUDIO_MP3 },
        .{ "wav", MimeTypes.AUDIO_WAV },
        .{ "ogg", MimeTypes.AUDIO_OGG },
        // Video
        .{ "mp4", MimeTypes.VIDEO_MP4 },
        .{ "webm", MimeTypes.VIDEO_WEBM },
    });

    // Remove leading dot if present
    const clean_ext = if (ext.len > 0 and ext[0] == '.') ext[1..] else ext;

    return extensions.get(clean_ext) orelse MimeTypes.APPLICATION_OCTET_STREAM;
}

/// Get file extension for a MIME type
pub fn toExtension(mime: []const u8) ?[]const u8 {
    const mimes = std.ComptimeStringMap([]const u8, .{
        .{ MimeTypes.TEXT_PLAIN, "txt" },
        .{ MimeTypes.TEXT_HTML, "html" },
        .{ MimeTypes.TEXT_CSS, "css" },
        .{ MimeTypes.TEXT_JAVASCRIPT, "js" },
        .{ MimeTypes.TEXT_CSV, "csv" },
        .{ MimeTypes.APPLICATION_JSON, "json" },
        .{ MimeTypes.APPLICATION_XML, "xml" },
        .{ MimeTypes.APPLICATION_PDF, "pdf" },
        .{ MimeTypes.APPLICATION_ZIP, "zip" },
        .{ MimeTypes.IMAGE_PNG, "png" },
        .{ MimeTypes.IMAGE_JPEG, "jpg" },
        .{ MimeTypes.IMAGE_GIF, "gif" },
        .{ MimeTypes.IMAGE_WEBP, "webp" },
        .{ MimeTypes.IMAGE_SVG, "svg" },
        .{ MimeTypes.AUDIO_MP3, "mp3" },
        .{ MimeTypes.AUDIO_WAV, "wav" },
        .{ MimeTypes.VIDEO_MP4, "mp4" },
        .{ MimeTypes.VIDEO_WEBM, "webm" },
    });

    return mimes.get(mime);
}

/// Check if MIME type is compressible
pub fn isCompressible(mime: []const u8) bool {
    const ct = parse(mime) catch return false;

    // Text types are generally compressible
    if (ct.isText()) return true;

    // JSON and XML are compressible
    if (ct.isJson() or ct.isXml()) return true;

    // JavaScript is compressible
    if (std.mem.eql(u8, ct.type_, "application") and std.mem.eql(u8, ct.subtype, "javascript")) return true;

    return false;
}

/// Check if MIME type is binary
pub fn isBinary(mime: []const u8) bool {
    const ct = parse(mime) catch return true;

    // Text types are not binary
    if (ct.isText()) return false;

    // JSON and XML are not binary
    if (ct.isJson() or ct.isXml()) return false;

    // Most other types are binary
    return true;
}

test "parse" {
    const ct = try parse("text/html; charset=utf-8");
    try std.testing.expectEqualStrings("text", ct.type_);
    try std.testing.expectEqualStrings("html", ct.subtype);
    try std.testing.expectEqualStrings("utf-8", ct.charset.?);
}

test "parse multipart" {
    const ct = try parse("multipart/form-data; boundary=----WebKitFormBoundary");
    try std.testing.expectEqualStrings("multipart", ct.type_);
    try std.testing.expectEqualStrings("form-data", ct.subtype);
    try std.testing.expectEqualStrings("----WebKitFormBoundary", ct.boundary.?);
}

test "fromExtension" {
    try std.testing.expectEqualStrings("text/html", fromExtension("html"));
    try std.testing.expectEqualStrings("application/json", fromExtension(".json"));
    try std.testing.expectEqualStrings("image/png", fromExtension("png"));
}

test "ContentType.isJson" {
    const json = try parse("application/json");
    try std.testing.expect(json.isJson());

    const api_json = try parse("application/vnd.api+json");
    try std.testing.expect(api_json.isJson());

    const html = try parse("text/html");
    try std.testing.expect(!html.isJson());
}
