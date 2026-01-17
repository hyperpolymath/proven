// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe archive format detection and validation operations.
//!
//! Provides safe functions to detect and validate common archive formats
//! (zip, tar, gzip, bzip2, xz, 7z, rar) based on magic bytes.
//! All operations are bounds-checked and cannot crash.

const std = @import("std");

/// Error types for archive operations.
pub const ArchiveError = error{
    UnknownFormat,
    BufferTooSmall,
    InvalidHeader,
    CorruptedArchive,
};

/// Supported archive formats.
pub const ArchiveFormat = enum {
    zip,
    tar,
    gzip,
    bzip2,
    xz,
    seven_zip,
    rar,
    zstd,

    /// Get the file extension for this format.
    pub fn extension(self: ArchiveFormat) []const u8 {
        return switch (self) {
            .zip => ".zip",
            .tar => ".tar",
            .gzip => ".gz",
            .bzip2 => ".bz2",
            .xz => ".xz",
            .seven_zip => ".7z",
            .rar => ".rar",
            .zstd => ".zst",
        };
    }

    /// Get the MIME type for this format.
    pub fn mimeType(self: ArchiveFormat) []const u8 {
        return switch (self) {
            .zip => "application/zip",
            .tar => "application/x-tar",
            .gzip => "application/gzip",
            .bzip2 => "application/x-bzip2",
            .xz => "application/x-xz",
            .seven_zip => "application/x-7z-compressed",
            .rar => "application/vnd.rar",
            .zstd => "application/zstd",
        };
    }

    /// Get the human-readable name for this format.
    pub fn displayName(self: ArchiveFormat) []const u8 {
        return switch (self) {
            .zip => "ZIP",
            .tar => "TAR",
            .gzip => "GZIP",
            .bzip2 => "BZIP2",
            .xz => "XZ",
            .seven_zip => "7-Zip",
            .rar => "RAR",
            .zstd => "Zstandard",
        };
    }
};

/// Magic byte signatures for archive detection.
const MagicSignature = struct {
    bytes: []const u8,
    offset: usize,
    format: ArchiveFormat,
};

/// Known archive magic signatures.
const magic_signatures = [_]MagicSignature{
    // ZIP: PK\x03\x04 (local file header) or PK\x05\x06 (empty archive)
    .{ .bytes = &[_]u8{ 0x50, 0x4B, 0x03, 0x04 }, .offset = 0, .format = .zip },
    .{ .bytes = &[_]u8{ 0x50, 0x4B, 0x05, 0x06 }, .offset = 0, .format = .zip },

    // GZIP: \x1f\x8b
    .{ .bytes = &[_]u8{ 0x1F, 0x8B }, .offset = 0, .format = .gzip },

    // BZIP2: BZ
    .{ .bytes = &[_]u8{ 0x42, 0x5A, 0x68 }, .offset = 0, .format = .bzip2 },

    // XZ: \xFD7zXZ\x00
    .{ .bytes = &[_]u8{ 0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00 }, .offset = 0, .format = .xz },

    // 7-Zip: 7z\xBC\xAF\x27\x1C
    .{ .bytes = &[_]u8{ 0x37, 0x7A, 0xBC, 0xAF, 0x27, 0x1C }, .offset = 0, .format = .seven_zip },

    // RAR 5.0: Rar!\x1A\x07\x01\x00
    .{ .bytes = &[_]u8{ 0x52, 0x61, 0x72, 0x21, 0x1A, 0x07, 0x01, 0x00 }, .offset = 0, .format = .rar },

    // RAR 4.x: Rar!\x1A\x07\x00
    .{ .bytes = &[_]u8{ 0x52, 0x61, 0x72, 0x21, 0x1A, 0x07, 0x00 }, .offset = 0, .format = .rar },

    // Zstandard: \x28\xB5\x2F\xFD
    .{ .bytes = &[_]u8{ 0x28, 0xB5, 0x2F, 0xFD }, .offset = 0, .format = .zstd },
};

/// TAR block size (512 bytes).
pub const TAR_BLOCK_SIZE: usize = 512;

/// Minimum bytes needed for reliable format detection.
pub const MIN_DETECTION_BYTES: usize = 512;

/// Detect archive format from magic bytes.
/// Returns the detected format or null if unknown.
pub fn detectFormat(data: []const u8) ?ArchiveFormat {
    // Check all magic signatures
    for (magic_signatures) |sig| {
        if (matchesMagic(data, sig.bytes, sig.offset)) {
            return sig.format;
        }
    }

    // TAR detection requires special handling (no magic at offset 0)
    if (isTarArchive(data)) {
        return .tar;
    }

    return null;
}

/// Check if data matches magic bytes at given offset.
fn matchesMagic(data: []const u8, magic: []const u8, offset: usize) bool {
    if (data.len < offset + magic.len) return false;

    for (magic, 0..) |byte, i| {
        if (data[offset + i] != byte) return false;
    }

    return true;
}

/// Check if data appears to be a TAR archive.
/// TAR files have "ustar" at offset 257.
fn isTarArchive(data: []const u8) bool {
    // Check for "ustar" magic at offset 257
    const ustar_offset: usize = 257;
    const ustar_magic = "ustar";

    if (data.len < ustar_offset + ustar_magic.len) return false;

    return std.mem.eql(u8, data[ustar_offset .. ustar_offset + 5], ustar_magic);
}

/// Validate ZIP archive header structure.
pub fn validateZipHeader(data: []const u8) ArchiveError!ZipHeaderInfo {
    if (data.len < 30) return error.BufferTooSmall;

    // Check magic
    if (!matchesMagic(data, &[_]u8{ 0x50, 0x4B, 0x03, 0x04 }, 0)) {
        return error.InvalidHeader;
    }

    // Parse header fields (little-endian)
    const version_needed = std.mem.readInt(u16, data[4..6], .little);
    const flags = std.mem.readInt(u16, data[6..8], .little);
    const compression_method = std.mem.readInt(u16, data[8..10], .little);
    const filename_len = std.mem.readInt(u16, data[26..28], .little);
    const extra_len = std.mem.readInt(u16, data[28..30], .little);

    // Validate header doesn't exceed data
    const header_size: usize = 30 + @as(usize, filename_len) + @as(usize, extra_len);
    if (data.len < header_size) return error.BufferTooSmall;

    return ZipHeaderInfo{
        .version_needed = version_needed,
        .flags = flags,
        .compression_method = compression_method,
        .filename_len = filename_len,
        .extra_len = extra_len,
        .header_size = header_size,
    };
}

/// ZIP local file header information.
pub const ZipHeaderInfo = struct {
    version_needed: u16,
    flags: u16,
    compression_method: u16,
    filename_len: u16,
    extra_len: u16,
    header_size: usize,

    /// Check if encrypted.
    pub fn isEncrypted(self: ZipHeaderInfo) bool {
        return (self.flags & 0x0001) != 0;
    }

    /// Get compression method name.
    pub fn compressionName(self: ZipHeaderInfo) []const u8 {
        return switch (self.compression_method) {
            0 => "stored",
            1 => "shrunk",
            6 => "imploded",
            8 => "deflated",
            9 => "deflate64",
            12 => "bzip2",
            14 => "lzma",
            93 => "zstd",
            95 => "xz",
            else => "unknown",
        };
    }
};

/// Validate GZIP header structure.
pub fn validateGzipHeader(data: []const u8) ArchiveError!GzipHeaderInfo {
    if (data.len < 10) return error.BufferTooSmall;

    // Check magic
    if (data[0] != 0x1F or data[1] != 0x8B) {
        return error.InvalidHeader;
    }

    const compression_method = data[2];
    const flags = data[3];
    const mtime = std.mem.readInt(u32, data[4..8], .little);
    const extra_flags = data[8];
    const os = data[9];

    // Validate compression method (only deflate supported)
    if (compression_method != 8) {
        return error.InvalidHeader;
    }

    var header_size: usize = 10;

    // Calculate extra header size based on flags
    if ((flags & 0x04) != 0) { // FEXTRA
        if (data.len < header_size + 2) return error.BufferTooSmall;
        const xlen = std.mem.readInt(u16, data[header_size..][0..2], .little);
        header_size += 2 + @as(usize, xlen);
    }

    if ((flags & 0x08) != 0) { // FNAME
        // Skip null-terminated filename
        while (header_size < data.len and data[header_size] != 0) {
            header_size += 1;
        }
        if (header_size >= data.len) return error.BufferTooSmall;
        header_size += 1; // Skip null terminator
    }

    if ((flags & 0x10) != 0) { // FCOMMENT
        // Skip null-terminated comment
        while (header_size < data.len and data[header_size] != 0) {
            header_size += 1;
        }
        if (header_size >= data.len) return error.BufferTooSmall;
        header_size += 1; // Skip null terminator
    }

    if ((flags & 0x02) != 0) { // FHCRC
        header_size += 2;
    }

    if (data.len < header_size) return error.BufferTooSmall;

    return GzipHeaderInfo{
        .compression_method = compression_method,
        .flags = flags,
        .mtime = mtime,
        .extra_flags = extra_flags,
        .os = os,
        .header_size = header_size,
    };
}

/// GZIP header information.
pub const GzipHeaderInfo = struct {
    compression_method: u8,
    flags: u8,
    mtime: u32,
    extra_flags: u8,
    os: u8,
    header_size: usize,

    /// Check if file has original filename.
    pub fn hasFilename(self: GzipHeaderInfo) bool {
        return (self.flags & 0x08) != 0;
    }

    /// Check if file has comment.
    pub fn hasComment(self: GzipHeaderInfo) bool {
        return (self.flags & 0x10) != 0;
    }

    /// Get OS name.
    pub fn osName(self: GzipHeaderInfo) []const u8 {
        return switch (self.os) {
            0 => "FAT filesystem",
            1 => "Amiga",
            2 => "VMS",
            3 => "Unix",
            4 => "VM/CMS",
            5 => "Atari TOS",
            6 => "HPFS filesystem",
            7 => "Macintosh",
            8 => "Z-System",
            9 => "CP/M",
            10 => "TOPS-20",
            11 => "NTFS filesystem",
            12 => "QDOS",
            13 => "Acorn RISCOS",
            255 => "unknown",
            else => "unknown",
        };
    }
};

/// Validate TAR header at given offset.
pub fn validateTarHeader(data: []const u8, offset: usize) ArchiveError!TarHeaderInfo {
    if (data.len < offset + TAR_BLOCK_SIZE) return error.BufferTooSmall;

    const header = data[offset .. offset + TAR_BLOCK_SIZE];

    // Check if this is an empty block (end of archive)
    var all_zeros = true;
    for (header) |byte| {
        if (byte != 0) {
            all_zeros = false;
            break;
        }
    }

    if (all_zeros) {
        return TarHeaderInfo{
            .name_len = 0,
            .size = 0,
            .typeflag = 0,
            .is_end = true,
            .is_ustar = false,
        };
    }

    // Parse filename (null-terminated, max 100 chars)
    var name_len: usize = 0;
    while (name_len < 100 and header[name_len] != 0) {
        name_len += 1;
    }

    // Parse file size (octal string at offset 124, 12 bytes)
    const size = parseOctal(header[124..136]) orelse return error.InvalidHeader;

    // Get type flag
    const typeflag = header[156];

    // Check for ustar magic
    const is_ustar = std.mem.eql(u8, header[257..262], "ustar");

    return TarHeaderInfo{
        .name_len = name_len,
        .size = size,
        .typeflag = typeflag,
        .is_end = false,
        .is_ustar = is_ustar,
    };
}

/// TAR header information.
pub const TarHeaderInfo = struct {
    name_len: usize,
    size: u64,
    typeflag: u8,
    is_end: bool,
    is_ustar: bool,

    /// Get the entry type name.
    pub fn typeName(self: TarHeaderInfo) []const u8 {
        return switch (self.typeflag) {
            0, '0' => "regular file",
            '1' => "hard link",
            '2' => "symbolic link",
            '3' => "character device",
            '4' => "block device",
            '5' => "directory",
            '6' => "FIFO",
            '7' => "contiguous file",
            'x' => "extended header",
            'g' => "global extended header",
            else => "unknown",
        };
    }

    /// Check if this is a regular file.
    pub fn isRegularFile(self: TarHeaderInfo) bool {
        return self.typeflag == 0 or self.typeflag == '0';
    }

    /// Check if this is a directory.
    pub fn isDirectory(self: TarHeaderInfo) bool {
        return self.typeflag == '5';
    }
};

/// Parse octal string to integer.
fn parseOctal(data: []const u8) ?u64 {
    var result: u64 = 0;

    for (data) |c| {
        if (c == 0 or c == ' ') break;
        if (c < '0' or c > '7') return null;
        result = result * 8 + (c - '0');
    }

    return result;
}

/// Check if data is a valid archive of any supported format.
pub fn isValidArchive(data: []const u8) bool {
    return detectFormat(data) != null;
}

/// Get minimum bytes needed to detect a specific format.
pub fn minBytesForFormat(format: ArchiveFormat) usize {
    return switch (format) {
        .zip => 4,
        .tar => 262, // Need to reach "ustar" magic
        .gzip => 2,
        .bzip2 => 3,
        .xz => 6,
        .seven_zip => 6,
        .rar => 8,
        .zstd => 4,
    };
}

/// Archive detection result with confidence level.
pub const DetectionResult = struct {
    format: ?ArchiveFormat,
    confidence: Confidence,
    reason: []const u8,

    pub const Confidence = enum {
        high, // Magic bytes match perfectly
        medium, // Some indicators present
        low, // Might be this format
        none, // Not detected
    };
};

/// Detect archive format with confidence level.
pub fn detectFormatWithConfidence(data: []const u8) DetectionResult {
    // Try magic byte detection first
    if (detectFormat(data)) |format| {
        return DetectionResult{
            .format = format,
            .confidence = .high,
            .reason = "Magic bytes match",
        };
    }

    // Check for partial matches or truncated data
    if (data.len < MIN_DETECTION_BYTES) {
        return DetectionResult{
            .format = null,
            .confidence = .low,
            .reason = "Insufficient data for reliable detection",
        };
    }

    return DetectionResult{
        .format = null,
        .confidence = .none,
        .reason = "No known archive format detected",
    };
}

test "detectFormat - zip" {
    const zip_data = [_]u8{ 0x50, 0x4B, 0x03, 0x04 } ++ [_]u8{0} ** 26;
    try std.testing.expectEqual(ArchiveFormat.zip, detectFormat(&zip_data).?);
}

test "detectFormat - gzip" {
    const gzip_data = [_]u8{ 0x1F, 0x8B, 0x08, 0x00 } ++ [_]u8{0} ** 6;
    try std.testing.expectEqual(ArchiveFormat.gzip, detectFormat(&gzip_data).?);
}

test "detectFormat - bzip2" {
    const bzip2_data = [_]u8{ 0x42, 0x5A, 0x68, 0x39 };
    try std.testing.expectEqual(ArchiveFormat.bzip2, detectFormat(&bzip2_data).?);
}

test "detectFormat - xz" {
    const xz_data = [_]u8{ 0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00 };
    try std.testing.expectEqual(ArchiveFormat.xz, detectFormat(&xz_data).?);
}

test "detectFormat - 7zip" {
    const sevenz_data = [_]u8{ 0x37, 0x7A, 0xBC, 0xAF, 0x27, 0x1C };
    try std.testing.expectEqual(ArchiveFormat.seven_zip, detectFormat(&sevenz_data).?);
}

test "detectFormat - rar5" {
    const rar_data = [_]u8{ 0x52, 0x61, 0x72, 0x21, 0x1A, 0x07, 0x01, 0x00 };
    try std.testing.expectEqual(ArchiveFormat.rar, detectFormat(&rar_data).?);
}

test "detectFormat - zstd" {
    const zstd_data = [_]u8{ 0x28, 0xB5, 0x2F, 0xFD };
    try std.testing.expectEqual(ArchiveFormat.zstd, detectFormat(&zstd_data).?);
}

test "detectFormat - unknown" {
    const unknown_data = [_]u8{ 0x00, 0x00, 0x00, 0x00 };
    try std.testing.expect(detectFormat(&unknown_data) == null);
}

test "validateZipHeader" {
    // Minimal valid ZIP local file header
    var zip_header: [30]u8 = undefined;
    // Magic
    zip_header[0] = 0x50;
    zip_header[1] = 0x4B;
    zip_header[2] = 0x03;
    zip_header[3] = 0x04;
    // Version needed
    zip_header[4] = 20;
    zip_header[5] = 0;
    // Flags (no encryption)
    zip_header[6] = 0;
    zip_header[7] = 0;
    // Compression (stored)
    zip_header[8] = 0;
    zip_header[9] = 0;
    // Rest can be zeros
    @memset(zip_header[10..], 0);

    const info = try validateZipHeader(&zip_header);
    try std.testing.expectEqual(@as(u16, 20), info.version_needed);
    try std.testing.expectEqual(@as(u16, 0), info.compression_method);
    try std.testing.expect(!info.isEncrypted());
    try std.testing.expectEqualStrings("stored", info.compressionName());
}

test "validateGzipHeader" {
    // Minimal valid GZIP header
    const gzip_header = [_]u8{
        0x1F, 0x8B, // Magic
        0x08, // Compression method (deflate)
        0x00, // Flags
        0x00, 0x00, 0x00, 0x00, // Modification time
        0x00, // Extra flags
        0x03, // OS (Unix)
    };

    const info = try validateGzipHeader(&gzip_header);
    try std.testing.expectEqual(@as(u8, 8), info.compression_method);
    try std.testing.expectEqual(@as(usize, 10), info.header_size);
    try std.testing.expect(!info.hasFilename());
    try std.testing.expectEqualStrings("Unix", info.osName());
}

test "ArchiveFormat methods" {
    try std.testing.expectEqualStrings(".zip", ArchiveFormat.zip.extension());
    try std.testing.expectEqualStrings("application/gzip", ArchiveFormat.gzip.mimeType());
    try std.testing.expectEqualStrings("7-Zip", ArchiveFormat.seven_zip.displayName());
}

test "detectFormatWithConfidence" {
    const zip_data = [_]u8{ 0x50, 0x4B, 0x03, 0x04 } ++ [_]u8{0} ** 26;
    const result = detectFormatWithConfidence(&zip_data);
    try std.testing.expectEqual(ArchiveFormat.zip, result.format.?);
    try std.testing.expectEqual(DetectionResult.Confidence.high, result.confidence);
}

test "parseOctal" {
    try std.testing.expectEqual(@as(u64, 0), parseOctal("0").?);
    try std.testing.expectEqual(@as(u64, 8), parseOctal("10").?);
    try std.testing.expectEqual(@as(u64, 511), parseOctal("777").?);
    try std.testing.expectEqual(@as(u64, 100), parseOctal("144").?);
    try std.testing.expect(parseOctal("9") == null); // Invalid octal digit
}
