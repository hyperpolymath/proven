// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe file operations with path validation and resource protection.
//!
//! Provides safe file I/O operations with:
//! - Path traversal attack prevention
//! - Bounded read operations to prevent memory exhaustion
//! - Atomic write operations where possible
//! - Proper resource cleanup on errors

const std = @import("std");
const Allocator = std.mem.Allocator;
const Io = std.Io;
const Dir = Io.Dir;
const File = Io.File;

/// Get the global single-threaded Io instance for blocking operations.
fn getIo() Io {
    return Io.Threaded.global_single_threaded.ioBasic();
}

/// Get the current working directory handle.
fn getCwd() Dir {
    return Dir.cwd();
}

/// Error types for file operations.
pub const FileError = error{
    TraversalDetected,
    PathTooLong,
    FileTooLarge,
    ReadError,
    WriteError,
    OpenError,
    NotFound,
    PermissionDenied,
    OutOfMemory,
    InvalidPath,
    IsDirectory,
    NotAFile,
};

/// Maximum allowed path length.
pub const MAX_PATH_LENGTH: usize = 4096;

/// Default maximum file size for bounded reads (16 MiB).
pub const DEFAULT_MAX_FILE_SIZE: usize = 16 * 1024 * 1024;

/// Check if a path contains directory traversal sequences.
pub fn hasTraversal(path: []const u8) bool {
    // Check for ".." sequence
    if (std.mem.indexOf(u8, path, "..") != null) return true;

    // Check for home directory expansion
    if (path.len > 0 and path[0] == '~') return true;

    return false;
}

/// Check if a path is safe (no traversal attacks, within length limits).
pub fn isSafePath(path: []const u8) bool {
    if (path.len == 0 or path.len > MAX_PATH_LENGTH) return false;
    if (hasTraversal(path)) return false;

    // Check for null bytes
    for (path) |char| {
        if (char == 0) return false;
    }

    return true;
}

/// Validate a path and return it if safe, otherwise return error.
pub fn validatePath(path: []const u8) FileError![]const u8 {
    if (path.len == 0) return error.InvalidPath;
    if (path.len > MAX_PATH_LENGTH) return error.PathTooLong;
    if (hasTraversal(path)) return error.TraversalDetected;

    for (path) |char| {
        if (char == 0) return error.InvalidPath;
    }

    return path;
}

/// Read entire file contents with a maximum size limit.
/// Returns the file contents as owned memory that must be freed.
pub fn readFileBounded(
    allocator: Allocator,
    path: []const u8,
    max_size: usize,
) FileError![]u8 {
    _ = validatePath(path) catch |err| return err;

    const io = getIo();
    const cwd = getCwd();

    const file = cwd.openFile(io, path, .{}) catch |err| {
        return switch (err) {
            error.FileNotFound => error.NotFound,
            error.AccessDenied => error.PermissionDenied,
            error.PermissionDenied => error.PermissionDenied,
            error.NotDir => error.IsDirectory,
            else => error.OpenError,
        };
    };
    defer file.close(io);

    const stat = file.stat(io) catch return error.ReadError;
    if (stat.size > max_size) return error.FileTooLarge;

    const contents = file.readAllAlloc(io, allocator, .limited(max_size)) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.OutOfMemory,
            else => error.ReadError,
        };
    };

    return contents;
}

/// Read entire file with default size limit.
pub fn readFile(allocator: Allocator, path: []const u8) FileError![]u8 {
    return readFileBounded(allocator, path, DEFAULT_MAX_FILE_SIZE);
}

/// Read file as lines with bounded total size.
/// Returns owned slice of lines that must be freed along with contents.
pub fn readLines(
    allocator: Allocator,
    path: []const u8,
    max_size: usize,
) FileError!struct { lines: [][]const u8, contents: []u8 } {
    const contents = try readFileBounded(allocator, path, max_size);
    errdefer allocator.free(contents);

    var lines_list = std.array_list.Managed([]const u8).init(allocator);
    errdefer lines_list.deinit();

    var iter = std.mem.splitScalar(u8, contents, '\n');
    while (iter.next()) |line| {
        lines_list.append(line) catch return error.OutOfMemory;
    }

    const lines = lines_list.toOwnedSlice() catch return error.OutOfMemory;

    return .{ .lines = lines, .contents = contents };
}

/// Write data to a file safely.
pub fn writeFile(path: []const u8, data: []const u8) FileError!void {
    _ = validatePath(path) catch |err| return err;

    const io = getIo();
    const cwd = getCwd();

    const file = cwd.createFile(io, path, .{}) catch |err| {
        return switch (err) {
            error.AccessDenied => error.PermissionDenied,
            error.PermissionDenied => error.PermissionDenied,
            error.NotDir => error.IsDirectory,
            else => error.OpenError,
        };
    };
    defer file.close(io);

    file.writeAll(io, data) catch return error.WriteError;
}

/// Append data to a file safely.
pub fn appendFile(path: []const u8, data: []const u8) FileError!void {
    _ = validatePath(path) catch |err| return err;

    const io = getIo();
    const cwd = getCwd();

    const file = cwd.openFile(io, path, .{ .write = true }) catch |err| {
        return switch (err) {
            error.FileNotFound => error.NotFound,
            error.AccessDenied => error.PermissionDenied,
            error.PermissionDenied => error.PermissionDenied,
            error.NotDir => error.IsDirectory,
            else => error.OpenError,
        };
    };
    defer file.close(io);

    file.seekFromEnd(io, 0) catch return error.WriteError;
    file.writeAll(io, data) catch return error.WriteError;
}

/// Check if a file exists and is a regular file (not a directory).
pub fn fileExists(path: []const u8) bool {
    _ = validatePath(path) catch return false;

    const io = getIo();
    const cwd = getCwd();

    const stat = cwd.statFile(io, path, .{}) catch return false;
    return stat.kind == .file;
}

/// Check if a directory exists.
pub fn directoryExists(path: []const u8) bool {
    _ = validatePath(path) catch return false;

    const io = getIo();
    const cwd = getCwd();

    var dir = cwd.openDir(io, path, .{}) catch return false;
    dir.close(io);
    return true;
}

/// Get file size safely, returning null if file doesn't exist or is inaccessible.
pub fn getFileSize(path: []const u8) ?u64 {
    _ = validatePath(path) catch return null;

    const io = getIo();
    const cwd = getCwd();

    const stat = cwd.statFile(io, path, .{}) catch return null;
    if (stat.kind != .file) return null;
    return stat.size;
}

/// Copy a file safely with path validation.
pub fn copyFile(src_path: []const u8, dest_path: []const u8) FileError!void {
    _ = validatePath(src_path) catch |err| return err;
    _ = validatePath(dest_path) catch |err| return err;

    const io = getIo();
    const cwd = getCwd();

    cwd.copyFile(io, src_path, cwd, dest_path, .{}) catch |err| {
        return switch (err) {
            error.FileNotFound => error.NotFound,
            error.AccessDenied => error.PermissionDenied,
            error.PermissionDenied => error.PermissionDenied,
            error.NotDir => error.IsDirectory,
            else => error.WriteError,
        };
    };
}

/// Delete a file safely with path validation.
/// Returns success even if file doesn't exist (idempotent).
pub fn deleteFile(path: []const u8) FileError!void {
    _ = validatePath(path) catch |err| return err;

    const io = getIo();
    const cwd = getCwd();

    cwd.deleteFile(io, path) catch |err| {
        return switch (err) {
            error.FileNotFound => {}, // Idempotent delete
            error.AccessDenied => error.PermissionDenied,
            error.PermissionDenied => error.PermissionDenied,
            error.NotDir => error.IsDirectory,
            else => error.WriteError,
        };
    };
}

/// Rename/move a file safely with path validation.
pub fn renameFile(old_path: []const u8, new_path: []const u8) FileError!void {
    _ = validatePath(old_path) catch |err| return err;
    _ = validatePath(new_path) catch |err| return err;

    const io = getIo();
    const cwd = getCwd();

    cwd.rename(io, old_path, new_path) catch |err| {
        return switch (err) {
            error.FileNotFound => error.NotFound,
            error.AccessDenied => error.PermissionDenied,
            error.PermissionDenied => error.PermissionDenied,
            error.NotDir => error.IsDirectory,
            else => error.WriteError,
        };
    };
}

/// Safely join base path with filename, rejecting traversal attempts.
pub fn safePath(allocator: Allocator, base: []const u8, filename: []const u8) FileError![]u8 {
    if (hasTraversal(filename)) return error.TraversalDetected;

    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    result.appendSlice(base) catch return error.OutOfMemory;

    if (result.items.len > 0 and result.items[result.items.len - 1] != '/') {
        result.append('/') catch return error.OutOfMemory;
    }

    result.appendSlice(filename) catch return error.OutOfMemory;

    if (result.items.len > MAX_PATH_LENGTH) return error.PathTooLong;

    return result.toOwnedSlice() catch error.OutOfMemory;
}

test "hasTraversal" {
    try std.testing.expect(hasTraversal("../etc/passwd"));
    try std.testing.expect(hasTraversal("foo/../bar"));
    try std.testing.expect(hasTraversal("~/file"));
    try std.testing.expect(!hasTraversal("normal/path"));
    try std.testing.expect(!hasTraversal("file.txt"));
}

test "isSafePath" {
    try std.testing.expect(isSafePath("safe/path/file.txt"));
    try std.testing.expect(isSafePath("/absolute/path"));
    try std.testing.expect(!isSafePath("../unsafe"));
    try std.testing.expect(!isSafePath(""));
    try std.testing.expect(!isSafePath("path\x00with\x00nulls"));
}

test "validatePath" {
    const valid = try validatePath("valid/path.txt");
    try std.testing.expectEqualStrings("valid/path.txt", valid);

    try std.testing.expectError(error.TraversalDetected, validatePath("../traversal"));
    try std.testing.expectError(error.InvalidPath, validatePath(""));
}

test "safePath" {
    const allocator = std.testing.allocator;

    const result = try safePath(allocator, "/base/dir", "file.txt");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("/base/dir/file.txt", result);

    try std.testing.expectError(error.TraversalDetected, safePath(allocator, "/base", "../etc"));
}

test "fileExists" {
    // Test with a non-existent file
    try std.testing.expect(!fileExists("nonexistent_file_12345.txt"));
    try std.testing.expect(!fileExists("../traversal"));
}
