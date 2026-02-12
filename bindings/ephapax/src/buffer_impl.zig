// SPDX-License-Identifier: Apache-2.0
//! Pure Zig implementation of SafeBuffer

const std = @import("std");

pub const Buffer = struct {
    capacity: u64,
    size: u64,
    data: []u8,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, capacity: u64) !*Buffer {
        const buf = try allocator.create(Buffer);
        const data = try allocator.alloc(u8, capacity);
        buf.* = .{
            .capacity = capacity,
            .size = 0,
            .data = data,
            .allocator = allocator,
        };
        return buf;
    }

    pub fn deinit(self: *Buffer) void {
        self.allocator.free(self.data);
        self.allocator.destroy(self);
    }

    pub fn write(self: *Buffer, data: []const u8) !void {
        if (self.size + data.len > self.capacity) {
            return error.BufferFull;
        }
        @memcpy(self.data[self.size..][0..data.len], data);
        self.size += data.len;
    }

    pub fn read(self: *Buffer, offset: u64, len: u64, out: []u8) !void {
        if (offset + len > self.size) {
            return error.OutOfBounds;
        }
        @memcpy(out[0..len], self.data[offset..][0..len]);
    }

    pub fn isFull(self: *Buffer) bool {
        return self.size >= self.capacity;
    }

    pub fn remaining(self: *Buffer) u64 {
        return self.capacity - self.size;
    }
};

// C-compatible FFI exports
var gpa = std.heap.GeneralPurposeAllocator(.{}){};

pub export fn idris_proven_buffer_new(capacity: u64) ?*Buffer {
    const allocator = gpa.allocator();
    return Buffer.init(allocator, capacity) catch null;
}

pub export fn idris_proven_buffer_write(
    buffer: *Buffer,
    data: [*]const u8,
    data_len: u64,
) i32 {
    const data_slice = data[0..data_len];
    buffer.write(data_slice) catch return 1;
    return 0;
}

pub export fn idris_proven_buffer_read(
    buffer: *Buffer,
    offset: u64,
    len: u64,
    out_data: [*]u8,
) i32 {
    const out_slice = out_data[0..len];
    buffer.read(offset, len, out_slice) catch return 1;
    return 0;
}

pub export fn idris_proven_buffer_capacity(buffer: *Buffer) u64 {
    return buffer.capacity;
}

pub export fn idris_proven_buffer_size(buffer: *Buffer) u64 {
    return buffer.size;
}

pub export fn idris_proven_buffer_is_full(buffer: *Buffer) bool {
    return buffer.isFull();
}

pub export fn idris_proven_buffer_remaining(buffer: *Buffer) u64 {
    return buffer.remaining();
}

pub export fn idris_proven_buffer_free(buffer: *Buffer) void {
    buffer.deinit();
}
