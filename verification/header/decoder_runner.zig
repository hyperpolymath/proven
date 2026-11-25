// SPDX-License-Identifier: MPL-2.0
const std = @import("std");
const source = @import("bits8_decode_word.zig");
const word = @import("bits8_word.zig");
fn emit(raw: u64) !void {
    var buf: [96]u8 = undefined;
    const byte = source.decodeWord(raw) catch {
        const line = try std.fmt.bufPrint(&buf, "{d} invalid\n", .{raw});
        try std.fs.File.stdout().writeAll(line);
        return;
    };
    const line = try std.fmt.bufPrint(&buf, "{d} {d}\n", .{ raw, byte });
    try std.fs.File.stdout().writeAll(line);
}
pub fn main() !void {
    for (0..256) |index| {
        const raw = word.encodeWord(@intCast(index));
        try emit(raw);
        try emit(raw - 1);
        try emit(raw + 2);
        try emit(raw + 1099511627776);
    }
    try emit(0);
    try emit(std.math.maxInt(u64));
}

