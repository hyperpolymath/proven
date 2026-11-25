// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
const std = @import("std");
const builtin = @import("builtin");
const BoolResult = extern struct { status: c_int, value: bool };
extern fn proven_header_has_crlf(?[*]const u8, usize) callconv(.c) BoolResult;

fn requireCorrect(result: BoolResult, expected: bool) !void {
    if (result.status != 0) return error.WrongHeaderStatus;
    if (result.value != expected) return error.WrongHeaderResult;
}

fn sampleCase(name: []const u8, input: []const u8, expected: bool, iterations: usize) !void {
    // Qualify each corpus before measuring. Timed calls retain correctness
    // guards too; elapsed time includes those guards, not only the foreign call.
    try requireCorrect(proven_header_has_crlf(input.ptr, input.len), expected);
    for (0..32) |_| try requireCorrect(proven_header_has_crlf(input.ptr, input.len), expected);
    var digest = [_]u8{0} ** 32;
    std.crypto.hash.sha2.Sha256.hash(input, &digest, .{});
    const input_hash = std.fmt.bytesToHex(digest, .lower);
    for (0..3) |sample| {
        var checksum: usize = 0;
        var timer = try std.time.Timer.start();
        for (0..iterations) |_| {
            const result = proven_header_has_crlf(input.ptr, input.len);
            try requireCorrect(result, expected);
            checksum += @intFromBool(result.value);
        }
        const elapsed = timer.read();
        if (elapsed == 0) return error.TimerResolutionUnavailable;
        var buffer = [_]u8{0} ** 1024;
        const row = try std.fmt.bufPrint(&buffer,
            "{{\"schema\":\"proven.header.benchmark/1\",\"backend\":\"actual-exported-refc-abi\",\"case\":\"{s}\",\"zig\":\"{s}\",\"mode\":\"{s}\",\"sample\":{d},\"bytes\":{d},\"iterations\":{d},\"warmup_iterations\":32,\"elapsed_ns\":{d},\"checksum\":{d},\"expected\":{},\"input_sha256\":\"{s}\",\"timed_correctness_guards\":true,\"correctness\":\"PASS\"}}\n",
            .{ name, builtin.zig_version_string, @tagName(builtin.mode), sample, input.len, iterations, elapsed, checksum, expected, input_hash });
        try std.fs.File.stdout().writeAll(row);
    }
}

pub fn main() !void {
    var input = [_]u8{'A'} ** 4096;
    try sampleCase("empty", input[0..0], false, 4096);
    try sampleCase("clean-byte", input[0..1], false, 4096);
    try sampleCase("clean-64", input[0..64], false, 2048);
    input[63] = 13;
    try sampleCase("forbidden-64", input[0..64], true, 2048);
    input[63] = 'A';
    input[4095] = 10;
    try sampleCase("forbidden-4096", &input, true, 256);
}
