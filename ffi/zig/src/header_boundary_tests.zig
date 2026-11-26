// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
const std = @import("std");
const core = @import("core.zig");

// This double observes the actual public entry's dispatch. The separate real
// ABI fixtures still link the Idris backend; this is not a replacement for it.
const Probe = struct {
    const Mode = enum { clean, found, unavailable, invalid };
    var mode: Mode = .clean;
    var calls: usize = 0;
    var received: ?[*]const u8 = null;
    var length: usize = 0;

    fn reset(next: Mode) void {
        mode = next;
        calls = 0;
        received = null;
        length = 0;
    }

    pub fn detect(bytes: []const u8) error{ BackendUnavailable, InvalidRefcResult }!bool {
        calls += 1;
        received = bytes.ptr;
        length = bytes.len;
        return switch (mode) {
            .clean => false,
            .found => true,
            .unavailable => error.BackendUnavailable,
            .invalid => error.InvalidRefcResult,
        };
    }
};

test "header public boundary refuses null before invoking backend" {
    for ([_]usize{ 0, 9, std.math.maxInt(usize) }) |len| {
        Probe.reset(.found);
        const result = core.headerHasCrlf(Probe, null, len);
        try std.testing.expectEqual(@as(c_int, -1), result.status);
        try std.testing.expectEqual(false, result.value);
        try std.testing.expectEqual(@as(usize, 0), Probe.calls);
        try std.testing.expectEqual(@as(?[*]const u8, null), Probe.received);
    }
}

test "header public boundary forwards every bounded subspan once and preserves storage" {
    var memory = [_]u8{ 255, 0, 13, 128, 10, 7 };
    const before = memory;
    for (0..memory.len + 1) |offset| {
        for (offset..memory.len + 1) |end| {
            const span = memory[offset..end];
            for ([_]Probe.Mode{ .clean, .found }) |mode| {
                Probe.reset(mode);
                const result = core.headerHasCrlf(Probe, span.ptr, span.len);
                try std.testing.expectEqual(@as(c_int, 0), result.status);
                try std.testing.expectEqual(mode == .found, result.value);
                try std.testing.expectEqual(@as(usize, 1), Probe.calls);
                try std.testing.expectEqual(@as(?[*]const u8, span.ptr), Probe.received);
                try std.testing.expectEqual(span.len, Probe.length);
                try std.testing.expectEqualSlices(u8, &before, &memory);
            }
        }
    }
}

test "header public boundary preserves error classes and rejects their Boolean" {
    const memory = [_]u8{ 0, 255, 13 };
    for ([_]Probe.Mode{ .unavailable, .invalid }) |mode| {
        for ([_]usize{ 0, memory.len }) |len| {
            Probe.reset(mode);
            const result = core.headerHasCrlf(Probe, &memory, len);
            const expected: c_int = if (mode == .unavailable) -99 else -7;
            try std.testing.expectEqual(expected, result.status);
            try std.testing.expectEqual(false, result.value);
            try std.testing.expectEqual(@as(usize, 1), Probe.calls);
            try std.testing.expectEqual(len, Probe.length);
        }
    }
}

// The exported C entries cannot take a backend double without linking either
// the Idris archive or the unavailable stub. Observe their forwarding instead:
// each exported entry must be exactly one delegation that passes its own
// `ptr` and `len` unchanged to the shared boundary, with no other statement.
fn entryBody(source: []const u8) error{EntryNotFound}![]const u8 {
    const head = "pub export fn proven_header_has_crlf(ptr: ?[*]const u8, len: usize) callconv(.c)";
    const start = std.mem.indexOf(u8, source, head) orelse return error.EntryNotFound;
    const open = std.mem.indexOfPos(u8, source, start, "{") orelse return error.EntryNotFound;
    const close = std.mem.indexOfPos(u8, source, open, "\n}") orelse return error.EntryNotFound;
    return std.mem.trim(u8, source[open + 1 .. close], " \n");
}

test "exported header entries forward ptr and len unchanged in a single delegation" {
    const standalone = try entryBody(@embedFile("main.zig"));
    const refc = try entryBody(@embedFile("main_refc.zig"));
    try std.testing.expectEqualStrings(
        "return core.headerHasCrlf(@import(\"header_unavailable.zig\"), ptr, len);",
        standalone,
    );
    try std.testing.expectEqualStrings(
        "return core.headerHasCrlf(@import(\"header_refc.zig\"), ptr, len);",
        refc,
    );
}
