// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// Pure word marshaling; the pointer carrier and SDK profile stay in bits8.zig.
const word = @import("bits8_word.zig");

pub fn decodeWord(raw: u64) error{InvalidRefcResult}!u8 {
    const payload = raw / 4294967296;
    if (payload > 255) return error.InvalidRefcResult;
    const byte: u8 = @intCast(payload);
    if (raw != word.encodeWord(byte)) return error.InvalidRefcResult;
    return byte;
}
