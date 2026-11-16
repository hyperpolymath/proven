// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! RefC Bits8 marshaling. The predicate and transition live in Idris.
const bits8 = @import("refc/bits8.zig");
extern fn Proven_SafeHeader_ByteKernel_headerStep(?*anyopaque, ?*anyopaque) callconv(.c) ?*anyopaque;
extern fn idris2_trampoline(?*anyopaque) callconv(.c) ?*anyopaque;

pub fn detect(bytes: []const u8) error{InvalidRefcResult}!bool {
    var state: u8 = 0;
    for (bytes) |b| {
        const result = idris2_trampoline(Proven_SafeHeader_ByteKernel_headerStep(bits8.encode(state), bits8.encode(b)));
        // Accept only the two canonical RefC Bits8 state encodings.
        const next = try bits8.decode(result);
        if (next > 1) return error.InvalidRefcResult;
        state = next;
    }
    return state == 1;
}

test {
    _ = bits8;
}
