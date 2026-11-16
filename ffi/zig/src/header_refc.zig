// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! RefC Bits8 marshaling. The predicate and transition live in Idris.
const c = @cImport({
    @cInclude("_datatypes.h");
});
extern fn Proven_SafeHeader_ByteKernel_headerStep(?*anyopaque, ?*anyopaque) callconv(.c) ?*anyopaque;
extern fn idris2_trampoline(?*anyopaque) callconv(.c) ?*anyopaque;
const shift = c.idris2_vp_int_shift;

fn box(b: u8) ?*anyopaque {
    return @ptrFromInt((@as(usize, b) << shift) | 1);
}

pub fn detect(bytes: []const u8) error{InvalidRefcResult}!bool {
    var state: u8 = 0;
    for (bytes) |b| {
        const result = idris2_trampoline(Proven_SafeHeader_ByteKernel_headerStep(box(state), box(b)));
        const raw = @intFromPtr(result);
        // Accept only the two canonical RefC Bits8 state encodings.
        if (raw == @intFromPtr(box(0))) {
            state = 0;
        } else if (raw == @intFromPtr(box(1))) {
            state = 1;
        } else return error.InvalidRefcResult;
    }
    return state == 1;
}
