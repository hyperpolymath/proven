// SPDX-License-Identifier: MPL-2.0
//! ABI glue only: lexical transitions and finalization execute in Idris.
const bits8 = @import("refc/bits8.zig");
extern fn Proven_SafePath_ByteKernel_pathStep(?*anyopaque, ?*anyopaque) callconv(.c) ?*anyopaque;
extern fn Proven_SafePath_ByteKernel_pathFinish(?*anyopaque) callconv(.c) ?*anyopaque;
extern fn idris2_trampoline(?*anyopaque) callconv(.c) ?*anyopaque;

pub fn detect(bytes: []const u8) error{InvalidRefcResult}!bool {
    var state: u8 = 0;
    for (bytes) |byte| {
        const result = idris2_trampoline(Proven_SafePath_ByteKernel_pathStep(bits8.encode(state), bits8.encode(byte)));
        const next = try bits8.decode(result);
        if (next > 4) return error.InvalidRefcResult;
        state = next;
    }
    const result = idris2_trampoline(Proven_SafePath_ByteKernel_pathFinish(bits8.encode(state)));
    const finished = try bits8.decode(result);
    if (finished > 1) return error.InvalidRefcResult;
    return finished == 1;
}
