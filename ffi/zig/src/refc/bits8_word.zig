// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// Scalar marshaling only. bits8.zig still checks the SDK shift and target.
pub fn encodeWord(value: u8) u64 {
    return (@as(u64, value) << 32) | 1;
}
