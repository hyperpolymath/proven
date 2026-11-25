// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
// A standalone fixture has no Idris lifecycle implementation.
pub const available = false;
pub fn init() i32 {
    return -99;
}
pub fn deinit() void {}
pub fn isInitialized() bool {
    return false;
}
