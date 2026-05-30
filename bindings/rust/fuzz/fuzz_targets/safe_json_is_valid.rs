// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Fuzz target: proven::safe_json::SafeJson::is_valid — JSON validator.
// Higher-value target than the others because JSON parsers historically
// crash on stack-busting nested structures, unterminated strings,
// degenerate escapes, surrogate-pair edge cases. Idris2 totality + the
// FFI bridge should make all inputs return a Result; the fuzzer's job
// is to find any input that panics, OOMs, or loops the FFI thread.

#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = proven::safe_json::SafeJson::is_valid(s);
    }
});
