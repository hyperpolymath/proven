// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Fuzz target: proven::safe_path::SafePath::has_traversal — path-
// traversal detector. CWE-22 surface. Fuzzer exercises path strings
// with mixed separators, percent-encoding, NUL bytes, embedded `..`
// patterns and raw separators. The independent component oracle checks the
// actual Idris byte policy through the Rust wrapper and RefC/Zig boundary.
// Percent decoding and filesystem confinement remain outside this predicate.

#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let expected = data.split(|b| matches!(b, 0 | 47 | 92)).any(|part| part == b"..");
        assert_eq!(proven::safe_path::SafePath::has_traversal(s), Ok(expected));
    }
});
