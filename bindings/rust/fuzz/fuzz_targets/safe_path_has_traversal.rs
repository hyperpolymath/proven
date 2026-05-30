// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Fuzz target: proven::safe_path::SafePath::has_traversal — path-
// traversal detector. CWE-22 surface. Fuzzer exercises path strings
// with mixed separators, percent-encoding, NUL bytes, embedded `..`
// patterns, Windows UNC, file:// URIs. Idris2 ensures the detector
// is total over `String`; this guards the Rust↔FFI marshaling for
// odd UTF-8 boundary conditions.

#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = proven::safe_path::SafePath::has_traversal(s);
    }
});
