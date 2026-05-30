// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Fuzz target: proven::safe_url::SafeUrl::parse — RFC 3986 URL parser
// FFI boundary. Throws arbitrary UTF-8 strings at the parser; any panic
// (FFI buffer mishandling, string-bounds violation, allocator misuse) is
// a finding to investigate. Idris2 verification covers the parser logic
// itself; this surface guards against marshaling errors in the Rust↔C
// boundary.

#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = proven::safe_url::SafeUrl::parse(s);
    }
});
