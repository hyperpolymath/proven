// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Fuzz target: proven::safe_email::SafeEmail::is_valid — email validator
// FFI boundary. RFC 5321/5322 validator with addresses up to 254 chars
// + local-part up to 64; fuzzer exercises edge cases (UTF-8 in local
// part, unbalanced quotes, IDN host, IP-literal etc.) to find any
// panic that would denote an FFI marshaling error rather than a
// validity decision.

#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = proven::safe_email::SafeEmail::is_valid(s);
    }
});
