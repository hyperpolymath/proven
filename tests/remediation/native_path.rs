// SPDX-License-Identifier: MPL-2.0
#[allow(dead_code)]
#[path = "../../bindings/rust/src/ffi.rs"]
mod ffi;

fn main() {
    let corpus: &[&[u8]] = &[b"", b"..", b"..\\x", b"a\\..\\b", b"..\0safe",
        b"safe\0..\\x", b"...", b"a..b", b"safe/file", &[255, 0, 46, 46, 92, 128]];
    for bytes in corpus {
        for len in 0..=bytes.len() {
            let input = &bytes[..len];
            let expected = input.split(|b| matches!(b, 0 | 47 | 92)).any(|part| part == b"..");
            // SAFETY: input is borrowed readable storage for exactly len bytes.
            let r = unsafe { ffi::proven_path_has_traversal(input.as_ptr(), len) };
            assert_eq!(r.status, 0);
            assert_eq!(r.value, expected);
        }
    }
    // SAFETY: null is a specified refusal, including zero length.
    let null = unsafe { ffi::proven_path_has_traversal(std::ptr::null(), 0) };
    assert_eq!(null.status, -1);
    assert!(!null.value);
    println!("PASS: actual Rust FFI path bytes, prefixes, NUL, high bytes and null refusal");
}
