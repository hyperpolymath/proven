// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
// Consume the real Rust declarations, without building unrelated bindings.
#[allow(dead_code)]
#[path = "../../bindings/rust/src/ffi.rs"]
mod ffi;

fn check(condition: bool, message: &str) -> Result<(), String> {
    if condition { Ok(()) } else { Err(message.to_owned()) }
}

fn main() -> Result<(), String> {
    // SAFETY: no arguments/pointers; reject old ABI before lifecycle calls.
    check(unsafe { ffi::proven_ffi_abi_version() } == 2, "ABI version must be 2")?;
    for _ in 0..256 {
        // SAFETY: these canonical functions take no pointers; this test owns
        // its process's lifecycle. Native tests separately cover concurrency.
        unsafe {
            ffi::proven_deinit();
            check(!ffi::proven_is_initialized(), "initially stopped")?;
            check(ffi::proven_init() == 0, "init returns success status")?;
            check(ffi::proven_init() == 0, "repeated init")?;
            check(ffi::proven_is_initialized(), "ready")?;
            ffi::proven_deinit();
            ffi::proven_deinit();
            check(!ffi::proven_is_initialized(), "stopped")?;
        }
    }
    println!("PASS: lifecycle through actual Rust FFI declarations");
    Ok(())
}
