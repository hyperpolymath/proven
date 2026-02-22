// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Build script for proven Rust bindings.
//!
//! Locates and links against the libproven shared library built from the
//! Zig FFI layer (which delegates to Idris 2 compiled code).

fn main() {
    // Development build path: relative to bindings/rust/ within the repo
    println!("cargo:rustc-link-search=native=../../ffi/zig/zig-out/lib");

    // System-wide installation paths
    println!("cargo:rustc-link-search=native=/usr/local/lib");
    println!("cargo:rustc-link-search=native=/usr/lib");

    // Allow override via PROVEN_LIB_DIR environment variable
    if let Ok(lib_dir) = std::env::var("PROVEN_LIB_DIR") {
        println!("cargo:rustc-link-search=native={}", lib_dir);
    }

    // Link against libproven (the compiled Zig/Idris2 shared library)
    println!("cargo:rustc-link-lib=dylib=proven");

    // Re-run build script if the library changes
    println!("cargo:rerun-if-env-changed=PROVEN_LIB_DIR");
    println!("cargo:rerun-if-changed=../../ffi/zig/zig-out/lib/libproven.so");
    println!("cargo:rerun-if-changed=../../ffi/zig/zig-out/lib/libproven.dylib");
}
