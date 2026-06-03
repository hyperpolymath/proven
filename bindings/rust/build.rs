// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! Build script for proven Rust bindings.
//!
//! Locates and links against the libproven shared library built from the
//! Zig FFI layer (which delegates to Idris 2 compiled code).

fn main() {
    // 1. Determine library search paths
    let mut search_paths = vec![
        "../../ffi/zig/zig-out/lib".to_string(),
        "/usr/local/lib".to_string(),
        "/usr/lib".to_string(),
    ];

    // Allow override via PROVEN_LIB_DIR environment variable
    if let Ok(env_path) = std::env::var("PROVEN_LIB_DIR") {
        search_paths.insert(0, env_path);
    }

    for path in &search_paths {
        println!("cargo:rustc-link-search=native={}", path);
    }

    // 2. Link against libproven (the compiled Zig/Idris2 library).
    // Prefer static linking if libproven.a is found (required for ClusterFuzzLite).
    let mut is_static = false;
    for path in &search_paths {
        if std::path::Path::new(path).join("libproven.a").exists() {
            is_static = true;
            break;
        }
    }

    if is_static {
        println!("cargo:rustc-link-lib=static=proven");
    } else {
        println!("cargo:rustc-link-lib=dylib=proven");
    }

    // 3. Add RPATH so binaries can find libproven.so relative to themselves (e.g. in ./lib/ or ./)
    // This is required for standalone distributions using dynamic linking.
    if std::env::var("CARGO_CFG_TARGET_OS").ok() == Some("linux".to_string()) {
        println!("cargo:rustc-link-arg=-Wl,-rpath,$ORIGIN/lib");
        println!("cargo:rustc-link-arg=-Wl,-rpath,$ORIGIN");
    }

    // Re-run build script if relevant files change
    println!("cargo:rerun-if-env-changed=PROVEN_LIB_DIR");
    println!("cargo:rerun-if-changed=../../ffi/zig/zig-out/lib/libproven.so");
    println!("cargo:rerun-if-changed=../../ffi/zig/zig-out/lib/libproven.a");
}
