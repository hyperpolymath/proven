// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Build script for proven_nif: locates libproven shared library.
//!
//! Searches for libproven in the following order:
//! 1. PROVEN_LIB_DIR environment variable
//! 2. Relative path from binding: ../../../../ffi/zig/zig-out/lib
//! 3. /usr/local/lib
//! 4. /usr/lib

fn main() {
    // Attempt to find libproven from environment or well-known paths.
    let search_paths = vec![
        std::env::var("PROVEN_LIB_DIR").ok(),
        resolve_relative_path("../../../../ffi/zig/zig-out/lib"),
        Some("/usr/local/lib".to_string()),
        Some("/usr/lib".to_string()),
    ];

    for path_opt in &search_paths {
        if let Some(ref path) = path_opt {
            let lib_path = std::path::Path::new(path);
            if lib_path.exists() {
                println!("cargo:rustc-link-search=native={}", path);
            }
        }
    }

    println!("cargo:rustc-link-lib=dylib=proven");

    // Re-run if the environment variable changes.
    println!("cargo:rerun-if-env-changed=PROVEN_LIB_DIR");
}

/// Resolve a relative path from the Cargo.toml directory to an absolute path.
fn resolve_relative_path(relative: &str) -> Option<String> {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").ok()?;
    let full_path = std::path::Path::new(&manifest_dir).join(relative);
    if full_path.exists() {
        full_path.canonicalize().ok().map(|p| p.to_string_lossy().to_string())
    } else {
        // Return the path even if it does not exist yet; the linker will
        // decide whether the library is actually there.
        Some(full_path.to_string_lossy().to_string())
    }
}
