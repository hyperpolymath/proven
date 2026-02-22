// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Zig FFI Bridge for Proven
//!
//! This build file compiles the Idris 2 RefC output and wraps it in a
//! stable C ABI that can be consumed by Python, Rust, Go, and other languages.
//!
//! Pipeline: Idris2 --codegen refc--> .c files --zig build--> libproven.{so,dylib,dll,a}
//!
//! Build options (pass via -D flags):
//!   -Didris-refc=<path>           Path to generated RefC .c files (build/refc/)
//!   -Didris-refc-runtime=<path>   Path to Idris2 RefC runtime headers + libidris2_refc.a
//!   -Didris-c-support=<path>      Path to Idris2 C support headers (idris_support.h, etc.)
//!   -Didris-support-lib=<path>    Path to libidris2_support.{a,so}
//!
//! Uses idris2-zig-ffi for generic Idris 2 FFI infrastructure.

const std = @import("std");

/// Configure a compilation step with all Idris2 RefC includes, sources, and libraries.
/// This is the core of the pipeline: it wires up the generated C code, the RefC runtime,
/// and the Idris2 support library so that Zig can link everything into a single artifact.
fn configureRefC(
    b: *std.Build,
    step: *std.Build.Step.Compile,
    refc_path: ?[]const u8,
    refc_runtime_path: ?[]const u8,
    c_support_path: ?[]const u8,
    support_lib_path: ?[]const u8,
) void {
    // -----------------------------------------------------------------------
    // 1. Generated RefC C sources (from idris2 --codegen refc)
    // -----------------------------------------------------------------------
    if (refc_path) |rp| {
        // Include path so generated .c files can find their own headers
        step.addIncludePath(.{ .cwd_relative = rp });

        // Scan the RefC output directory for all .c files.
        // Idris2 RefC generates one or more .c files depending on the package;
        // we compile them all rather than hardcoding filenames.
        if (std.fs.openDirAbsolute(rp, .{ .iterate = true })) |dir_raw| {
            var dir = dir_raw;
            defer dir.close();
            var c_files = std.ArrayList([]const u8).init(b.allocator);
            var iter = dir.iterate();
            while (iter.next() catch null) |entry| {
                if (entry.kind == .file) {
                    const name = entry.name;
                    if (std.mem.endsWith(u8, name, ".c")) {
                        c_files.append(b.allocator.dupe(u8, name) catch @panic("OOM")) catch @panic("OOM");
                    }
                }
            }
            if (c_files.items.len > 0) {
                step.addCSourceFiles(.{
                    .root = .{ .cwd_relative = rp },
                    .files = c_files.items,
                    .flags = &.{
                        "-std=c11",
                        "-fno-strict-aliasing",
                        "-D_GNU_SOURCE",
                    },
                });
            }
        } else |_| {
            // Directory doesn't exist yet (pre-RefC build); that's okay for
            // target enumeration. The actual build will fail with a clear error.
        }
    }

    // -----------------------------------------------------------------------
    // 2. Idris2 RefC runtime headers + static library (libidris2_refc.a)
    // -----------------------------------------------------------------------
    if (refc_runtime_path) |rrp| {
        // Headers: runtime.h, memoryManagement.h, _datatypes.h, etc.
        step.addIncludePath(.{ .cwd_relative = rrp });

        // Static library: libidris2_refc.a
        step.addObjectFile(.{ .cwd_relative = b.fmt("{s}/libidris2_refc.a", .{rrp}) });
    }

    // -----------------------------------------------------------------------
    // 3. Idris2 C support headers (idris_support.h, idris_file.h, etc.)
    // -----------------------------------------------------------------------
    if (c_support_path) |csp| {
        step.addIncludePath(.{ .cwd_relative = csp });
    }

    // -----------------------------------------------------------------------
    // 4. Idris2 support library (libidris2_support.a / libidris2_support.so)
    // -----------------------------------------------------------------------
    if (support_lib_path) |slp| {
        step.addLibraryPath(.{ .cwd_relative = slp });
        step.linkSystemLibrary("idris2_support");
    }

    // -----------------------------------------------------------------------
    // 5. System libraries required by RefC runtime
    // -----------------------------------------------------------------------
    step.linkLibC();
    // RefC runtime uses pthreads for concurrency primitives
    step.linkSystemLibrary("pthread");
    // RefC runtime uses libm for math operations
    step.linkSystemLibrary("m");
    // GMP is required by Idris2's Integer type (arbitrary precision)
    step.linkSystemLibrary("gmp");
}

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // ======================================================================
    // Build options for the Idris2 RefC pipeline
    // ======================================================================

    const refc_path = b.option(
        []const u8,
        "idris-refc",
        "Path to Idris 2 RefC generated C files (e.g., build/refc/)",
    );
    const refc_runtime_path = b.option(
        []const u8,
        "idris-refc-runtime",
        "Path to Idris 2 RefC runtime (headers + libidris2_refc.a)",
    );
    const c_support_path = b.option(
        []const u8,
        "idris-c-support",
        "Path to Idris 2 C support headers (idris_support.h, etc.)",
    );
    const support_lib_path = b.option(
        []const u8,
        "idris-support-lib",
        "Path to libidris2_support.{a,so} directory",
    );

    // ======================================================================
    // Get idris2-zig-ffi dependency
    // ======================================================================

    const idris2_zig_ffi_dep = b.dependency("idris2_zig_ffi", .{
        .target = target,
        .optimize = optimize,
    });
    const idris2_zig_ffi_mod = idris2_zig_ffi_dep.module("idris2_zig_ffi");

    // ======================================================================
    // Static library for embedding (libproven.a)
    // ======================================================================

    const lib = b.addStaticLibrary(.{
        .name = "proven",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    lib.root_module.addImport("idris2_zig_ffi", idris2_zig_ffi_mod);
    lib.addIncludePath(b.path("include"));

    configureRefC(b, lib, refc_path, refc_runtime_path, c_support_path, support_lib_path);

    b.installArtifact(lib);

    // ======================================================================
    // Shared library for dynamic loading (libproven.so / .dylib / .dll)
    // ======================================================================

    const shared_lib = b.addSharedLibrary(.{
        .name = "proven",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    shared_lib.root_module.addImport("idris2_zig_ffi", idris2_zig_ffi_mod);
    shared_lib.addIncludePath(b.path("include"));

    configureRefC(b, shared_lib, refc_path, refc_runtime_path, c_support_path, support_lib_path);

    b.installArtifact(shared_lib);

    // ======================================================================
    // WASM targets (using idris2-zig-ffi infrastructure)
    // Note: WASM targets do NOT link RefC -- they use the Zig-native
    // implementations only. RefC requires libc/pthreads/gmp which are
    // not available in WASM. Future work: compile Idris2 to JS backend
    // for WASM interop.
    // ======================================================================

    // Browser WASM
    const wasm_browser = b.addSharedLibrary(.{
        .name = "proven",
        .root_source_file = b.path("src/main.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
        }),
        .optimize = .ReleaseSmall,
    });
    wasm_browser.root_module.addImport("idris2_zig_ffi", idris2_zig_ffi_dep.module("idris2_zig_ffi"));
    wasm_browser.rdynamic = true;

    const wasm_browser_step = b.step("wasm", "Build WASM library for browsers");
    const wasm_browser_install = b.addInstallArtifact(wasm_browser, .{});
    wasm_browser_step.dependOn(&wasm_browser_install.step);

    // WASI
    const wasm_wasi = b.addSharedLibrary(.{
        .name = "proven",
        .root_source_file = b.path("src/main.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .wasi,
        }),
        .optimize = .ReleaseSmall,
    });
    wasm_wasi.root_module.addImport("idris2_zig_ffi", idris2_zig_ffi_dep.module("idris2_zig_ffi"));
    wasm_wasi.rdynamic = true;

    const wasm_wasi_step = b.step("wasi", "Build WASI library for runtimes");
    const wasm_wasi_install = b.addInstallArtifact(wasm_wasi, .{});
    wasm_wasi_step.dependOn(&wasm_wasi_install.step);

    // All WASM targets
    const all_wasm_step = b.step("all-wasm", "Build all WASM targets");
    all_wasm_step.dependOn(wasm_browser_step);
    all_wasm_step.dependOn(wasm_wasi_step);

    // ======================================================================
    // Header file installation (for C/C++ consumers)
    // ======================================================================

    // The Zig library exposes a C ABI, but consumers using Zig, Rust (via zig-cc),
    // or other modern FFI mechanisms don't need a header file.
    // Uncomment if you need C header for legacy interop:
    // b.installFile("include/proven.h", "include/proven.h");

    // ======================================================================
    // Tests
    // ======================================================================

    const main_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    main_tests.root_module.addImport("idris2_zig_ffi", idris2_zig_ffi_mod);

    // Wire up RefC for integration tests so they can exercise the full pipeline
    configureRefC(b, main_tests, refc_path, refc_runtime_path, c_support_path, support_lib_path);

    const run_main_tests = b.addRunArtifact(main_tests);
    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_main_tests.step);

    // ======================================================================
    // Integration test step (runs only FFI integration tests)
    // ======================================================================

    const integration_test_path = b.path("test/integration_test.zig");
    // Only add integration tests if the file exists
    if (std.fs.cwd().access("test/integration_test.zig", .{})) |_| {
        const integration_tests = b.addTest(.{
            .root_source_file = integration_test_path,
            .target = target,
            .optimize = optimize,
        });
        integration_tests.root_module.addImport("idris2_zig_ffi", idris2_zig_ffi_mod);
        configureRefC(b, integration_tests, refc_path, refc_runtime_path, c_support_path, support_lib_path);

        const run_integration_tests = b.addRunArtifact(integration_tests);
        const integration_step = b.step("test-integration", "Run FFI integration tests");
        integration_step.dependOn(&run_integration_tests.step);
    } else |_| {
        // No integration test file yet; skip silently
    }
}
