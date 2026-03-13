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
    mod: *std.Build.Module,
    refc_path: ?[]const u8,
    refc_runtime_path: ?[]const u8,
    c_support_path: ?[]const u8,
    support_lib_path: ?[]const u8,
) void {
    // -----------------------------------------------------------------------
    // 1. Generated RefC C sources (from idris2 --codegen refc)
    // -----------------------------------------------------------------------
    if (refc_path) |rp| {
        mod.addIncludePath(.{ .cwd_relative = rp });

        if (std.fs.openDirAbsolute(rp, .{ .iterate = true })) |dir_raw| {
            var dir = dir_raw;
            defer dir.close();
            var c_files: std.ArrayListUnmanaged([]const u8) = .empty;
            var iter = dir.iterate();
            while (iter.next() catch null) |entry| {
                if (entry.kind == .file) {
                    const name = entry.name;
                    if (std.mem.endsWith(u8, name, ".c")) {
                        c_files.append(b.allocator, b.allocator.dupe(u8, name) catch @panic("OOM")) catch @panic("OOM");
                    }
                }
            }
            if (c_files.items.len > 0) {
                mod.addCSourceFiles(.{
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
            // Directory doesn't exist yet (pre-RefC build)
        }
    }

    // -----------------------------------------------------------------------
    // 2. Idris2 RefC runtime headers + static library (libidris2_refc.a)
    // -----------------------------------------------------------------------
    if (refc_runtime_path) |rrp| {
        mod.addIncludePath(.{ .cwd_relative = rrp });
        mod.addObjectFile(.{ .cwd_relative = b.fmt("{s}/libidris2_refc.a", .{rrp}) });
    }

    // -----------------------------------------------------------------------
    // 3. Idris2 C support headers (idris_support.h, idris_file.h, etc.)
    // -----------------------------------------------------------------------
    if (c_support_path) |csp| {
        mod.addIncludePath(.{ .cwd_relative = csp });
    }

    // -----------------------------------------------------------------------
    // 4. Idris2 support library (libidris2_support.a / libidris2_support.so)
    // -----------------------------------------------------------------------
    if (support_lib_path) |slp| {
        mod.addLibraryPath(.{ .cwd_relative = slp });
        mod.linkSystemLibrary("idris2_support", .{});
    }

    // -----------------------------------------------------------------------
    // 5. System libraries required by RefC runtime
    // -----------------------------------------------------------------------
    mod.linkSystemLibrary("c", .{});
    mod.linkSystemLibrary("pthread", .{});
    mod.linkSystemLibrary("m", .{});
    mod.linkSystemLibrary("gmp", .{});
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
    // Main library module
    // ======================================================================

    const proven_mod = b.addModule("proven", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "idris2_zig_ffi", .module = idris2_zig_ffi_mod },
        },
    });

    proven_mod.addIncludePath(b.path("include"));
    configureRefC(b, proven_mod, refc_path, refc_runtime_path, c_support_path, support_lib_path);

    // ======================================================================
    // Static library for embedding (libproven.a)
    // ======================================================================

    const lib = b.addLibrary(.{
        .name = "proven",
        .root_module = proven_mod,
    });

    b.installArtifact(lib);

    // ======================================================================
    // Tests
    // ======================================================================

    const mod_tests = b.addTest(.{
        .root_module = proven_mod,
    });

    const run_mod_tests = b.addRunArtifact(mod_tests);
    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_mod_tests.step);
}
