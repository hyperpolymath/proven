// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

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
//! RefC marshaling lives in src/refc and uses the compiler's actual SDK.

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
            while (iter.next() catch @panic("cannot enumerate RefC translation units")) |entry| {
                if (entry.kind == .file) {
                    const name = entry.name;
                    if (std.mem.endsWith(u8, name, ".c")) {
                        c_files.append(b.allocator, b.allocator.dupe(u8, name) catch @panic("OOM")) catch @panic("OOM");
                    }
                }
            }
            if (c_files.items.len == 0) @panic("empty RefC source directory; refusing an unwired library");
            {
                mod.addCSourceFiles(.{
                    .root = .{ .cwd_relative = rp },
                    .files = c_files.items,
                    .flags = &.{
                        "-std=c11",
                        "-fno-strict-aliasing",
                        "-D_GNU_SOURCE",
                        "-Dmain=idris_proven_codegen_main",
                    },
                });
            }
        } else |_| {
            @panic("missing RefC source directory; build Idris before linking");
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
        mod.addRPath(.{ .cwd_relative = slp });
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
    if (refc_path == null or refc_runtime_path == null or c_support_path == null or support_lib_path == null)
        @panic("the full FFI build requires all four Idris RefC paths; use build_standalone.zig for explicit unavailable-backend behavior");

    // ======================================================================
    // Main library module
    // ======================================================================

    const proven_mod = b.addModule("proven", .{
        .root_source_file = b.path("src/main_refc.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lifecycle_mod = b.createModule(.{
        .root_source_file = b.path("src/lifecycle_refc.zig"),
        .target = target,
        .optimize = optimize,
    });
    lifecycle_mod.addIncludePath(.{ .cwd_relative = refc_runtime_path.? });
    lifecycle_mod.addIncludePath(.{ .cwd_relative = c_support_path.? });
    proven_mod.addImport("lifecycle", lifecycle_mod);
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
    const shared = b.addLibrary(.{
        .name = "proven",
        .root_module = proven_mod,
        .linkage = .dynamic,
    });
    b.installArtifact(shared);

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
