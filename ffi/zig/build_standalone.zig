// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// build_standalone.zig — standalone build for proven FFI without the Idris2 RefC
// pipeline dependency (idris2_zig_ffi from nextgen-languages/language-bridges).
//
// This build produces libproven_ffi.a from src/main.zig only, exposing the
// pure-Zig C ABI symbols (proven_path_has_traversal, proven_init, etc.) that
// consumers such as developer-ecosystem/zig-api can link against.
//
// Usage:
//   zig build --build-file build_standalone.zig
//   # Outputs: zig-out-standalone/lib/libproven_ffi.a

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target   = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // -------------------------------------------------------------------------
    // Static library — libproven_ffi.a
    // Contains all C ABI exports from src/main.zig, including
    // proven_path_has_traversal and the lifecycle functions.
    // -------------------------------------------------------------------------
    const proven_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target           = target,
        .optimize         = optimize,
        .link_libc        = true,
    });

    const lib = b.addLibrary(.{
        .name        = "proven_ffi",
        .root_module = proven_mod,
        .linkage     = .static,
    });
    b.installArtifact(lib);

    // -------------------------------------------------------------------------
    // Unit tests — run with `zig build test --build-file build_standalone.zig`
    // -------------------------------------------------------------------------
    const test_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target           = target,
        .optimize         = optimize,
        .link_libc        = true,
    });
    const unit_tests = b.addTest(.{
        .root_module = test_mod,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run proven_ffi unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
