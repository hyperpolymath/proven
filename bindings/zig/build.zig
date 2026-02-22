// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Build configuration for Proven Zig bindings.
// Links against libproven (Idris 2 core + Zig FFI bridge).

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Create module for library
    const proven_mod = b.createModule(.{
        .root_source_file = b.path("src/proven.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Link against libproven (the Idris 2 + Zig FFI compiled library)
    proven_mod.linkSystemLibrary("proven", .{});

    // Add C header include path for proven.h
    proven_mod.addIncludePath(b.path("../c/include"));

    // Library (Zig 0.16+ API)
    const lib = b.addLibrary(.{
        .name = "proven",
        .linkage = .static,
        .root_module = proven_mod,
    });

    b.installArtifact(lib);

    // Module for consumption by other Zig projects
    _ = b.addModule("proven", .{
        .root_source_file = b.path("src/proven.zig"),
    });

    // Tests (Zig 0.16+ API - uses root_module)
    const test_mod = b.createModule(.{
        .root_source_file = b.path("src/proven.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Tests also need libproven
    test_mod.linkSystemLibrary("proven", .{});
    test_mod.addIncludePath(b.path("../c/include"));

    const main_tests = b.addTest(.{
        .root_module = test_mod,
    });

    const run_main_tests = b.addRunArtifact(main_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_main_tests.step);
}
