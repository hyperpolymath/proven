// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

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

    const main_tests = b.addTest(.{
        .root_module = test_mod,
    });

    const run_main_tests = b.addRunArtifact(main_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_main_tests.step);
}
