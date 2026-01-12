// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Library
    const lib = b.addStaticLibrary(.{
        .name = "proven",
        .root_source_file = b.path("src/proven.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(lib);

    // Module for consumption by other Zig projects
    _ = b.addModule("proven", .{
        .root_source_file = b.path("src/proven.zig"),
    });

    // Tests
    const main_tests = b.addTest(.{
        .root_source_file = b.path("src/proven.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_main_tests = b.addRunArtifact(main_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_main_tests.step);
}
