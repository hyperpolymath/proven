// SPDX-License-Identifier: PMPL-1.0
// Zig build file for Ephapax ↔ Proven FFI bindings

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Create module for the library
    const lib_module = b.createModule(.{
        .root_source_file = b.path("src/ephapax_proven.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Add C stub implementations to the module
    lib_module.addCSourceFile(.{
        .file = b.path("src/stubs.c"),
        .flags = &[_][]const u8{
            "-std=c11",
            "-Wall",
            "-Wextra",
        },
    });
    lib_module.linkLibC();

    // Build library
    const lib = b.addLibrary(.{
        .name = "ephapax_proven",
        .root_module = lib_module,
    });

    b.installArtifact(lib);

    // Create tests
    const unit_tests = b.addTest(.{
        .root_source_file = b.path("tests/test_all.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Link tests against C stubs
    unit_tests.root_module.addCSourceFile(.{
        .file = b.path("src/stubs.c"),
        .flags = &[_][]const u8{"-std=c11"},
    });
    unit_tests.root_module.linkLibC();

    const run_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);
}
