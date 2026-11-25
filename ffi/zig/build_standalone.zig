// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// build_standalone.zig — build the legacy surface with an optional qualified
// Idris header bundle, without requiring whole-package RefC code generation.
//
// Without -Dheader-refc the header export refuses with -99. Passing a native
// SDK bundle from scripts/build-header-refc.sh selects the real Idris kernel.
// Other legacy exports are unchanged; this is not a whole-library proof claim.
//
// Usage:
//   zig build --build-file build_standalone.zig
//   # Outputs: zig-out/lib/libproven_ffi.a

const std = @import("std");

fn attachHeader(b: *std.Build, mod: *std.Build.Module, path: []const u8) void {
    if (!std.fs.path.isAbsolute(path)) @panic("-Dheader-refc must be an absolute SDK bundle path");
    const result = std.fs.cwd().readFileAlloc(b.allocator, b.fmt("{s}/result.txt", .{path}), 16) catch
        @panic("header RefC bundle is incomplete; rebuild it with scripts/build-header-refc.sh");
    if (!std.mem.eql(u8, result, "PASS\n"))
        @panic("header RefC bundle is incomplete; rebuild it with scripts/build-header-refc.sh");
    mod.addIncludePath(.{ .cwd_relative = b.fmt("{s}/refc", .{path}) });
    mod.addIncludePath(.{ .cwd_relative = b.fmt("{s}/support", .{path}) });
    mod.addCSourceFile(.{
        .file = .{ .cwd_relative = b.fmt("{s}/kernel.c", .{path}) },
        .flags = &.{ "-std=c11", "-D_GNU_SOURCE", "-Dmain=idris_proven_codegen_main", "-fno-strict-aliasing" },
    });
    mod.addObjectFile(.{ .cwd_relative = b.fmt("{s}/libidris2_refc.a", .{path}) });
    mod.addObjectFile(.{ .cwd_relative = b.fmt("{s}/libidris2_support.a", .{path}) });
    mod.linkSystemLibrary("gmp", .{});
    mod.linkSystemLibrary("m", .{});
    mod.linkSystemLibrary("pthread", .{});
}

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const shared = b.option(bool, "shared", "Build libproven.so for C ABI consumers") orelse false;
    const header = b.option([]const u8, "header-refc", "Absolute bundle from scripts/build-header-refc.sh; absent means header operations return -99");
    if (header != null and (!target.query.isNative() or target.result.os.tag != .linux or target.result.cpu.arch != .x86_64 or target.result.abi != .gnu))
        @panic("header RefC profile currently supports native Linux x86_64 GNU only");
    const source = if (header != null) "src/main_refc.zig" else "src/main.zig";

    // -------------------------------------------------------------------------
    // Static library — libproven_ffi.a
    // Contains all C ABI exports from src/main.zig, including
    // proven_path_has_traversal and the lifecycle functions.
    // -------------------------------------------------------------------------
    const proven_mod = b.createModule(.{
        .root_source_file = b.path(source),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    const lifecycle_mod = b.createModule(.{
        .root_source_file = b.path("src/lifecycle_unavailable.zig"),
        .target = target,
        .optimize = optimize,
    });
    proven_mod.addImport("lifecycle", lifecycle_mod);
    if (header) |path| attachHeader(b, proven_mod, path);

    const lib = b.addLibrary(.{
        .name = if (shared) "proven" else "proven_ffi",
        .root_module = proven_mod,
        .linkage = if (shared) .dynamic else .static,
    });
    b.installArtifact(lib);

    // -------------------------------------------------------------------------
    // Unit tests — run with `zig build test --build-file build_standalone.zig`
    // -------------------------------------------------------------------------
    const test_mod = b.createModule(.{
        .root_source_file = b.path(source),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    test_mod.addImport("lifecycle", lifecycle_mod);
    if (header) |path| attachHeader(b, test_mod, path);
    const unit_tests = b.addTest(.{
        .root_module = test_mod,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run proven_ffi unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
