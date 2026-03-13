// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Build configuration for Proven BEAM NIF
//
// Links against libproven.a (the proven C ABI) and erl_nif.h to produce
// libproven_nif.so for Erlang/Gleam consumption.

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // ================================================================
    // Proven FFI module (the C ABI library)
    // ================================================================

    const proven_ffi_dep = b.dependency("proven_ffi", .{
        .target = target,
        .optimize = optimize,
    });
    const proven_mod = proven_ffi_dep.module("proven");

    // ================================================================
    // Erlang NIF headers
    // ================================================================

    const erl_include = blk: {
        if (std.process.getEnvVarOwned(b.allocator, "ERL_INCLUDE_PATH")) |path| {
            break :blk path;
        } else |_| {}

        const result = std.process.Child.run(.{
            .allocator = b.allocator,
            .argv = &.{ "erl", "-noshell", "-eval", "io:format(\"~s\", [code:root_dir()])", "-s", "init", "stop" },
        }) catch {
            @panic("Failed to find Erlang installation. Set ERL_INCLUDE_PATH.");
        };

        const root_dir = std.mem.trim(u8, result.stdout, &std.ascii.whitespace);
        break :blk b.fmt("{s}/usr/include", .{root_dir});
    };

    // ================================================================
    // NIF shared library
    // ================================================================

    const lib = b.addLibrary(.{
        .name = "proven_nif",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/proven_nif.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "proven", .module = proven_mod },
            },
        }),
        .linkage = .dynamic,
    });

    // Erlang NIF headers
    lib.root_module.addIncludePath(.{ .cwd_relative = erl_include });

    // System libraries required by proven (RefC runtime dependencies)
    lib.linkLibC();
    lib.root_module.linkSystemLibrary("gmp", .{});

    // Install to priv/ for BEAM to find
    const install = b.addInstallArtifact(lib, .{
        .dest_dir = .{ .override = .{ .custom = "../priv" } },
    });

    b.getInstallStep().dependOn(&install.step);

    // ================================================================
    // Tests
    // ================================================================

    const unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/proven_nif.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "proven", .module = proven_mod },
            },
        }),
    });

    unit_tests.root_module.addIncludePath(.{ .cwd_relative = erl_include });

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
