# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

using Test
using Proven

@testset "Proven.jl FFI Bindings" begin

    @testset "Lifecycle" begin
        @test Lifecycle.init() == true
        @test Lifecycle.is_initialized() == true
    end

    @testset "Version" begin
        @test Version.major() >= 0
        @test Version.minor() >= 0
        @test Version.patch() >= 0
        @test Version.module_count() > 0
    end

    @testset "SafeMath" begin
        @test SafeMath.div(Int64(10), Int64(2)) == 5
        @test SafeMath.div(Int64(10), Int64(0)) === nothing

        @test SafeMath.mod(Int64(10), Int64(3)) == 1
        @test SafeMath.mod(Int64(10), Int64(0)) === nothing

        @test SafeMath.add_checked(Int64(1), Int64(2)) == 3
        @test SafeMath.add_checked(typemax(Int64), Int64(1)) === nothing

        @test SafeMath.sub_checked(Int64(5), Int64(3)) == 2
        @test SafeMath.mul_checked(Int64(3), Int64(4)) == 12
        @test SafeMath.mul_checked(typemax(Int64), Int64(2)) === nothing

        @test SafeMath.abs_safe(Int64(-5)) == 5
        @test SafeMath.abs_safe(typemin(Int64)) === nothing

        @test SafeMath.clamp(Int64(0), Int64(10), Int64(5)) == 5
        @test SafeMath.clamp(Int64(0), Int64(10), Int64(-1)) == 0
        @test SafeMath.clamp(Int64(0), Int64(10), Int64(15)) == 10
    end

    @testset "SafeString" begin
        @test SafeString.escape_html("<script>") !== nothing
        @test SafeString.escape_sql("it's") !== nothing
        @test SafeString.escape_js("line\nbreak") !== nothing
    end

    @testset "SafePath" begin
        @test SafePath.has_traversal("../etc/passwd") == true
        @test SafePath.has_traversal("safe/path") == false
        @test SafePath.sanitize_filename("file<>name") !== nothing
    end

    @testset "SafeEmail" begin
        @test SafeEmail.is_valid("user@example.com") == true
        @test SafeEmail.is_valid("not-an-email") == false
    end

    @testset "SafeFloat" begin
        @test SafeFloat.div(10.0, 2.0) == 5.0
        @test SafeFloat.div(10.0, 0.0) === nothing
        @test SafeFloat.is_finite(1.0) == true
        @test SafeFloat.is_nan(NaN) == true
        @test SafeFloat.sqrt(4.0) == 2.0
        @test SafeFloat.sqrt(-1.0) === nothing
        @test SafeFloat.ln(1.0) == 0.0
        @test SafeFloat.ln(-1.0) === nothing
    end

    @testset "SafeCrypto" begin
        a = Vector{UInt8}("secret")
        b = Vector{UInt8}("secret")
        c = Vector{UInt8}("other!")
        @test SafeCrypto.constant_time_eq(a, b) == true
        @test SafeCrypto.constant_time_eq(a, c) == false

        buf = zeros(UInt8, 32)
        @test SafeCrypto.random_bytes!(buf) == true
    end

    @testset "SafeAngle" begin
        @test SafeAngle.deg_to_rad(180.0) ≈ π
        @test SafeAngle.rad_to_deg(π) ≈ 180.0
        @test SafeAngle.normalize_degrees(370.0) ≈ 10.0
    end

    @testset "SafeProbability" begin
        @test SafeProbability.create(0.5) == 0.5
        @test SafeProbability.create(1.5) == 1.0
        @test SafeProbability.create(-0.5) == 0.0
        @test SafeProbability.not(0.3) ≈ 0.7
    end

    @testset "Lifecycle cleanup" begin
        Lifecycle.deinit()
        @test true  # If we got here, cleanup did not crash
    end
end
