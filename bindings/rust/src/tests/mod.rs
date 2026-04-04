// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! CRG Grade C test suite for the proven Rust binding.
//!
//! # Test Categories
//!
//! | Category    | Module          | Purpose                                               |
//! |-------------|-----------------|-------------------------------------------------------|
//! | P2P         | p2p             | Property-based tests (proptest) for overflow safety   |
//! | E2E         | e2e             | Structural/reflexive: symbols exist, modules compile  |
//! | Aspect      | aspect          | Security boundaries: overflow inputs, FFI null safety |
//!
//! ## Note on FFI Tests
//!
//! Tests that call through the Zig/Idris2 FFI at runtime require `libproven.so`
//! to be present (built from `ffi/zig/`). Tests in this suite are marked
//! `#[cfg(feature = "ffi-runtime")]` when they require the runtime, and run
//! as structural/type tests otherwise.
//!
//! The P2P and aspect tests here exercise the *pure Rust* portion of the
//! binding: the `core` types (`Bounded`, `NonEmpty`, `Error`), the status-code
//! conversion functions, and the safe-math error-type mapping — all of which
//! are fully testable without the shared library.

#[cfg(test)]
pub mod p2p;

#[cfg(test)]
pub mod e2e;

#[cfg(test)]
pub mod aspect;
