// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeVersion - Typed wrapper for semantic versioning parsing and comparison.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * The JS FFI layer provides SemVer and VersionConstraint classes
 * backed by native version parsing/comparison implementations.
 */

/** Opaque handle to a JavaScript SemVer instance. */
type semVer

/** Opaque handle to a JavaScript VersionConstraint instance. */
type versionConstraint

/** JavaScript bindings to the SafeVersion FFI wrapper. */
module SafeVersionJs = {
  @module("../../javascript/src/safe_version.js") @scope("SafeVersion")
  external parse: string => Nullable.t<semVer> = "parse"

  @module("../../javascript/src/safe_version.js") @scope("SafeVersion")
  external compare: (semVer, semVer) => int = "compare"
}

/**
 * Parse a semantic version string.
 *
 * @param version Version string (e.g. "1.2.3").
 * @returns Some(semVer) or None on parse failure.
 */
let parse = (version: string): option<semVer> => {
  SafeVersionJs.parse(version)->Nullable.toOption
}

/**
 * Compare two semantic versions.
 *
 * @param a First version.
 * @param b Second version.
 * @returns negative if a < b, 0 if equal, positive if a > b.
 */
let compare = SafeVersionJs.compare
