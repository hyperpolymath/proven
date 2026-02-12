// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Proven_Bitwise - Bitwise operations for ReScript.
 *
 * ReScript does not have built-in bitwise operators like OCaml's lsl, lsr, land, lor, etc.
 * This module provides these operations using JavaScript's native bitwise operators.
 *
 * Operations:
 * - lsl (logical shift left)
 * - lsr (logical shift right)
 * - asr (arithmetic shift right)
 * - land (bitwise AND)
 * - lor (bitwise OR)
 * - lxor (bitwise XOR)
 * - lnot (bitwise NOT)
 */

/** Logical shift left: a << b */
let lsl = %raw(`function(a, b) { return a << b; }`)

/** Logical shift right (unsigned): a >>> b */
let lsr = %raw(`function(a, b) { return a >>> b; }`)

/** Arithmetic shift right (signed): a >> b */
let asr = %raw(`function(a, b) { return a >> b; }`)

/** Bitwise AND: a & b */
let land = %raw(`function(a, b) { return a & b; }`)

/** Bitwise OR: a | b */
let lor = %raw(`function(a, b) { return a | b; }`)

/** Bitwise XOR: a ^ b */
let lxor = %raw(`function(a, b) { return a ^ b; }`)

/** Bitwise NOT: ~a */
let lnot = %raw(`function(a) { return ~a; }`)
