// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeTensor - Typed wrapper for vector and matrix operations with bounds checking.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * The JS FFI layer provides Vector and Matrix classes backed by
 * native tensor implementations.
 */

/** Opaque handle to a JavaScript Vector instance. */
type vector

/** Opaque handle to a JavaScript Matrix instance. */
type matrix

/** JavaScript bindings to the SafeTensor FFI wrapper. */
module SafeTensorJs = {
  @module("../../javascript/src/safe_tensor.js")
  external createVector: array<float> => vector = "Vector"

  @module("../../javascript/src/safe_tensor.js")
  external createMatrix: (int, int) => matrix = "Matrix"
}

/**
 * Create a new Vector from an array of floats.
 *
 * @param data The vector elements.
 * @returns A new Vector handle.
 */
let createVector = SafeTensorJs.createVector

/**
 * Create a new Matrix with given dimensions.
 *
 * @param rows Number of rows.
 * @param cols Number of columns.
 * @returns A new Matrix handle.
 */
let createMatrix = SafeTensorJs.createMatrix
