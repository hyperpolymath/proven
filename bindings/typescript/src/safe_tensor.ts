// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeTensor - Typed re-exports from the JavaScript FFI binding.
 * Delegates all computation to libproven via the JavaScript FFI layer.
 * @module
 */

export { Vector, Matrix } from '../../javascript/src/safe_tensor.js';
export const SafeTensor = { Vector: null, Matrix: null };
