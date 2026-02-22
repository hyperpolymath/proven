// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeUnit - Typed re-exports from the JavaScript FFI binding.
 * Delegates all computation to libproven via the JavaScript FFI layer.
 * @module
 */

export {
  SafeUnit,
  LengthUnit,
  Length,
  MassUnit,
  Mass,
  TemperatureUnit,
  Temperature,
  TimeUnit,
  TimeValue,
  DataUnit,
  DataSize,
} from '../../javascript/src/safe_unit.js';
