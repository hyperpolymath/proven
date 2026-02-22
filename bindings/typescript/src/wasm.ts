// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * FFI bridge for TypeScript -- delegates to the JavaScript FFI layer.
 *
 * TypeScript does NOT load libproven directly. Instead, it re-exports
 * the loader functions from the JavaScript binding's ffi.js, which handles
 * Deno.dlopen / library loading and lifetime management.
 *
 * @module
 */

import {
  getLib as _getLib,
  isAvailable as _isAvailable,
  getLoadError as _getLoadError,
  close as _close,
  encodeString as _encodeString,
  readAndFreeString as _readAndFreeString,
  readString as _readString,
  ProvenStatus as _ProvenStatus,
  statusToError as _statusToError,
} from '../../javascript/src/ffi.js';

import {
  ok as _ok,
  err as _err,
  isOk as _isOk,
  isErr as _isErr,
  unwrap as _unwrap,
  unwrapOr as _unwrapOr,
} from '../../javascript/src/result.js';

// Re-export FFI utilities
export const getLib = _getLib;
export const isAvailable = _isAvailable;
export const getLoadError = _getLoadError;
export const close = _close;
export const encodeString = _encodeString;
export const readAndFreeString = _readAndFreeString;
export const readString = _readString;
export const ProvenStatus = _ProvenStatus;
export const statusToError = _statusToError;

// Re-export result utilities
export const ok = _ok;
export const err = _err;
export const isOk = _isOk;
export const isErr = _isErr;
export const unwrap = _unwrap;
export const unwrapOr = _unwrapOr;

/**
 * Initialize the Proven library.
 *
 * The JavaScript FFI layer initializes lazily on first call to `getLib()`.
 * Call this explicitly if you want to detect load errors early.
 *
 * @returns true if the library is available.
 */
export function init(): boolean {
  return _isAvailable();
}

/**
 * Check if the native library is loaded and initialized.
 *
 * @returns true if libproven is loaded and ready.
 */
export function isInitialized(): boolean {
  return _isAvailable();
}
