// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Result type for proven bindings.
 *
 * Matches the JavaScript { ok: boolean, value?: T, error?: string } pattern
 * used by the JS FFI layer. Converts to/from ReScript's native result type.
 */

/** JavaScript-side result shape returned by FFI wrappers. */
type jsResult<'value> = {
  ok: bool,
  value?: 'value,
  error?: string,
}

/**
 * Convert a JavaScript FFI result to a ReScript result.
 *
 * Uses safe pattern matching -- never throws.
 *
 * @param jsResult The JS result object from the FFI layer.
 * @returns Ok(value) on success, Error(message) on failure.
 */
let fromJs = (jsResult: jsResult<'value>): result<'value, string> => {
  if jsResult.ok {
    switch jsResult.value {
    | Some(v) => Ok(v)
    | None => Error("Ok result missing value")
    }
  } else {
    switch jsResult.error {
    | Some(e) => Error(e)
    | None => Error("Unknown error")
    }
  }
}

/**
 * Convert a ReScript result to a JavaScript result.
 *
 * @param result The ReScript result to convert.
 * @returns A JS-compatible result object.
 */
let toJs = (result: result<'value, string>): jsResult<'value> => {
  switch result {
  | Ok(value) => {ok: true, value: Some(value), error: None}
  | Error(error) => {ok: false, value: None, error: Some(error)}
  }
}
