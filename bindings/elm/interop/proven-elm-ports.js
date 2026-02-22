// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven Safety Library - Elm port interop
//
// This file wires Elm outgoing ports to the JavaScript FFI binding
// (bindings/javascript/src/ffi.js) which loads libproven. Results
// are sent back through Elm incoming ports.
//
// ALL computation delegates to libproven (Idris 2 + Zig).
// This file ONLY performs data marshaling between Elm and the JS FFI.
//
// Usage:
//   import { setupProvenPorts } from './proven-elm-ports.js';
//   const app = Elm.Main.init({ node: document.getElementById('app') });
//   setupProvenPorts(app);

import { getLib, ProvenStatus, encodeString, readAndFreeString } from '../../javascript/src/ffi.js';

/**
 * Wire all Proven ports between Elm app and libproven FFI.
 *
 * @param {object} app - The Elm application instance (from Elm.Main.init).
 */
export function setupProvenPorts(app) {
  const lib = getLib();

  // -----------------------------------------------------------------------
  // Helper: send result back to Elm
  // -----------------------------------------------------------------------

  function sendIntResult(id, status, value) {
    app.ports.receiveIntResult.send({ id, status, value: Number(value) });
  }

  function sendBoolResult(id, status, value) {
    app.ports.receiveBoolResult.send({ id, status, value });
  }

  function sendStringResult(id, status, value) {
    app.ports.receiveStringResult.send({ id, status, value: value || '' });
  }

  function sendFloatResult(id, status, value) {
    app.ports.receiveFloatResult.send({ id, status, value });
  }

  // -----------------------------------------------------------------------
  // SafeMath
  // -----------------------------------------------------------------------

  app.ports.requestMathDiv.subscribe(({ id, a, b }) => {
    const r = lib.proven_math_div(BigInt(a), BigInt(b));
    sendIntResult(id, r[0], r[1]);
  });

  app.ports.requestMathMod.subscribe(({ id, a, b }) => {
    const r = lib.proven_math_mod(BigInt(a), BigInt(b));
    sendIntResult(id, r[0], r[1]);
  });

  app.ports.requestMathAdd.subscribe(({ id, a, b }) => {
    const r = lib.proven_math_add_checked(BigInt(a), BigInt(b));
    sendIntResult(id, r[0], r[1]);
  });

  app.ports.requestMathSub.subscribe(({ id, a, b }) => {
    const r = lib.proven_math_sub_checked(BigInt(a), BigInt(b));
    sendIntResult(id, r[0], r[1]);
  });

  app.ports.requestMathMul.subscribe(({ id, a, b }) => {
    const r = lib.proven_math_mul_checked(BigInt(a), BigInt(b));
    sendIntResult(id, r[0], r[1]);
  });

  app.ports.requestMathAbs.subscribe(({ id, n }) => {
    const r = lib.proven_math_abs_safe(BigInt(n));
    sendIntResult(id, r[0], r[1]);
  });

  app.ports.requestMathClamp.subscribe(({ id, lo, hi, value }) => {
    const r = lib.proven_math_clamp(BigInt(lo), BigInt(hi), BigInt(value));
    sendIntResult(id, 0, r);
  });

  // -----------------------------------------------------------------------
  // SafeString
  // -----------------------------------------------------------------------

  app.ports.requestStringIsValidUtf8.subscribe(({ id, input }) => {
    const buf = encodeString(input);
    const r = lib.proven_string_is_valid_utf8(buf, buf.length - 1);
    sendBoolResult(id, r[0], r[1]);
  });

  app.ports.requestStringEscapeHtml.subscribe(({ id, input }) => {
    const buf = encodeString(input);
    const r = lib.proven_string_escape_html(buf, buf.length - 1);
    const str = r[0] === ProvenStatus.OK ? readAndFreeString(r[1], Number(r[2])) : '';
    sendStringResult(id, r[0], str);
  });

  app.ports.requestStringEscapeSql.subscribe(({ id, input }) => {
    const buf = encodeString(input);
    const r = lib.proven_string_escape_sql(buf, buf.length - 1);
    const str = r[0] === ProvenStatus.OK ? readAndFreeString(r[1], Number(r[2])) : '';
    sendStringResult(id, r[0], str);
  });

  app.ports.requestStringEscapeJs.subscribe(({ id, input }) => {
    const buf = encodeString(input);
    const r = lib.proven_string_escape_js(buf, buf.length - 1);
    const str = r[0] === ProvenStatus.OK ? readAndFreeString(r[1], Number(r[2])) : '';
    sendStringResult(id, r[0], str);
  });

  // -----------------------------------------------------------------------
  // SafePath
  // -----------------------------------------------------------------------

  app.ports.requestPathHasTraversal.subscribe(({ id, path }) => {
    const buf = encodeString(path);
    const r = lib.proven_path_has_traversal(buf, buf.length - 1);
    sendBoolResult(id, r[0], r[1]);
  });

  app.ports.requestPathSanitizeFilename.subscribe(({ id, filename }) => {
    const buf = encodeString(filename);
    const r = lib.proven_path_sanitize_filename(buf, buf.length - 1);
    const str = r[0] === ProvenStatus.OK ? readAndFreeString(r[1], Number(r[2])) : '';
    sendStringResult(id, r[0], str);
  });

  // -----------------------------------------------------------------------
  // SafeCrypto
  // -----------------------------------------------------------------------

  app.ports.requestCryptoConstantTimeEq.subscribe(({ id, a, b }) => {
    const bufA = encodeString(a);
    const bufB = encodeString(b);
    const r = lib.proven_crypto_constant_time_eq(bufA, bufA.length - 1, bufB, bufB.length - 1);
    sendBoolResult(id, r[0], r[1]);
  });

  // -----------------------------------------------------------------------
  // SafeEmail
  // -----------------------------------------------------------------------

  app.ports.requestEmailIsValid.subscribe(({ id, email }) => {
    const buf = encodeString(email);
    const r = lib.proven_email_is_valid(buf, buf.length - 1);
    sendBoolResult(id, r[0], r[1]);
  });

  // -----------------------------------------------------------------------
  // SafeJson
  // -----------------------------------------------------------------------

  app.ports.requestJsonIsValid.subscribe(({ id, json }) => {
    const buf = encodeString(json);
    const r = lib.proven_json_is_valid(buf, buf.length - 1);
    sendBoolResult(id, r[0], r[1]);
  });

  app.ports.requestJsonGetType.subscribe(({ id, json }) => {
    const buf = encodeString(json);
    const r = lib.proven_json_get_type(buf, buf.length - 1);
    sendIntResult(id, 0, r);
  });

  // -----------------------------------------------------------------------
  // SafeFloat
  // -----------------------------------------------------------------------

  app.ports.requestFloatDiv.subscribe(({ id, a, b }) => {
    const r = lib.proven_float_div(a, b);
    sendFloatResult(id, r[0], r[1]);
  });

  app.ports.requestFloatSqrt.subscribe(({ id, x }) => {
    const r = lib.proven_float_sqrt(x);
    sendFloatResult(id, r[0], r[1]);
  });

  app.ports.requestFloatLn.subscribe(({ id, x }) => {
    const r = lib.proven_float_ln(x);
    sendFloatResult(id, r[0], r[1]);
  });

  // -----------------------------------------------------------------------
  // SafeDateTime
  // -----------------------------------------------------------------------

  app.ports.requestDatetimeIsLeapYear.subscribe(({ id, year }) => {
    const r = lib.proven_datetime_is_leap_year(year);
    sendBoolResult(id, 0, r);
  });

  // -----------------------------------------------------------------------
  // SafeCalculator
  // -----------------------------------------------------------------------

  app.ports.requestCalculatorEval.subscribe(({ id, expr }) => {
    const buf = encodeString(expr);
    const r = lib.proven_calculator_eval(buf, buf.length - 1);
    sendFloatResult(id, r[0], r[1]);
  });
}
