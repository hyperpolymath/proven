// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeString - JNA wrapper for proven_string_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import com.sun.jna.Memory
import groovy.transform.CompileStatic

import java.nio.charset.StandardCharsets
import java.util.Optional

/**
 * Safe string operations via libproven JNA FFI.
 *
 * Provides UTF-8 validation and context-aware escaping (HTML, SQL, JS).
 * Every method delegates to the Idris 2 verified implementation.
 * No escaping logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeString {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /**
     * Validate that a byte array contains valid UTF-8.
     */
    static Optional<Boolean> isValidUtf8(byte[] data) {
        Memory mem = NativeUtil.toNativeBytes(data)
        if (mem == null) {
            return Optional.empty()
        }
        NativeUtil.extractBool(
            LIB.proven_string_is_valid_utf8(mem, data.length))
    }

    /**
     * Escape a string for safe HTML insertion.
     */
    static Optional<String> escapeHtml(String input) {
        Memory mem = NativeUtil.toNativeString(input)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = input.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractString(
            LIB.proven_string_escape_html(mem, bytes.length))
    }

    /**
     * Escape a string for safe SQL insertion.
     */
    static Optional<String> escapeSql(String input) {
        Memory mem = NativeUtil.toNativeString(input)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = input.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractString(
            LIB.proven_string_escape_sql(mem, bytes.length))
    }

    /**
     * Escape a string for safe JavaScript string context.
     */
    static Optional<String> escapeJs(String input) {
        Memory mem = NativeUtil.toNativeString(input)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = input.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractString(
            LIB.proven_string_escape_js(mem, bytes.length))
    }
}
