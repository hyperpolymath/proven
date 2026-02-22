// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCookie - JNA wrapper for proven_cookie_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import com.sun.jna.Memory
import groovy.transform.CompileStatic

import java.nio.charset.StandardCharsets
import java.util.Optional

/**
 * Safe HTTP cookie operations via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation.
 * No cookie logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeCookie {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /** Check if a cookie value contains injection characters. */
    static Optional<Boolean> hasInjection(String value) {
        Memory mem = NativeUtil.toNativeString(value)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = value.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractBool(
            LIB.proven_cookie_has_injection(mem, bytes.length))
    }

    /** Validate a cookie name. */
    static Optional<Boolean> validateName(String name) {
        Memory mem = NativeUtil.toNativeString(name)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = name.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractBool(
            LIB.proven_cookie_validate_name(mem, bytes.length))
    }

    /** Validate a cookie value. */
    static Optional<Boolean> validateValue(String value) {
        Memory mem = NativeUtil.toNativeString(value)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = value.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractBool(
            LIB.proven_cookie_validate_value(mem, bytes.length))
    }

    /** Get the cookie prefix type. */
    static Optional<Long> getPrefix(String name) {
        Memory mem = NativeUtil.toNativeString(name)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = name.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractLong(
            LIB.proven_cookie_get_prefix(mem, bytes.length))
    }
}
