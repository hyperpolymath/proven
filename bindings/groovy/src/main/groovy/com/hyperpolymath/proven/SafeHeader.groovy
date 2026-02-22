// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeHeader - JNA wrapper for proven_header_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import com.sun.jna.Memory
import groovy.transform.CompileStatic

import java.nio.charset.StandardCharsets
import java.util.Optional

/**
 * Safe HTTP header operations via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation.
 * No header logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeHeader {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /** Check if a header value contains CRLF injection sequences. */
    static Optional<Boolean> hasCrlf(String value) {
        Memory mem = NativeUtil.toNativeString(value)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = value.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractBool(
            LIB.proven_header_has_crlf(mem, bytes.length))
    }

    /** Check if a header name is valid per HTTP spec. */
    static Optional<Boolean> isValidName(String name) {
        Memory mem = NativeUtil.toNativeString(name)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = name.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractBool(
            LIB.proven_header_is_valid_name(mem, bytes.length))
    }

    /** Check if a header name is security-dangerous. */
    static Optional<Boolean> isDangerous(String name) {
        Memory mem = NativeUtil.toNativeString(name)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = name.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractBool(
            LIB.proven_header_is_dangerous(mem, bytes.length))
    }

    /** Render a header name-value pair as a proper HTTP header line. */
    static Optional<String> render(String name, String value) {
        if (name == null || value == null) {
            return Optional.empty()
        }
        byte[] nameBytes = name.getBytes(StandardCharsets.UTF_8)
        byte[] valueBytes = value.getBytes(StandardCharsets.UTF_8)
        Memory nameMem = NativeUtil.toNativeBytes(nameBytes)
        Memory valueMem = NativeUtil.toNativeBytes(valueBytes)
        if (nameMem == null || valueMem == null) {
            return Optional.empty()
        }
        NativeUtil.extractString(
            LIB.proven_header_render(nameMem, nameBytes.length, valueMem, valueBytes.length))
    }

    /** Build an HSTS header value. */
    static Optional<String> buildHsts(long maxAge, boolean includeSubdomains, boolean preload) {
        NativeUtil.extractString(
            LIB.proven_header_build_hsts(maxAge, includeSubdomains, preload))
    }
}
