// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeNetwork - JNA wrapper for proven_network_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import com.sun.jna.Memory
import groovy.transform.CompileStatic

import java.nio.charset.StandardCharsets
import java.util.Optional

/**
 * Safe network operations via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation.
 * No network logic is reimplemented in Groovy.
 *
 * Note: IPv4 parse/classify operations require the IPv4Result/IPv4Address
 * structs. The basic parse operation returns a status code and octets
 * which are extracted from raw memory.
 */
@CompileStatic
class SafeNetwork {

    // Network functions that require complex struct mapping are left for
    // future work when IPv4Result/IPv4Address JNA structures are defined.
    // The Java binding also notes this limitation.
    //
    // Available operations via the simple function signatures are limited;
    // the full IPv4 API will be available when JNA struct mapping is added.
}
