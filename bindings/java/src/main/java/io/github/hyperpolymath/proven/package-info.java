// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Java JNA binding for libproven -- formally verified safety primitives.
 *
 * <p>This package is a <b>thin FFI wrapper</b> over the libproven C ABI.
 * All computation is performed in the Idris 2 core (with dependent types
 * and totality checking) via the Zig FFI bridge. No logic is reimplemented
 * in Java.</p>
 *
 * <h2>Architecture</h2>
 * <pre>
 *   Java (this package)
 *     -- JNA --&gt;
 *   libproven.so (Zig C ABI)
 *     -- RefC --&gt;
 *   Idris 2 verified core
 * </pre>
 *
 * <h2>Quick Start</h2>
 * <pre>{@code
 * import io.github.hyperpolymath.proven.*;
 *
 * Proven.init();
 * try {
 *     // Safe arithmetic (returns Optional, never throws)
 *     Optional<Long> sum = SafeMath.add(Long.MAX_VALUE, 1);
 *     // sum.isEmpty() == true (overflow)
 *
 *     // String escaping
 *     Optional<String> safe = SafeString.escapeHtml("<script>alert('xss')</script>");
 *
 *     // Path traversal detection
 *     Optional<Boolean> dangerous = SafePath.hasTraversal("../../etc/passwd");
 * } finally {
 *     Proven.deinit();
 * }
 * }</pre>
 *
 * <h2>Error Handling</h2>
 * <p>All fallible operations return {@link java.util.Optional}. Domain errors
 * (overflow, division by zero, invalid input) are represented as empty Optionals.
 * This binding never throws exceptions for domain errors.</p>
 *
 * <h2>Memory Management</h2>
 * <p>Strings allocated by libproven are freed automatically by this binding's
 * internal utilities. Users do not need to manage native memory.</p>
 *
 * @see Proven Entry point and lifecycle management
 * @see SafeMath Overflow-checked arithmetic
 * @see SafeString Injection-safe string escaping
 * @see SafeCrypto Constant-time comparison and secure random
 * @see SafePath Path traversal detection
 */
package io.github.hyperpolymath.proven;
