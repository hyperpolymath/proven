// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Proven - Code that cannot crash
 *
 * A verified safety library providing:
 * - SafeMath: Arithmetic without overflow/underflow/division-by-zero
 * - SafeString: UTF-8 and escaping without exceptions
 * - SafeUrl: URL parsing without malformed URL crashes
 * - SafeEmail: Validation without regex catastrophic backtracking
 * - SafePath: Filesystem ops without traversal attacks
 * - SafeCrypto: Cryptographic primitives done right
 * - SafeNetwork: Network operations that cannot fail unsafely
 */

export { SafeMath } from './safe_math.js';
export { SafeString } from './safe_string.js';
export { SafeUrl, type ParsedUrl } from './safe_url.js';
export { SafeEmail } from './safe_email.js';
export { SafePath } from './safe_path.js';
export { SafeCrypto } from './safe_crypto.js';
export { SafeNetwork, type IPv4 } from './safe_network.js';
export { ProvenError, type ProvenStatus } from './error.js';
export { init, isInitialized } from './wasm.js';

export const VERSION = '0.3.0';
