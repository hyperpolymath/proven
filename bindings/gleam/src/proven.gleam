// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// Proven - Code that cannot crash
////
//// A verified safety library providing:
//// - SafeMath: Arithmetic without overflow/underflow/division-by-zero
//// - SafeString: UTF-8 and escaping without exceptions
//// - SafeUrl: URL parsing without malformed URL crashes
//// - SafeEmail: Validation without regex catastrophic backtracking
//// - SafePath: Filesystem ops without traversal attacks
//// - SafeCrypto: Cryptographic primitives done right
//// - SafeNetwork: Network operations that cannot fail unsafely

pub const version = "0.3.0"
