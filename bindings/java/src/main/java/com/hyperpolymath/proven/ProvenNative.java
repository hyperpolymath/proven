// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.hyperpolymath.proven;

/**
 * JNI bridge to Idris 2 verified code via Zig ABI.
 *
 * The native library is built from ffi/zig and provides:
 * - Formally verified safety primitives
 * - Overflow-checked arithmetic
 * - Injection-safe string operations
 * - Cryptographic primitives
 */
public final class ProvenNative {
    private static boolean loaded = false;

    static {
        try {
            System.loadLibrary("proven_zig");
            loaded = true;
        } catch (UnsatisfiedLinkError e) {
            // Fall back to pure Java implementations
            loaded = false;
        }
    }

    /**
     * Check if native library is loaded.
     */
    public static boolean isNativeLoaded() {
        return loaded;
    }

    // Native method declarations - these call into Zig FFI

    // SafeMath natives
    public static native long nativeAddSafe(long a, long b, long[] overflow);
    public static native long nativeSubSafe(long a, long b, long[] overflow);
    public static native long nativeMulSafe(long a, long b, long[] overflow);
    public static native long nativeDivSafe(long a, long b, long[] error);

    // SafeString natives
    public static native boolean nativeIsValidUtf8(byte[] data);
    public static native byte[] nativeEscapeHtml(byte[] data);
    public static native byte[] nativeEscapeSql(byte[] data);
    public static native byte[] nativeEscapeUrl(byte[] data);

    // SafeCrypto natives
    public static native byte[] nativeSha3_256(byte[] data);
    public static native byte[] nativeBlake3(byte[] data);
    public static native byte[] nativeHmacSha3(byte[] key, byte[] data);
    public static native boolean nativeConstantTimeCompare(byte[] a, byte[] b);
    public static native byte[] nativeSecureRandom(int length);

    // SafeEmail natives
    public static native boolean nativeValidateEmail(byte[] email);

    // SafePath natives
    public static native boolean nativeHasTraversal(byte[] path);
    public static native byte[] nativeNormalizePath(byte[] path);

    // SafeNetwork natives
    public static native boolean nativeValidateIpv4(byte[] addr);
    public static native boolean nativeValidateIpv6(byte[] addr);
    public static native boolean nativeValidateCidr(byte[] cidr);

    // SafeUUID natives
    public static native byte[] nativeGenerateUuidV4();
    public static native boolean nativeValidateUuid(byte[] uuid);

    // SafeJson natives
    public static native byte[] nativeParseJson(byte[] json);
    public static native boolean nativeValidateJson(byte[] json);
}
