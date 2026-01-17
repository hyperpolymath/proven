// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.proven;

/**
 * JNI bridge to Proven library via C ABI.
 *
 * The native library is built from Zig and provides:
 * - Formally verified safety primitives
 * - Overflow-checked arithmetic
 * - Injection-safe string operations
 * - Cryptographic primitives
 * - And 38 safety modules
 */
public final class ProvenNative {
    private static boolean loaded = false;

    static {
        try {
            System.loadLibrary("proven");
            loaded = true;
        } catch (UnsatisfiedLinkError e) {
            // Fall back to pure Java implementations
            loaded = false;
        }
    }

    private ProvenNative() {}

    /**
     * Check if native library is loaded.
     */
    public static boolean isNativeLoaded() {
        return loaded;
    }

    // ============================================================================
    // Runtime Management
    // ============================================================================

    public static native int nativeInit();
    public static native void nativeDeinit();
    public static native boolean nativeIsInitialized();
    public static native int nativeFfiAbiVersion();

    // ============================================================================
    // SafeMath - Arithmetic operations
    // ============================================================================

    public static native long nativeMathAdd(long a, long b, int[] status);
    public static native long nativeMathSub(long a, long b, int[] status);
    public static native long nativeMathMul(long a, long b, int[] status);
    public static native long nativeMathDiv(long a, long b, int[] status);
    public static native long nativeMathMod(long a, long b, int[] status);
    public static native long nativeMathAbs(long n, int[] status);
    public static native long nativeMathClamp(long lo, long hi, long value);
    public static native long nativeMathPow(long base, int exp, int[] status);

    // ============================================================================
    // SafeString - Text operations
    // ============================================================================

    public static native boolean nativeStringIsValidUtf8(byte[] data);
    public static native byte[] nativeStringEscapeSql(byte[] data, int[] status);
    public static native byte[] nativeStringEscapeHtml(byte[] data, int[] status);
    public static native byte[] nativeStringEscapeJs(byte[] data, int[] status);
    public static native byte[] nativeStringEscapeUrl(byte[] data, int[] status);

    // ============================================================================
    // SafePath - Filesystem operations
    // ============================================================================

    public static native boolean nativePathHasTraversal(byte[] path);
    public static native byte[] nativePathSanitizeFilename(byte[] path, int[] status);
    public static native byte[] nativePathNormalize(byte[] path, int[] status);

    // ============================================================================
    // SafeEmail - Email validation
    // ============================================================================

    public static native boolean nativeEmailIsValid(byte[] email);
    public static native byte[] nativeEmailNormalize(byte[] email, int[] status);

    // ============================================================================
    // SafeUrl - URL parsing
    // ============================================================================

    public static native boolean nativeUrlIsValid(byte[] url);
    public static native byte[] nativeUrlParse(byte[] url, int[] status);
    public static native byte[] nativeUrlEncode(byte[] data, int[] status);
    public static native byte[] nativeUrlDecode(byte[] data, int[] status);

    // ============================================================================
    // SafeNetwork - IP address parsing
    // ============================================================================

    public static native boolean nativeNetworkIsValidIpv4(byte[] addr);
    public static native boolean nativeNetworkIsValidIpv6(byte[] addr);
    public static native boolean nativeNetworkIsValidCidr(byte[] cidr);
    public static native boolean nativeNetworkIpv4IsPrivate(byte[] octets);
    public static native boolean nativeNetworkIpv4IsLoopback(byte[] octets);
    public static native byte[] nativeNetworkParseIpv4(byte[] addr, int[] status);

    // ============================================================================
    // SafeCrypto - Cryptographic operations
    // ============================================================================

    public static native byte[] nativeCryptoSha256(byte[] data, int[] status);
    public static native byte[] nativeCryptoSha3_256(byte[] data, int[] status);
    public static native byte[] nativeCryptoBlake3(byte[] data, int[] status);
    public static native byte[] nativeCryptoHmacSha256(byte[] key, byte[] data, int[] status);
    public static native boolean nativeCryptoConstantTimeEq(byte[] a, byte[] b);
    public static native byte[] nativeCryptoRandomBytes(int length, int[] status);

    // ============================================================================
    // SafeUuid - UUID generation
    // ============================================================================

    public static native byte[] nativeUuidV4Generate(int[] status);
    public static native boolean nativeUuidIsValid(byte[] uuid);
    public static native byte[] nativeUuidParse(byte[] uuidStr, int[] status);
    public static native byte[] nativeUuidToString(byte[] uuidBytes, int[] status);
    public static native int nativeUuidGetVersion(byte[] uuidBytes);

    // ============================================================================
    // SafeCurrency - Currency operations
    // ============================================================================

    public static native int nativeCurrencyParseCode(byte[] code);
    public static native boolean nativeCurrencyIsValidCode(byte[] code);
    public static native byte[] nativeCurrencyGetSymbol(int code);
    public static native byte[] nativeCurrencyGetName(int code);
    public static native int nativeCurrencyGetDecimals(int code);
    public static native long nativeMoneyAdd(long a, long b, int currency, int[] status);
    public static native long nativeMoneySub(long a, long b, int currency, int[] status);
    public static native long nativeMoneyMul(long amount, long scalar, int currency, int[] status);
    public static native long nativeMoneyDiv(long amount, long divisor, int currency, int[] status);

    // ============================================================================
    // SafePhone - Phone number validation
    // ============================================================================

    public static native boolean nativePhoneIsValid(byte[] phone);
    public static native byte[] nativePhoneParse(byte[] phone, int[] status);
    public static native byte[] nativePhoneFormatE164(byte[] phone, int country, int[] status);
    public static native byte[] nativePhoneFormatInternational(byte[] phone, int country, int[] status);
    public static native int nativePhoneGetCountryCode(byte[] phone);

    // ============================================================================
    // SafeHex - Hexadecimal encoding
    // ============================================================================

    public static native byte[] nativeHexEncode(byte[] data, boolean uppercase, int[] status);
    public static native byte[] nativeHexDecode(byte[] hex, int[] status);
    public static native boolean nativeHexIsValid(byte[] hex);
    public static native boolean nativeHexConstantTimeEq(byte[] a, byte[] b);

    // ============================================================================
    // SafeJson - JSON operations
    // ============================================================================

    public static native boolean nativeJsonIsValid(byte[] json);
    public static native byte[] nativeJsonParse(byte[] json, int[] status);
    public static native byte[] nativeJsonStringify(byte[] data, int[] status);
    public static native byte[] nativeJsonEscape(byte[] str, int[] status);

    // ============================================================================
    // SafeDateTime - Date/time operations
    // ============================================================================

    public static native boolean nativeDateTimeIsValidIso8601(byte[] datetime);
    public static native long nativeDateTimeParse(byte[] datetime, int[] status);
    public static native byte[] nativeDateTimeFormat(long timestamp, byte[] format, int[] status);
    public static native long nativeDateTimeAddDays(long timestamp, int days, int[] status);
    public static native long nativeDateTimeAddHours(long timestamp, int hours, int[] status);

    // ============================================================================
    // SafeFloat - Floating point operations
    // ============================================================================

    public static native boolean nativeFloatIsFinite(double value);
    public static native double nativeFloatClamp(double value, double min, double max);
    public static native boolean nativeFloatApproxEq(double a, double b, double epsilon);
    public static native double nativeFloatSafeDiv(double a, double b, int[] status);

    // ============================================================================
    // SafeVersion - Semantic versioning
    // ============================================================================

    public static native boolean nativeVersionIsValid(byte[] version);
    public static native int nativeVersionCompare(byte[] a, byte[] b);
    public static native byte[] nativeVersionParse(byte[] version, int[] status);
    public static native boolean nativeVersionSatisfies(byte[] version, byte[] range);

    // ============================================================================
    // SafeColor - Color operations
    // ============================================================================

    public static native boolean nativeColorIsValidHex(byte[] hex);
    public static native int nativeColorParseHex(byte[] hex, int[] status);
    public static native byte[] nativeColorToHex(int rgba, int[] status);
    public static native int nativeColorBlend(int c1, int c2, double ratio);

    // ============================================================================
    // SafeAngle - Angle operations
    // ============================================================================

    public static native double nativeAngleNormalizeDegrees(double degrees);
    public static native double nativeAngleNormalizeRadians(double radians);
    public static native double nativeAngleDegreesToRadians(double degrees);
    public static native double nativeAngleRadiansToDegrees(double radians);

    // ============================================================================
    // SafeUnit - Unit conversions
    // ============================================================================

    public static native double nativeUnitConvert(double value, int fromUnit, int toUnit, int[] status);
    public static native boolean nativeUnitIsCompatible(int unit1, int unit2);

    // ============================================================================
    // SafeBuffer - Buffer operations
    // ============================================================================

    public static native long nativeBufferCreate(int capacity, int[] status);
    public static native void nativeBufferDestroy(long handle);
    public static native int nativeBufferPush(long handle, byte[] data);
    public static native byte[] nativeBufferPop(long handle, int[] status);
    public static native int nativeBufferLen(long handle);
    public static native boolean nativeBufferIsFull(long handle);
    public static native boolean nativeBufferIsEmpty(long handle);

    // ============================================================================
    // SafeQueue - Queue operations
    // ============================================================================

    public static native long nativeQueueCreate(int capacity, int[] status);
    public static native void nativeQueueDestroy(long handle);
    public static native int nativeQueueEnqueue(long handle, byte[] data);
    public static native byte[] nativeQueueDequeue(long handle, int[] status);
    public static native byte[] nativeQueuePeek(long handle, int[] status);
    public static native int nativeQueueLen(long handle);

    // ============================================================================
    // SafeBloom - Bloom filter operations
    // ============================================================================

    public static native long nativeBloomCreate(int size, int numHashes, int[] status);
    public static native void nativeBloomDestroy(long handle);
    public static native void nativeBloomInsert(long handle, byte[] data);
    public static native boolean nativeBloomContains(long handle, byte[] data);
    public static native double nativeBloomFillRatio(long handle);
    public static native void nativeBloomClear(long handle);

    // ============================================================================
    // SafeLru - LRU cache operations
    // ============================================================================

    public static native long nativeLruCreate(int capacity, int[] status);
    public static native void nativeLruDestroy(long handle);
    public static native int nativeLruPut(long handle, byte[] key, byte[] value);
    public static native byte[] nativeLruGet(long handle, byte[] key, int[] status);
    public static native boolean nativeLruContains(long handle, byte[] key);
    public static native int nativeLruRemove(long handle, byte[] key);
    public static native int nativeLruLen(long handle);

    // ============================================================================
    // SafeGraph - Graph operations
    // ============================================================================

    public static native long nativeGraphCreate(boolean directed, int[] status);
    public static native void nativeGraphDestroy(long handle);
    public static native int nativeGraphAddNode(long handle, byte[] nodeId);
    public static native int nativeGraphAddEdge(long handle, byte[] fromId, byte[] toId, double weight);
    public static native boolean nativeGraphHasNode(long handle, byte[] nodeId);
    public static native boolean nativeGraphHasEdge(long handle, byte[] fromId, byte[] toId);
    public static native int nativeGraphNodeCount(long handle);
    public static native int nativeGraphEdgeCount(long handle);

    // ============================================================================
    // SafeRateLimiter - Rate limiting
    // ============================================================================

    public static native long nativeRateLimiterCreate(long capacity, long refillRate, int[] status);
    public static native void nativeRateLimiterDestroy(long handle);
    public static native boolean nativeRateLimiterTryAcquire(long handle, long count, long currentTime);
    public static native long nativeRateLimiterCurrentTokens(long handle, long currentTime);
    public static native boolean nativeRateLimiterWouldAllow(long handle, long count, long currentTime);

    // ============================================================================
    // SafeCircuitBreaker - Circuit breaker pattern
    // ============================================================================

    public static native long nativeCircuitBreakerCreate(int failureThreshold, int successThreshold, long timeout, int[] status);
    public static native void nativeCircuitBreakerDestroy(long handle);
    public static native boolean nativeCircuitBreakerCanExecute(long handle, long currentTime);
    public static native void nativeCircuitBreakerRecordSuccess(long handle);
    public static native void nativeCircuitBreakerRecordFailure(long handle, long currentTime);
    public static native int nativeCircuitBreakerGetState(long handle);
    public static native void nativeCircuitBreakerReset(long handle);

    // ============================================================================
    // SafeRetry - Retry with backoff
    // ============================================================================

    public static native long nativeRetryCreate(int maxRetries, long baseDelay, long maxDelay, int[] status);
    public static native void nativeRetryDestroy(long handle);
    public static native long nativeRetryNextDelay(long handle);
    public static native boolean nativeRetryShouldRetry(long handle);
    public static native void nativeRetryRecordAttempt(long handle, boolean success);
    public static native void nativeRetryReset(long handle);
    public static native int nativeRetryAttemptCount(long handle);

    // ============================================================================
    // SafeMonotonic - Monotonic values
    // ============================================================================

    public static native long nativeMonotonicCreate(long initial, int[] status);
    public static native void nativeMonotonicDestroy(long handle);
    public static native long nativeMonotonicGet(long handle);
    public static native long nativeMonotonicIncrement(long handle);
    public static native long nativeMonotonicIncrementBy(long handle, long amount, int[] status);
    public static native boolean nativeMonotonicTrySet(long handle, long newValue);

    // ============================================================================
    // SafeStateMachine - State machine
    // ============================================================================

    public static native long nativeStateMachineCreate(int initialState, int[] status);
    public static native void nativeStateMachineDestroy(long handle);
    public static native int nativeStateMachineAddTransition(long handle, int fromState, int toState);
    public static native int nativeStateMachineTransition(long handle, int toState);
    public static native int nativeStateMachineGetCurrent(long handle);
    public static native boolean nativeStateMachineCanTransition(long handle, int toState);

    // ============================================================================
    // SafeCalculator - Calculator operations
    // ============================================================================

    public static native long nativeCalculatorCreate(int[] status);
    public static native void nativeCalculatorDestroy(long handle);
    public static native double nativeCalculatorEvaluate(long handle, byte[] expression, int[] status);
    public static native int nativeCalculatorSetVariable(long handle, byte[] name, double value);
    public static native double nativeCalculatorGetVariable(long handle, byte[] name, int[] status);

    // ============================================================================
    // SafeGeo - Geographic operations
    // ============================================================================

    public static native boolean nativeGeoIsValidCoordinate(double lat, double lon);
    public static native double nativeGeoHaversineDistance(double lat1, double lon1, double lat2, double lon2, double radius);
    public static native double nativeGeoBearing(double lat1, double lon1, double lat2, double lon2);
    public static native double[] nativeGeoDestination(double lat, double lon, double bearing, double distanceKm, int[] status);

    // ============================================================================
    // SafeProbability - Probability operations
    // ============================================================================

    public static native double nativeProbabilityClamp(double value);
    public static native double nativeProbabilityComplement(double p);
    public static native double nativeProbabilityAnd(double p1, double p2);
    public static native double nativeProbabilityOr(double p1, double p2);
    public static native double nativeProbabilityBinomial(int n, int k, double p);
    public static native double nativeProbabilityEntropy(double[] probs);

    // ============================================================================
    // SafeChecksum - Checksum operations
    // ============================================================================

    public static native int nativeChecksumCrc32(byte[] data);
    public static native long nativeChecksumCrc64(byte[] data);
    public static native int nativeChecksumAdler32(byte[] data);
    public static native boolean nativeChecksumLuhnValidate(byte[] digits);
    public static native byte nativeChecksumLuhnGenerate(byte[] digits);

    // ============================================================================
    // SafeTensor - Tensor/matrix operations
    // ============================================================================

    public static native double nativeTensorDot(double[] a, double[] b, int[] status);
    public static native double[] nativeTensorAdd(double[] a, double[] b, int[] status);
    public static native double[] nativeTensorSub(double[] a, double[] b, int[] status);
    public static native double[] nativeTensorScale(double[] v, double scalar);
    public static native double[] nativeTensorMatVec(double[] matrix, int rows, int cols, double[] vector, int[] status);
    public static native int nativeTensorArgmax(double[] v, int[] status);
    public static native double nativeTensorSum(double[] v, int[] status);

    // ============================================================================
    // SafePassword - Password operations
    // ============================================================================

    public static native int nativePasswordStrength(byte[] password);
    public static native byte[] nativePasswordHash(byte[] password, int[] status);
    public static native boolean nativePasswordVerify(byte[] password, byte[] hash);
    public static native byte[] nativePasswordGenerate(int length, boolean includeSymbols, int[] status);

    // ============================================================================
    // SafeMl - Machine learning operations
    // ============================================================================

    public static native double nativeMlSigmoid(double x);
    public static native double nativeMlRelu(double x);
    public static native double[] nativeMlSoftmax(double[] x, int[] status);
    public static native double nativeMlCrossEntropy(double[] predicted, double[] actual, int[] status);
    public static native double[] nativeMlNormalize(double[] x, int[] status);

    // ============================================================================
    // SafeHeader - HTTP header operations
    // ============================================================================

    public static native boolean nativeHeaderIsValidName(byte[] name);
    public static native boolean nativeHeaderIsValidValue(byte[] value);
    public static native byte[] nativeHeaderNormalizeName(byte[] name, int[] status);
    public static native byte[] nativeHeaderParse(byte[] header, int[] status);

    // ============================================================================
    // SafeCookie - HTTP cookie operations
    // ============================================================================

    public static native boolean nativeCookieIsValidName(byte[] name);
    public static native boolean nativeCookieIsValidValue(byte[] value);
    public static native byte[] nativeCookieParse(byte[] cookie, int[] status);
    public static native byte[] nativeCookieSerialize(byte[] name, byte[] value, byte[] options, int[] status);

    // ============================================================================
    // SafeContentType - Content type operations
    // ============================================================================

    public static native boolean nativeContentTypeIsValid(byte[] contentType);
    public static native byte[] nativeContentTypeParse(byte[] contentType, int[] status);
    public static native byte[] nativeContentTypeGetMimeType(byte[] contentType, int[] status);
    public static native byte[] nativeContentTypeGetCharset(byte[] contentType, int[] status);
}
