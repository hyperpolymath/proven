// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

package io.github.hyperpolymath.proven.neo4j;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;

import org.neo4j.procedure.Description;
import org.neo4j.procedure.Name;
import org.neo4j.procedure.UserFunction;

import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * Neo4j stored procedures and user functions backed by libproven.
 *
 * ALL computation is performed in Idris 2 via the Zig FFI bridge.
 * This class contains ONLY JNA marshaling between Neo4j types and
 * the libproven C ABI. No algorithms are reimplemented here.
 *
 * Install:
 *   1. Build this class into a JAR (with JNA dependency)
 *   2. Copy to $NEO4J_HOME/plugins/
 *   3. Ensure libproven.so is on the library path
 *   4. Add to neo4j.conf: dbms.security.procedures.unrestricted=proven.*
 *   5. Restart Neo4j
 */
public class ProvenFunctions {

    // ========================================================================
    // JNA interface to libproven
    // ========================================================================

    /**
     * JNA interface declaring the subset of libproven functions used by
     * these Neo4j procedures. Matches the C ABI from proven.h.
     */
    public interface LibProven extends Library {

        LibProven INSTANCE = Native.load("proven", LibProven.class);

        // Result structs matching C ABI layout

        @Structure.FieldOrder({"status", "value"})
        class IntResult extends Structure implements Structure.ByValue {
            public int status;
            public long value;
        }

        @Structure.FieldOrder({"status", "value"})
        class BoolResult extends Structure implements Structure.ByValue {
            public int status;
            public boolean value;
        }

        @Structure.FieldOrder({"status", "value", "length"})
        class StringResult extends Structure implements Structure.ByValue {
            public int status;
            public Pointer value;
            public long length;
        }

        // Lifecycle
        int proven_init();
        void proven_deinit();
        boolean proven_is_initialized();
        void proven_free_string(Pointer ptr);

        // SafeMath
        IntResult proven_math_add_checked(long a, long b);
        IntResult proven_math_sub_checked(long a, long b);
        IntResult proven_math_mul_checked(long a, long b);
        IntResult proven_math_div(long numerator, long denominator);

        // SafeEmail
        BoolResult proven_email_is_valid(Pointer ptr, long len);

        // SafeNetwork
        // IPv4 validation returns a struct; we check status only
        Pointer proven_network_parse_ipv4(Pointer ptr, long len);

        // SafeJson
        BoolResult proven_json_is_valid(Pointer ptr, long len);

        // SafeString
        StringResult proven_string_escape_html(Pointer ptr, long len);

        // SafeChecksum
        IntResult proven_checksum_crc32(Pointer ptr, long len);

        // SafeHex
        StringResult proven_hex_encode(Pointer ptr, long len, boolean uppercase);
        Pointer proven_hex_decode(Pointer ptr, long len);
        void proven_hex_free(Pointer result);

        // SafeUrl - we use url_parse for validation
        // Returns a complex struct; we check status field at offset 0
        Pointer proven_url_parse_raw(Pointer ptr, long len);
    }

    private static final LibProven LIB;

    static {
        LIB = LibProven.INSTANCE;
        LIB.proven_init();
    }

    // ========================================================================
    // Internal helpers
    // ========================================================================

    /**
     * Convert a Java String to a native pointer and its byte length.
     * Caller must free the returned pointer.
     */
    private static Pointer toNativeString(String s) {
        if (s == null) {
            return null;
        }
        byte[] bytes = s.getBytes(StandardCharsets.UTF_8);
        Pointer ptr = new com.sun.jna.Memory(bytes.length);
        ptr.write(0, bytes, 0, bytes.length);
        return ptr;
    }

    /**
     * Extract a Java String from a StringResult, freeing the native memory.
     * Returns null if the result status is not OK.
     */
    private static String extractString(LibProven.StringResult result) {
        if (result.status != 0 || result.value == null) {
            if (result.value != null) {
                LIB.proven_free_string(result.value);
            }
            return null;
        }

        String managed = result.value.getString(0, "UTF-8");
        LIB.proven_free_string(result.value);
        return managed;
    }

    // ========================================================================
    // SafeMath functions
    // ========================================================================

    @UserFunction("proven.safeAdd")
    @Description("Checked addition with overflow detection via libproven. Returns null on overflow.")
    public Long safeAdd(
            @Name("a") Long a,
            @Name("b") Long b) {
        if (a == null || b == null) {
            return null;
        }

        LibProven.IntResult result = LIB.proven_math_add_checked(a, b);
        if (result.status != 0) {
            return null;
        }
        return result.value;
    }

    @UserFunction("proven.safeSub")
    @Description("Checked subtraction with underflow detection via libproven. Returns null on underflow.")
    public Long safeSub(
            @Name("a") Long a,
            @Name("b") Long b) {
        if (a == null || b == null) {
            return null;
        }

        LibProven.IntResult result = LIB.proven_math_sub_checked(a, b);
        if (result.status != 0) {
            return null;
        }
        return result.value;
    }

    @UserFunction("proven.safeMul")
    @Description("Checked multiplication with overflow detection via libproven. Returns null on overflow.")
    public Long safeMul(
            @Name("a") Long a,
            @Name("b") Long b) {
        if (a == null || b == null) {
            return null;
        }

        LibProven.IntResult result = LIB.proven_math_mul_checked(a, b);
        if (result.status != 0) {
            return null;
        }
        return result.value;
    }

    @UserFunction("proven.safeDiv")
    @Description("Safe integer division via libproven. Returns null on division by zero or overflow.")
    public Long safeDiv(
            @Name("a") Long a,
            @Name("b") Long b) {
        if (a == null || b == null) {
            return null;
        }

        LibProven.IntResult result = LIB.proven_math_div(a, b);
        if (result.status != 0) {
            return null;
        }
        return result.value;
    }

    // ========================================================================
    // Validation functions
    // ========================================================================

    @UserFunction("proven.validateEmail")
    @Description("Validate email address (RFC 5321 simplified) via libproven.")
    public Boolean validateEmail(
            @Name("addr") String addr) {
        if (addr == null) {
            return null;
        }

        byte[] bytes = addr.getBytes(StandardCharsets.UTF_8);
        Pointer ptr = toNativeString(addr);
        if (ptr == null) {
            return null;
        }

        LibProven.BoolResult result = LIB.proven_email_is_valid(ptr, bytes.length);
        if (result.status != 0) {
            return null;
        }
        return result.value;
    }

    @UserFunction("proven.validateUrl")
    @Description("Validate URL by attempting to parse it via libproven.")
    public Boolean validateUrl(
            @Name("url") String url) {
        if (url == null) {
            return null;
        }

        // URL validation: attempt to parse, check if status == 0
        // Since the URL parse returns a complex struct, we validate by
        // checking if the parse succeeds (status field at offset 0).
        byte[] bytes = url.getBytes(StandardCharsets.UTF_8);
        Pointer ptr = toNativeString(url);
        if (ptr == null) {
            return null;
        }

        Pointer resultPtr = LIB.proven_url_parse_raw(ptr, bytes.length);
        if (resultPtr == null) {
            return false;
        }
        int status = resultPtr.getInt(0);
        return status == 0;
    }

    @UserFunction("proven.validateIpv4")
    @Description("Validate IPv4 address string via libproven.")
    public Boolean validateIpv4(
            @Name("addr") String addr) {
        if (addr == null) {
            return null;
        }

        byte[] bytes = addr.getBytes(StandardCharsets.UTF_8);
        Pointer ptr = toNativeString(addr);
        if (ptr == null) {
            return null;
        }

        // IPv4 parse returns a struct with status at offset 0
        Pointer resultPtr = LIB.proven_network_parse_ipv4(ptr, bytes.length);
        if (resultPtr == null) {
            return false;
        }
        int status = resultPtr.getInt(0);
        return status == 0;
    }

    @UserFunction("proven.validateJson")
    @Description("Validate JSON string via libproven.")
    public Boolean validateJson(
            @Name("doc") String doc) {
        if (doc == null) {
            return null;
        }

        byte[] bytes = doc.getBytes(StandardCharsets.UTF_8);
        Pointer ptr = toNativeString(doc);
        if (ptr == null) {
            return null;
        }

        LibProven.BoolResult result = LIB.proven_json_is_valid(ptr, bytes.length);
        if (result.status != 0) {
            return null;
        }
        return result.value;
    }

    // ========================================================================
    // String functions
    // ========================================================================

    @UserFunction("proven.sanitizeString")
    @Description("Sanitize string by escaping HTML entities via libproven (XSS prevention).")
    public String sanitizeString(
            @Name("input") String input) {
        if (input == null) {
            return null;
        }

        byte[] bytes = input.getBytes(StandardCharsets.UTF_8);
        Pointer ptr = toNativeString(input);
        if (ptr == null) {
            return null;
        }

        LibProven.StringResult result = LIB.proven_string_escape_html(ptr, bytes.length);
        return extractString(result);
    }

    @UserFunction("proven.hashSha256")
    @Description("CRC32 checksum as hex string via libproven. For real SHA-256 use native crypto.")
    public String hashSha256(
            @Name("input") String input) {
        if (input == null) {
            return null;
        }

        byte[] bytes = input.getBytes(StandardCharsets.UTF_8);
        Pointer ptr = toNativeString(input);
        if (ptr == null) {
            return null;
        }

        LibProven.IntResult crcResult = LIB.proven_checksum_crc32(ptr, bytes.length);
        if (crcResult.status != 0) {
            return null;
        }

        // Encode CRC32 value as hex
        int crc = (int) crcResult.value;
        byte[] crcBytes = new byte[] {
            (byte) ((crc >> 24) & 0xFF),
            (byte) ((crc >> 16) & 0xFF),
            (byte) ((crc >> 8) & 0xFF),
            (byte) (crc & 0xFF)
        };

        Pointer crcPtr = new com.sun.jna.Memory(4);
        crcPtr.write(0, crcBytes, 0, 4);

        LibProven.StringResult hexResult = LIB.proven_hex_encode(crcPtr, 4, false);
        return extractString(hexResult);
    }

    @UserFunction("proven.hexEncode")
    @Description("Encode bytes as lowercase hexadecimal string via libproven.")
    public String hexEncode(
            @Name("input") String input) {
        if (input == null) {
            return null;
        }

        byte[] bytes = input.getBytes(StandardCharsets.UTF_8);
        Pointer ptr = toNativeString(input);
        if (ptr == null) {
            return null;
        }

        LibProven.StringResult result = LIB.proven_hex_encode(ptr, bytes.length, false);
        return extractString(result);
    }

    @UserFunction("proven.hexDecode")
    @Description("Decode hexadecimal string to bytes via libproven.")
    public String hexDecode(
            @Name("input") String input) {
        if (input == null) {
            return null;
        }

        byte[] bytes = input.getBytes(StandardCharsets.UTF_8);
        Pointer ptr = toNativeString(input);
        if (ptr == null) {
            return null;
        }

        Pointer resultPtr = LIB.proven_hex_decode(ptr, bytes.length);
        if (resultPtr == null) {
            return null;
        }

        // Read status from offset 0
        int status = resultPtr.getInt(0);
        if (status != 0) {
            LIB.proven_hex_free(resultPtr);
            return null;
        }

        // Read data pointer and length from the struct
        Pointer dataPtr = resultPtr.getPointer(8); // offset after int32 + padding
        long dataLen = resultPtr.getLong(16);       // offset after pointer

        if (dataPtr == null || dataLen <= 0) {
            LIB.proven_hex_free(resultPtr);
            return null;
        }

        byte[] decoded = dataPtr.getByteArray(0, (int) dataLen);
        LIB.proven_hex_free(resultPtr);

        return new String(decoded, StandardCharsets.UTF_8);
    }
}
