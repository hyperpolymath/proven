// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// LibProven - Low-level CFFI declarations for libproven.
// All computation is performed in verified Idris 2 code via the Zig FFI layer.
// This module loads the native library and exposes raw C function pointers.
// Higher-level Safe* modules provide idiomatic Haxe wrappers around these.

package proven;

#if cpp
import cpp.Lib;
import cpp.Pointer;
import cpp.RawPointer;
import cpp.Int64;
import cpp.UInt32;
import cpp.UInt8;
import cpp.Float64;
#end

/**
 * Status codes returned by libproven operations.
 * Zero indicates success; negative values indicate specific error conditions.
 */
enum abstract ProvenStatus(Int) from Int to Int {
    var OK = 0;
    var ERR_NULL_POINTER = -1;
    var ERR_INVALID_ARGUMENT = -2;
    var ERR_OVERFLOW = -3;
    var ERR_UNDERFLOW = -4;
    var ERR_DIVISION_BY_ZERO = -5;
    var ERR_PARSE_FAILURE = -6;
    var ERR_VALIDATION_FAILED = -7;
    var ERR_OUT_OF_BOUNDS = -8;
    var ERR_ENCODING_ERROR = -9;
    var ERR_ALLOCATION_FAILED = -10;
    var ERR_NOT_IMPLEMENTED = -99;

    /**
     * Returns a human-readable description of the status code.
     */
    public function toString():String {
        return switch (cast this : Int) {
            case 0: "OK";
            case -1: "NULL_POINTER";
            case -2: "INVALID_ARGUMENT";
            case -3: "OVERFLOW";
            case -4: "UNDERFLOW";
            case -5: "DIVISION_BY_ZERO";
            case -6: "PARSE_FAILURE";
            case -7: "VALIDATION_FAILED";
            case -8: "OUT_OF_BOUNDS";
            case -9: "ENCODING_ERROR";
            case -10: "ALLOCATION_FAILED";
            case -99: "NOT_IMPLEMENTED";
            default: "UNKNOWN(" + Std.string(this) + ")";
        };
    }
}

/**
 * Result container for integer operations.
 * Contains a status code and an Int64 value.
 */
typedef IntResult = {
    var status:ProvenStatus;
    var value:haxe.Int64;
};

/**
 * Result container for boolean operations.
 * Contains a status code and a Bool value.
 */
typedef BoolResult = {
    var status:ProvenStatus;
    var value:Bool;
};

/**
 * Result container for string operations.
 * Contains a status code, a string value, and its length.
 * The native string memory is freed automatically upon conversion.
 */
typedef StringResult = {
    var status:ProvenStatus;
    var value:String;
};

/**
 * Result container for floating-point operations.
 * Contains a status code and a Float value.
 */
typedef FloatResult = {
    var status:ProvenStatus;
    var value:Float;
};

/**
 * Low-level CFFI interface to libproven.
 *
 * This class loads the libproven shared library and provides raw FFI
 * function bindings. Use the higher-level Safe* wrapper classes for
 * idiomatic Haxe usage with proper error handling.
 *
 * All computation is delegated to the formally verified Idris 2 core
 * via the Zig FFI bridge. No logic is reimplemented in Haxe.
 */
@:buildXml('<linker><lib name="proven"/></linker>')
class LibProven {
    // ========================================================================
    // Lifecycle
    // ========================================================================

    /**
     * Initialize the Proven runtime (includes Idris 2 runtime).
     * Must be called before any other Proven function. Safe to call
     * multiple times.
     * @return ProvenStatus.OK on success
     */
    @:cppFileCode('#include <proven.h>')
    public static function init():ProvenStatus {
        #if cpp
        return untyped __cpp__('proven_init()');
        #else
        return ProvenStatus.ERR_NOT_IMPLEMENTED;
        #end
    }

    /**
     * Cleanup the Proven runtime. Call when done using Proven functions.
     */
    public static function deinit():Void {
        #if cpp
        untyped __cpp__('proven_deinit()');
        #end
    }

    /**
     * Check if the runtime is initialized.
     * @return true if initialized
     */
    public static function isInitialized():Bool {
        #if cpp
        return untyped __cpp__('proven_is_initialized()');
        #else
        return false;
        #end
    }

    /**
     * Get the FFI ABI version for compatibility checking.
     * @return ABI version number
     */
    public static function abiVersion():Int {
        #if cpp
        return untyped __cpp__('(int)proven_ffi_abi_version()');
        #else
        return 0;
        #end
    }

    /**
     * Get the library version string.
     * @return Version in "major.minor.patch" format
     */
    public static function version():String {
        #if cpp
        var major:Int = untyped __cpp__('(int)proven_version_major()');
        var minor:Int = untyped __cpp__('(int)proven_version_minor()');
        var patch:Int = untyped __cpp__('(int)proven_version_patch()');
        return Std.string(major) + "." + Std.string(minor) + "." + Std.string(patch);
        #else
        return "0.0.0";
        #end
    }

    /**
     * Get total module count.
     * @return number of modules in the library
     */
    public static function moduleCount():Int {
        #if cpp
        return untyped __cpp__('(int)proven_module_count()');
        #else
        return 0;
        #end
    }

    // ========================================================================
    // Memory management
    // ========================================================================

    /**
     * Free a string allocated by libproven functions.
     * Called automatically by string result helpers.
     */
    public static function freeString(ptr:Dynamic):Void {
        #if cpp
        untyped __cpp__('proven_free_string((char*){0})', ptr);
        #end
    }

    // ========================================================================
    // Internal helpers for result extraction
    // ========================================================================

    /**
     * Call a libproven function that returns IntResult and extract the value.
     * Returns null if the status indicates an error.
     */
    public static function extractIntResult(status:Int, value:haxe.Int64):Null<haxe.Int64> {
        if (status != 0) return null;
        return value;
    }

    /**
     * Call a libproven function that returns BoolResult and extract the value.
     * Returns null if the status indicates an error.
     */
    public static function extractBoolResult(status:Int, value:Bool):Null<Bool> {
        if (status != 0) return null;
        return value;
    }

    /**
     * Call a libproven function that returns FloatResult and extract the value.
     * Returns null if the status indicates an error.
     */
    public static function extractFloatResult(status:Int, value:Float):Null<Float> {
        if (status != 0) return null;
        return value;
    }

    // ========================================================================
    // SafeMath - raw FFI calls
    // ========================================================================

    /** Safe integer addition with overflow checking via libproven. */
    public static function mathAddChecked(a:haxe.Int64, b:haxe.Int64):IntResult {
        #if cpp
        var status:Int = 0;
        var value:haxe.Int64 = haxe.Int64.make(0, 0);
        untyped __cpp__('{
            ProvenIntResult r = proven_math_add_checked((int64_t){0}, (int64_t){1});
            {2} = r.status;
            {3} = r.value;
        }', a, b, status, value);
        return {status: (status : ProvenStatus), value: value};
        #else
        return {status: ProvenStatus.ERR_NOT_IMPLEMENTED, value: haxe.Int64.make(0, 0)};
        #end
    }

    /** Safe integer subtraction with underflow checking via libproven. */
    public static function mathSubChecked(a:haxe.Int64, b:haxe.Int64):IntResult {
        #if cpp
        var status:Int = 0;
        var value:haxe.Int64 = haxe.Int64.make(0, 0);
        untyped __cpp__('{
            ProvenIntResult r = proven_math_sub_checked((int64_t){0}, (int64_t){1});
            {2} = r.status;
            {3} = r.value;
        }', a, b, status, value);
        return {status: (status : ProvenStatus), value: value};
        #else
        return {status: ProvenStatus.ERR_NOT_IMPLEMENTED, value: haxe.Int64.make(0, 0)};
        #end
    }

    /** Safe integer multiplication with overflow checking via libproven. */
    public static function mathMulChecked(a:haxe.Int64, b:haxe.Int64):IntResult {
        #if cpp
        var status:Int = 0;
        var value:haxe.Int64 = haxe.Int64.make(0, 0);
        untyped __cpp__('{
            ProvenIntResult r = proven_math_mul_checked((int64_t){0}, (int64_t){1});
            {2} = r.status;
            {3} = r.value;
        }', a, b, status, value);
        return {status: (status : ProvenStatus), value: value};
        #else
        return {status: ProvenStatus.ERR_NOT_IMPLEMENTED, value: haxe.Int64.make(0, 0)};
        #end
    }

    /** Safe integer division via libproven. */
    public static function mathDiv(a:haxe.Int64, b:haxe.Int64):IntResult {
        #if cpp
        var status:Int = 0;
        var value:haxe.Int64 = haxe.Int64.make(0, 0);
        untyped __cpp__('{
            ProvenIntResult r = proven_math_div((int64_t){0}, (int64_t){1});
            {2} = r.status;
            {3} = r.value;
        }', a, b, status, value);
        return {status: (status : ProvenStatus), value: value};
        #else
        return {status: ProvenStatus.ERR_NOT_IMPLEMENTED, value: haxe.Int64.make(0, 0)};
        #end
    }

    /** Safe integer modulo via libproven. */
    public static function mathMod(a:haxe.Int64, b:haxe.Int64):IntResult {
        #if cpp
        var status:Int = 0;
        var value:haxe.Int64 = haxe.Int64.make(0, 0);
        untyped __cpp__('{
            ProvenIntResult r = proven_math_mod((int64_t){0}, (int64_t){1});
            {2} = r.status;
            {3} = r.value;
        }', a, b, status, value);
        return {status: (status : ProvenStatus), value: value};
        #else
        return {status: ProvenStatus.ERR_NOT_IMPLEMENTED, value: haxe.Int64.make(0, 0)};
        #end
    }

    /** Safe absolute value via libproven. */
    public static function mathAbsSafe(n:haxe.Int64):IntResult {
        #if cpp
        var status:Int = 0;
        var value:haxe.Int64 = haxe.Int64.make(0, 0);
        untyped __cpp__('{
            ProvenIntResult r = proven_math_abs_safe((int64_t){0});
            {1} = r.status;
            {2} = r.value;
        }', n, status, value);
        return {status: (status : ProvenStatus), value: value};
        #else
        return {status: ProvenStatus.ERR_NOT_IMPLEMENTED, value: haxe.Int64.make(0, 0)};
        #end
    }

    /** Clamp value to [lo, hi] via libproven. */
    public static function mathClamp(lo:haxe.Int64, hi:haxe.Int64, value:haxe.Int64):haxe.Int64 {
        #if cpp
        var result:haxe.Int64 = haxe.Int64.make(0, 0);
        untyped __cpp__('{0} = proven_math_clamp((int64_t){1}, (int64_t){2}, (int64_t){3})',
            result, lo, hi, value);
        return result;
        #else
        return value;
        #end
    }

    /** Checked integer exponentiation via libproven. */
    public static function mathPowChecked(base:haxe.Int64, exp:Int):IntResult {
        #if cpp
        var status:Int = 0;
        var value:haxe.Int64 = haxe.Int64.make(0, 0);
        untyped __cpp__('{
            ProvenIntResult r = proven_math_pow_checked((int64_t){0}, (uint32_t){1});
            {2} = r.status;
            {3} = r.value;
        }', base, exp, status, value);
        return {status: (status : ProvenStatus), value: value};
        #else
        return {status: ProvenStatus.ERR_NOT_IMPLEMENTED, value: haxe.Int64.make(0, 0)};
        #end
    }

    // ========================================================================
    // SafeString - raw FFI calls
    // ========================================================================

    /** Check if bytes are valid UTF-8 via libproven. */
    public static function stringIsValidUtf8(s:String):Null<Bool> {
        #if cpp
        var status:Int = 0;
        var value:Bool = false;
        untyped __cpp__('{
            ProvenBoolResult r = proven_string_is_valid_utf8((const uint8_t*){0}, (size_t){1});
            {2} = r.status;
            {3} = r.value;
        }', s, s.length, status, value);
        if (status != 0) return null;
        return value;
        #else
        return null;
        #end
    }

    /** Escape string for SQL via libproven. */
    public static function stringEscapeSql(s:String):Null<String> {
        #if cpp
        return _callStringFn("proven_string_escape_sql", s);
        #else
        return null;
        #end
    }

    /** Escape string for HTML via libproven. */
    public static function stringEscapeHtml(s:String):Null<String> {
        #if cpp
        return _callStringFn("proven_string_escape_html", s);
        #else
        return null;
        #end
    }

    /** Escape string for JavaScript via libproven. */
    public static function stringEscapeJs(s:String):Null<String> {
        #if cpp
        return _callStringFn("proven_string_escape_js", s);
        #else
        return null;
        #end
    }

    // ========================================================================
    // SafePath - raw FFI calls
    // ========================================================================

    /** Check for directory traversal sequences via libproven. */
    public static function pathHasTraversal(s:String):Null<Bool> {
        #if cpp
        var status:Int = 0;
        var value:Bool = false;
        untyped __cpp__('{
            ProvenBoolResult r = proven_path_has_traversal((const uint8_t*){0}, (size_t){1});
            {2} = r.status;
            {3} = r.value;
        }', s, s.length, status, value);
        if (status != 0) return null;
        return value;
        #else
        return null;
        #end
    }

    /** Sanitize a filename via libproven. */
    public static function pathSanitizeFilename(s:String):Null<String> {
        #if cpp
        return _callStringFn("proven_path_sanitize_filename", s);
        #else
        return null;
        #end
    }

    // ========================================================================
    // SafeEmail - raw FFI calls
    // ========================================================================

    /** Validate email address via libproven. */
    public static function emailIsValid(s:String):Null<Bool> {
        #if cpp
        var status:Int = 0;
        var value:Bool = false;
        untyped __cpp__('{
            ProvenBoolResult r = proven_email_is_valid((const uint8_t*){0}, (size_t){1});
            {2} = r.status;
            {3} = r.value;
        }', s, s.length, status, value);
        if (status != 0) return null;
        return value;
        #else
        return null;
        #end
    }

    // ========================================================================
    // SafeUrl - raw FFI calls
    // ========================================================================

    /** Parse URL into components via libproven. Returns null on error. */
    public static function urlParse(s:String):Null<{
        scheme:String,
        host:String,
        port:Null<Int>,
        path:String,
        query:String,
        fragment:String
    }> {
        #if cpp
        var status:Int = 0;
        var scheme:String = "";
        var host:String = "";
        var port:Int = 0;
        var hasPort:Bool = false;
        var path:String = "";
        var query:String = "";
        var fragment:String = "";
        untyped __cpp__('{
            ProvenUrlResult r = proven_url_parse((const uint8_t*){0}, (size_t){1});
            {2} = r.status;
            if (r.status == 0) {
                if (r.components.scheme && r.components.scheme_len > 0)
                    {3} = ::String(r.components.scheme, r.components.scheme_len);
                if (r.components.host && r.components.host_len > 0)
                    {4} = ::String(r.components.host, r.components.host_len);
                {5} = (int)r.components.port;
                {6} = r.components.has_port;
                if (r.components.path && r.components.path_len > 0)
                    {7} = ::String(r.components.path, r.components.path_len);
                if (r.components.query && r.components.query_len > 0)
                    {8} = ::String(r.components.query, r.components.query_len);
                if (r.components.fragment && r.components.fragment_len > 0)
                    {9} = ::String(r.components.fragment, r.components.fragment_len);
                proven_url_free(&r.components);
            }
        }', s, s.length, status, scheme, host, port, hasPort, path, query, fragment);
        if (status != 0) return null;
        return {
            scheme: scheme,
            host: host,
            port: hasPort ? port : null,
            path: path,
            query: query,
            fragment: fragment
        };
        #else
        return null;
        #end
    }

    // ========================================================================
    // SafeNetwork - raw FFI calls
    // ========================================================================

    /** Parse IPv4 address via libproven. Returns octets array or null. */
    public static function networkParseIpv4(s:String):Null<Array<Int>> {
        #if cpp
        var status:Int = 0;
        var a:Int = 0;
        var b:Int = 0;
        var c:Int = 0;
        var d:Int = 0;
        untyped __cpp__('{
            ProvenIPv4Result r = proven_network_parse_ipv4((const uint8_t*){0}, (size_t){1});
            {2} = r.status;
            {3} = r.address.octets[0];
            {4} = r.address.octets[1];
            {5} = r.address.octets[2];
            {6} = r.address.octets[3];
        }', s, s.length, status, a, b, c, d);
        if (status != 0) return null;
        return [a, b, c, d];
        #else
        return null;
        #end
    }

    /** Check if IPv4 address is private via libproven. */
    public static function networkIpv4IsPrivate(octets:Array<Int>):Bool {
        #if cpp
        return untyped __cpp__('{
            ProvenIPv4Address addr;
            addr.octets[0] = (uint8_t){0};
            addr.octets[1] = (uint8_t){1};
            addr.octets[2] = (uint8_t){2};
            addr.octets[3] = (uint8_t){3};
            return proven_network_ipv4_is_private(addr);
        }', octets[0], octets[1], octets[2], octets[3]);
        #else
        return false;
        #end
    }

    /** Check if IPv4 address is loopback via libproven. */
    public static function networkIpv4IsLoopback(octets:Array<Int>):Bool {
        #if cpp
        return untyped __cpp__('{
            ProvenIPv4Address addr;
            addr.octets[0] = (uint8_t){0};
            addr.octets[1] = (uint8_t){1};
            addr.octets[2] = (uint8_t){2};
            addr.octets[3] = (uint8_t){3};
            return proven_network_ipv4_is_loopback(addr);
        }', octets[0], octets[1], octets[2], octets[3]);
        #else
        return false;
        #end
    }

    // ========================================================================
    // SafeCrypto - raw FFI calls
    // ========================================================================

    /** Constant-time byte comparison via libproven. */
    public static function cryptoConstantTimeEq(a:String, b:String):Null<Bool> {
        #if cpp
        var status:Int = 0;
        var value:Bool = false;
        untyped __cpp__('{
            ProvenBoolResult r = proven_crypto_constant_time_eq(
                (const uint8_t*){0}, (size_t){1},
                (const uint8_t*){2}, (size_t){3});
            {4} = r.status;
            {5} = r.value;
        }', a, a.length, b, b.length, status, value);
        if (status != 0) return null;
        return value;
        #else
        return null;
        #end
    }

    // ========================================================================
    // SafeJson - raw FFI calls
    // ========================================================================

    /** Validate JSON string via libproven. */
    public static function jsonIsValid(s:String):Null<Bool> {
        #if cpp
        var status:Int = 0;
        var value:Bool = false;
        untyped __cpp__('{
            ProvenBoolResult r = proven_json_is_valid((const uint8_t*){0}, (size_t){1});
            {2} = r.status;
            {3} = r.value;
        }', s, s.length, status, value);
        if (status != 0) return null;
        return value;
        #else
        return null;
        #end
    }

    /** Get JSON root value type via libproven. Returns -1 for invalid. */
    public static function jsonGetType(s:String):Int {
        #if cpp
        return untyped __cpp__(
            '(int)proven_json_get_type((const uint8_t*){0}, (size_t){1})', s, s.length);
        #else
        return -1;
        #end
    }

    // ========================================================================
    // SafeDateTime - raw FFI calls
    // ========================================================================

    /** Check if year is a leap year via libproven. */
    public static function datetimeIsLeapYear(year:Int):Bool {
        #if cpp
        return untyped __cpp__('proven_datetime_is_leap_year((int32_t){0})', year);
        #else
        return false;
        #end
    }

    /** Get days in a given month via libproven. */
    public static function datetimeDaysInMonth(year:Int, month:Int):Int {
        #if cpp
        return untyped __cpp__(
            '(int)proven_datetime_days_in_month((int32_t){0}, (uint8_t){1})', year, month);
        #else
        return 0;
        #end
    }

    // ========================================================================
    // SafeFloat - raw FFI calls
    // ========================================================================

    /** Safe floating-point division via libproven. */
    public static function floatDiv(a:Float, b:Float):FloatResult {
        #if cpp
        var status:Int = 0;
        var value:Float = 0.0;
        untyped __cpp__('{
            ProvenFloatResult r = proven_float_div((double){0}, (double){1});
            {2} = r.status;
            {3} = r.value;
        }', a, b, status, value);
        return {status: (status : ProvenStatus), value: value};
        #else
        return {status: ProvenStatus.ERR_NOT_IMPLEMENTED, value: 0.0};
        #end
    }

    /** Check if float is finite via libproven. */
    public static function floatIsFinite(x:Float):Bool {
        #if cpp
        return untyped __cpp__('proven_float_is_finite((double){0})', x);
        #else
        return false;
        #end
    }

    /** Check if float is NaN via libproven. */
    public static function floatIsNaN(x:Float):Bool {
        #if cpp
        return untyped __cpp__('proven_float_is_nan((double){0})', x);
        #else
        return false;
        #end
    }

    /** Safe square root via libproven. */
    public static function floatSqrt(x:Float):FloatResult {
        #if cpp
        var status:Int = 0;
        var value:Float = 0.0;
        untyped __cpp__('{
            ProvenFloatResult r = proven_float_sqrt((double){0});
            {1} = r.status;
            {2} = r.value;
        }', x, status, value);
        return {status: (status : ProvenStatus), value: value};
        #else
        return {status: ProvenStatus.ERR_NOT_IMPLEMENTED, value: 0.0};
        #end
    }

    /** Safe natural logarithm via libproven. */
    public static function floatLn(x:Float):FloatResult {
        #if cpp
        var status:Int = 0;
        var value:Float = 0.0;
        untyped __cpp__('{
            ProvenFloatResult r = proven_float_ln((double){0});
            {1} = r.status;
            {2} = r.value;
        }', x, status, value);
        return {status: (status : ProvenStatus), value: value};
        #else
        return {status: ProvenStatus.ERR_NOT_IMPLEMENTED, value: 0.0};
        #end
    }

    // ========================================================================
    // Internal: generic string function caller
    // ========================================================================

    #if cpp
    private static function _callStringFn(fnName:String, s:String):Null<String> {
        var status:Int = 0;
        var result:String = "";
        // We use a dispatch pattern to call the correct C function
        if (fnName == "proven_string_escape_sql") {
            untyped __cpp__('{
                ProvenStringResult r = proven_string_escape_sql((const uint8_t*){0}, (size_t){1});
                {2} = r.status;
                if (r.status == 0 && r.value != NULL) {
                    {3} = ::String(r.value, r.length);
                    proven_free_string(r.value);
                }
            }', s, s.length, status, result);
        } else if (fnName == "proven_string_escape_html") {
            untyped __cpp__('{
                ProvenStringResult r = proven_string_escape_html((const uint8_t*){0}, (size_t){1});
                {2} = r.status;
                if (r.status == 0 && r.value != NULL) {
                    {3} = ::String(r.value, r.length);
                    proven_free_string(r.value);
                }
            }', s, s.length, status, result);
        } else if (fnName == "proven_string_escape_js") {
            untyped __cpp__('{
                ProvenStringResult r = proven_string_escape_js((const uint8_t*){0}, (size_t){1});
                {2} = r.status;
                if (r.status == 0 && r.value != NULL) {
                    {3} = ::String(r.value, r.length);
                    proven_free_string(r.value);
                }
            }', s, s.length, status, result);
        } else if (fnName == "proven_path_sanitize_filename") {
            untyped __cpp__('{
                ProvenStringResult r = proven_path_sanitize_filename((const uint8_t*){0}, (size_t){1});
                {2} = r.status;
                if (r.status == 0 && r.value != NULL) {
                    {3} = ::String(r.value, r.length);
                    proven_free_string(r.value);
                }
            }', s, s.length, status, result);
        }
        if (status != 0) return null;
        return result;
    }
    #end
}
