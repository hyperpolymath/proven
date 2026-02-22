// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Raw FFI bindings to libproven shared library.
///
/// This file provides the low-level dart:ffi interface to libproven.
/// All computation is performed by the Idris 2 verified core; this layer
/// handles only data marshaling across the C ABI boundary.
///
/// DO NOT add any logic here -- only FFI declarations and library loading.
library;

import 'dart:ffi';
import 'dart:io' show Platform;

import 'package:ffi/ffi.dart';

// ============================================================================
// C ABI result structs (must match ffi/zig/src/main.zig layout)
// ============================================================================

/// Status codes returned by libproven functions.
class ProvenStatus {
  static const int ok = 0;
  static const int errNullPointer = -1;
  static const int errInvalidArgument = -2;
  static const int errOverflow = -3;
  static const int errUnderflow = -4;
  static const int errDivisionByZero = -5;
  static const int errParseFailure = -6;
  static const int errValidationFailed = -7;
  static const int errOutOfBounds = -8;
  static const int errEncodingError = -9;
  static const int errAllocationFailed = -10;
  static const int errNotImplemented = -99;
}

/// Result struct for integer operations.
final class IntResult extends Struct {
  /// Status code (0 = ok, negative = error).
  @Int32()
  external int status;

  /// The result value (valid only when status == 0).
  @Int64()
  external int value;
}

/// Result struct for boolean operations.
final class BoolResult extends Struct {
  /// Status code (0 = ok, negative = error).
  @Int32()
  external int status;

  /// The result value (valid only when status == 0).
  @Bool()
  external bool value;
}

/// Result struct for string operations.
/// Caller must free the returned string via proven_free_string.
final class StringResult extends Struct {
  /// Status code (0 = ok, negative = error).
  @Int32()
  external int status;

  /// Pointer to null-terminated UTF-8 string (valid only when status == 0).
  external Pointer<Utf8> value;

  /// Length of string in bytes.
  @Size()
  external int length;
}

/// Result struct for float operations.
final class FloatResult extends Struct {
  /// Status code (0 = ok, negative = error).
  @Int32()
  external int status;

  /// The result value (valid only when status == 0).
  @Double()
  external double value;
}

/// IPv4 address as 4 octets.
final class IPv4Address extends Struct {
  @Uint8()
  external int a;

  @Uint8()
  external int b;

  @Uint8()
  external int c;

  @Uint8()
  external int d;
}

/// Result struct for IPv4 parse operations.
final class IPv4Result extends Struct {
  @Int32()
  external int status;

  external IPv4Address address;
}

/// UUID as 16 bytes.
final class UUID extends Struct {
  @Array(16)
  external Array<Uint8> bytes;
}

/// Result struct for UUID operations.
final class UUIDResult extends Struct {
  @Int32()
  external int status;

  external UUID uuid;
}

/// RGB color.
final class RGBColor extends Struct {
  @Uint8()
  external int r;

  @Uint8()
  external int g;

  @Uint8()
  external int b;
}

/// HSL color.
final class HSLColor extends Struct {
  @Float()
  external double h;

  @Float()
  external double s;

  @Float()
  external double l;
}

/// Result struct for color parse operations.
final class ColorParseResult extends Struct {
  @Int32()
  external int status;

  external RGBColor color;
}

/// Hex decode result.
final class HexDecodeResult extends Struct {
  @Int32()
  external int status;

  external Pointer<Uint8> data;

  @Size()
  external int length;
}

/// Semantic version struct.
final class CSemanticVersion extends Struct {
  @Uint32()
  external int major;

  @Uint32()
  external int minor;

  @Uint32()
  external int patch;

  external Pointer<Utf8> preRelease;

  @Size()
  external int preReleaseLen;

  external Pointer<Utf8> buildMetadata;

  @Size()
  external int buildMetadataLen;
}

/// Result struct for version parse operations.
final class VersionResult extends Struct {
  @Int32()
  external int status;

  external CSemanticVersion version;
}

/// DateTime struct matching C ABI.
final class CDateTime extends Struct {
  @Int32()
  external int year;

  @Uint8()
  external int month;

  @Uint8()
  external int day;

  @Uint8()
  external int hour;

  @Uint8()
  external int minute;

  @Uint8()
  external int second;
}

/// Result struct for datetime parse operations.
final class DateTimeResult extends Struct {
  @Int32()
  external int status;

  external CDateTime datetime;
}

/// Currency result from C ABI.
final class CCurrencyResult extends Struct {
  @Int32()
  external int status;

  @Int64()
  external int amountMinor;

  @Array(3)
  external Array<Uint8> code;

  @Uint8()
  external int decimalPlaces;
}

/// Phone result from C ABI.
final class CPhoneResult extends Struct {
  @Int32()
  external int status;

  @Uint16()
  external int countryCode;

  @Uint64()
  external int nationalNumber;
}

/// Length unit enum matching C ABI.
class CLengthUnit {
  static const int meters = 0;
  static const int kilometers = 1;
  static const int centimeters = 2;
  static const int millimeters = 3;
  static const int miles = 4;
  static const int yards = 5;
  static const int feet = 6;
  static const int inches = 7;
}

/// Temperature unit enum matching C ABI.
class CTempUnit {
  static const int celsius = 0;
  static const int fahrenheit = 1;
  static const int kelvin = 2;
}

// ============================================================================
// Native function type definitions (C signatures)
// ============================================================================

// --- Lifecycle ---
typedef ProvenInitC = Int32 Function();
typedef ProvenDeinitC = Void Function();
typedef ProvenIsInitializedC = Bool Function();
typedef ProvenAbiVersionC = Uint32 Function();

// --- String management ---
typedef ProvenFreeStringC = Void Function(Pointer<Utf8>);

// --- SafeMath ---
typedef ProvenMathDivC = IntResult Function(Int64, Int64);
typedef ProvenMathModC = IntResult Function(Int64, Int64);
typedef ProvenMathAddCheckedC = IntResult Function(Int64, Int64);
typedef ProvenMathSubCheckedC = IntResult Function(Int64, Int64);
typedef ProvenMathMulCheckedC = IntResult Function(Int64, Int64);
typedef ProvenMathAbsSafeC = IntResult Function(Int64);
typedef ProvenMathClampC = Int64 Function(Int64, Int64, Int64);
typedef ProvenMathPowCheckedC = IntResult Function(Int64, Uint32);

// --- SafeString ---
typedef ProvenStringIsValidUtf8C = BoolResult Function(Pointer<Uint8>, Size);
typedef ProvenStringEscapeSqlC = StringResult Function(Pointer<Uint8>, Size);
typedef ProvenStringEscapeHtmlC = StringResult Function(Pointer<Uint8>, Size);
typedef ProvenStringEscapeJsC = StringResult Function(Pointer<Uint8>, Size);

// --- SafePath ---
typedef ProvenPathHasTraversalC = BoolResult Function(Pointer<Uint8>, Size);
typedef ProvenPathSanitizeFilenameC = StringResult Function(
    Pointer<Uint8>, Size);

// --- SafeCrypto ---
typedef ProvenCryptoConstantTimeEqC = BoolResult Function(
    Pointer<Uint8>, Size, Pointer<Uint8>, Size);
typedef ProvenCryptoRandomBytesC = Int32 Function(Pointer<Uint8>, Size);

// --- SafeEmail ---
typedef ProvenEmailIsValidC = BoolResult Function(Pointer<Uint8>, Size);

// --- SafeNetwork ---
typedef ProvenNetworkParseIPv4C = IPv4Result Function(Pointer<Uint8>, Size);
typedef ProvenNetworkIPv4IsPrivateC = Bool Function(IPv4Address);
typedef ProvenNetworkIPv4IsLoopbackC = Bool Function(IPv4Address);

// --- SafeFloat ---
typedef ProvenFloatDivC = FloatResult Function(Double, Double);
typedef ProvenFloatIsFiniteC = Bool Function(Double);
typedef ProvenFloatIsNanC = Bool Function(Double);
typedef ProvenFloatSqrtC = FloatResult Function(Double);
typedef ProvenFloatLnC = FloatResult Function(Double);

// --- SafeHex ---
typedef ProvenHexEncodeC = StringResult Function(Pointer<Uint8>, Size, Bool);
typedef ProvenHexDecodeC = HexDecodeResult Function(Pointer<Uint8>, Size);
typedef ProvenHexFreeC = Void Function(Pointer<HexDecodeResult>);

// --- SafeUUID ---
typedef ProvenUuidV4C = UUIDResult Function();
typedef ProvenUuidToStringC = StringResult Function(UUID);
typedef ProvenUuidParseC = UUIDResult Function(Pointer<Uint8>, Size);
typedef ProvenUuidIsNilC = Bool Function(UUID);
typedef ProvenUuidVersionC = Uint8 Function(UUID);

// --- SafeJson ---
typedef ProvenJsonIsValidC = BoolResult Function(Pointer<Uint8>, Size);

// --- SafeColor ---
typedef ProvenColorParseHexC = ColorParseResult Function(Pointer<Uint8>, Size);
typedef ProvenColorRgbToHslC = HSLColor Function(RGBColor);
typedef ProvenColorToHexC = StringResult Function(RGBColor);

// --- SafeAngle ---
typedef ProvenAngleDegToRadC = Double Function(Double);
typedef ProvenAngleRadToDegC = Double Function(Double);
typedef ProvenAngleNormalizeDegreesC = Double Function(Double);
typedef ProvenAngleNormalizeRadiansC = Double Function(Double);

// --- SafeUnit ---
typedef ProvenUnitConvertLengthC = FloatResult Function(Double, Int32, Int32);
typedef ProvenUnitConvertTempC = FloatResult Function(Double, Int32, Int32);

// --- SafeVersion ---
typedef ProvenVersionParseC = VersionResult Function(Pointer<Uint8>, Size);
typedef ProvenVersionCompareC = Int32 Function(
    CSemanticVersion, CSemanticVersion);
typedef ProvenVersionFreeC = Void Function(Pointer<CSemanticVersion>);

// --- SafeDateTime ---
typedef ProvenDatetimeParseC = DateTimeResult Function(Pointer<Uint8>, Size);
typedef ProvenDatetimeFormatIso8601C = StringResult Function(CDateTime);
typedef ProvenDatetimeIsLeapYearC = Bool Function(Int32);
typedef ProvenDatetimeDaysInMonthC = Uint8 Function(Int32, Uint8);

// --- SafeCurrency ---
typedef ProvenCurrencyParseC = CCurrencyResult Function(Pointer<Uint8>, Size);
typedef ProvenCurrencyFormatC = StringResult Function(
    Int64, Array<Uint8>, Uint8);

// --- SafePhone ---
typedef ProvenPhoneParseC = CPhoneResult Function(Pointer<Uint8>, Size);
typedef ProvenPhoneFormatE164C = StringResult Function(Uint16, Uint64);

// ============================================================================
// Dart function type definitions (for lookup)
// ============================================================================

// --- Lifecycle ---
typedef ProvenInitDart = int Function();
typedef ProvenDeinitDart = void Function();
typedef ProvenIsInitializedDart = bool Function();
typedef ProvenAbiVersionDart = int Function();

// --- String management ---
typedef ProvenFreeStringDart = void Function(Pointer<Utf8>);

// --- SafeMath ---
typedef ProvenMathDivDart = IntResult Function(int, int);
typedef ProvenMathModDart = IntResult Function(int, int);
typedef ProvenMathAddCheckedDart = IntResult Function(int, int);
typedef ProvenMathSubCheckedDart = IntResult Function(int, int);
typedef ProvenMathMulCheckedDart = IntResult Function(int, int);
typedef ProvenMathAbsSafeDart = IntResult Function(int);
typedef ProvenMathClampDart = int Function(int, int, int);
typedef ProvenMathPowCheckedDart = IntResult Function(int, int);

// --- SafeString ---
typedef ProvenStringIsValidUtf8Dart = BoolResult Function(
    Pointer<Uint8>, int);
typedef ProvenStringEscapeSqlDart = StringResult Function(
    Pointer<Uint8>, int);
typedef ProvenStringEscapeHtmlDart = StringResult Function(
    Pointer<Uint8>, int);
typedef ProvenStringEscapeJsDart = StringResult Function(Pointer<Uint8>, int);

// --- SafePath ---
typedef ProvenPathHasTraversalDart = BoolResult Function(
    Pointer<Uint8>, int);
typedef ProvenPathSanitizeFilenameDart = StringResult Function(
    Pointer<Uint8>, int);

// --- SafeCrypto ---
typedef ProvenCryptoConstantTimeEqDart = BoolResult Function(
    Pointer<Uint8>, int, Pointer<Uint8>, int);
typedef ProvenCryptoRandomBytesDart = int Function(Pointer<Uint8>, int);

// --- SafeEmail ---
typedef ProvenEmailIsValidDart = BoolResult Function(Pointer<Uint8>, int);

// --- SafeNetwork ---
typedef ProvenNetworkParseIPv4Dart = IPv4Result Function(
    Pointer<Uint8>, int);
typedef ProvenNetworkIPv4IsPrivateDart = bool Function(IPv4Address);
typedef ProvenNetworkIPv4IsLoopbackDart = bool Function(IPv4Address);

// --- SafeFloat ---
typedef ProvenFloatDivDart = FloatResult Function(double, double);
typedef ProvenFloatIsFiniteDart = bool Function(double);
typedef ProvenFloatIsNanDart = bool Function(double);
typedef ProvenFloatSqrtDart = FloatResult Function(double);
typedef ProvenFloatLnDart = FloatResult Function(double);

// --- SafeHex ---
typedef ProvenHexEncodeDart = StringResult Function(Pointer<Uint8>, int, bool);
typedef ProvenHexDecodeDart = HexDecodeResult Function(Pointer<Uint8>, int);
typedef ProvenHexFreeDart = void Function(Pointer<HexDecodeResult>);

// --- SafeUUID ---
typedef ProvenUuidV4Dart = UUIDResult Function();
typedef ProvenUuidToStringDart = StringResult Function(UUID);
typedef ProvenUuidParseDart = UUIDResult Function(Pointer<Uint8>, int);
typedef ProvenUuidIsNilDart = bool Function(UUID);
typedef ProvenUuidVersionDart = int Function(UUID);

// --- SafeJson ---
typedef ProvenJsonIsValidDart = BoolResult Function(Pointer<Uint8>, int);

// --- SafeColor ---
typedef ProvenColorParseHexDart = ColorParseResult Function(
    Pointer<Uint8>, int);
typedef ProvenColorRgbToHslDart = HSLColor Function(RGBColor);
typedef ProvenColorToHexDart = StringResult Function(RGBColor);

// --- SafeAngle ---
typedef ProvenAngleDegToRadDart = double Function(double);
typedef ProvenAngleRadToDegDart = double Function(double);
typedef ProvenAngleNormalizeDegreesDart = double Function(double);
typedef ProvenAngleNormalizeRadiansDart = double Function(double);

// --- SafeUnit ---
typedef ProvenUnitConvertLengthDart = FloatResult Function(double, int, int);
typedef ProvenUnitConvertTempDart = FloatResult Function(double, int, int);

// --- SafeVersion ---
typedef ProvenVersionParseDart = VersionResult Function(Pointer<Uint8>, int);
typedef ProvenVersionCompareDart = int Function(
    CSemanticVersion, CSemanticVersion);
typedef ProvenVersionFreeDart = void Function(Pointer<CSemanticVersion>);

// --- SafeDateTime ---
typedef ProvenDatetimeParseDart = DateTimeResult Function(
    Pointer<Uint8>, int);
typedef ProvenDatetimeFormatIso8601Dart = StringResult Function(CDateTime);
typedef ProvenDatetimeIsLeapYearDart = bool Function(int);
typedef ProvenDatetimeDaysInMonthDart = int Function(int, int);

// --- SafeCurrency ---
typedef ProvenCurrencyParseDart = CCurrencyResult Function(
    Pointer<Uint8>, int);
typedef ProvenCurrencyFormatDart = StringResult Function(
    int, Array<Uint8>, int);

// --- SafePhone ---
typedef ProvenPhoneParseDart = CPhoneResult Function(Pointer<Uint8>, int);
typedef ProvenPhoneFormatE164Dart = StringResult Function(int, int);

// ============================================================================
// Library loader
// ============================================================================

/// Load the libproven shared library for the current platform.
DynamicLibrary _loadLibrary() {
  if (Platform.isLinux) return DynamicLibrary.open('libproven.so');
  if (Platform.isMacOS) return DynamicLibrary.open('libproven.dylib');
  if (Platform.isWindows) return DynamicLibrary.open('proven.dll');
  throw UnsupportedError('Unsupported platform: ${Platform.operatingSystem}');
}

/// The loaded libproven shared library.
final DynamicLibrary provenLib = _loadLibrary();

// ============================================================================
// Bound functions
// ============================================================================

// --- Lifecycle ---
final provenInit =
    provenLib.lookupFunction<ProvenInitC, ProvenInitDart>('proven_init');
final provenDeinit =
    provenLib.lookupFunction<ProvenDeinitC, ProvenDeinitDart>('proven_deinit');
final provenIsInitialized =
    provenLib.lookupFunction<ProvenIsInitializedC, ProvenIsInitializedDart>(
        'proven_is_initialized');
final provenAbiVersion =
    provenLib.lookupFunction<ProvenAbiVersionC, ProvenAbiVersionDart>(
        'proven_ffi_abi_version');

// --- String management ---
final provenFreeString =
    provenLib.lookupFunction<ProvenFreeStringC, ProvenFreeStringDart>(
        'proven_free_string');

// --- SafeMath ---
final provenMathDiv =
    provenLib.lookupFunction<ProvenMathDivC, ProvenMathDivDart>(
        'proven_math_div');
final provenMathMod =
    provenLib.lookupFunction<ProvenMathModC, ProvenMathModDart>(
        'proven_math_mod');
final provenMathAddChecked =
    provenLib.lookupFunction<ProvenMathAddCheckedC, ProvenMathAddCheckedDart>(
        'proven_math_add_checked');
final provenMathSubChecked =
    provenLib.lookupFunction<ProvenMathSubCheckedC, ProvenMathSubCheckedDart>(
        'proven_math_sub_checked');
final provenMathMulChecked =
    provenLib.lookupFunction<ProvenMathMulCheckedC, ProvenMathMulCheckedDart>(
        'proven_math_mul_checked');
final provenMathAbsSafe =
    provenLib.lookupFunction<ProvenMathAbsSafeC, ProvenMathAbsSafeDart>(
        'proven_math_abs_safe');
final provenMathClamp =
    provenLib.lookupFunction<ProvenMathClampC, ProvenMathClampDart>(
        'proven_math_clamp');
final provenMathPowChecked =
    provenLib.lookupFunction<ProvenMathPowCheckedC, ProvenMathPowCheckedDart>(
        'proven_math_pow_checked');

// --- SafeString ---
final provenStringIsValidUtf8 = provenLib
    .lookupFunction<ProvenStringIsValidUtf8C, ProvenStringIsValidUtf8Dart>(
        'proven_string_is_valid_utf8');
final provenStringEscapeSql = provenLib
    .lookupFunction<ProvenStringEscapeSqlC, ProvenStringEscapeSqlDart>(
        'proven_string_escape_sql');
final provenStringEscapeHtml = provenLib
    .lookupFunction<ProvenStringEscapeHtmlC, ProvenStringEscapeHtmlDart>(
        'proven_string_escape_html');
final provenStringEscapeJs =
    provenLib.lookupFunction<ProvenStringEscapeJsC, ProvenStringEscapeJsDart>(
        'proven_string_escape_js');

// --- SafePath ---
final provenPathHasTraversal = provenLib
    .lookupFunction<ProvenPathHasTraversalC, ProvenPathHasTraversalDart>(
        'proven_path_has_traversal');
final provenPathSanitizeFilename = provenLib.lookupFunction<
    ProvenPathSanitizeFilenameC,
    ProvenPathSanitizeFilenameDart>('proven_path_sanitize_filename');

// --- SafeCrypto ---
final provenCryptoConstantTimeEq = provenLib.lookupFunction<
    ProvenCryptoConstantTimeEqC,
    ProvenCryptoConstantTimeEqDart>('proven_crypto_constant_time_eq');
final provenCryptoRandomBytes = provenLib.lookupFunction<
    ProvenCryptoRandomBytesC,
    ProvenCryptoRandomBytesDart>('proven_crypto_random_bytes');

// --- SafeEmail ---
final provenEmailIsValid =
    provenLib.lookupFunction<ProvenEmailIsValidC, ProvenEmailIsValidDart>(
        'proven_email_is_valid');

// --- SafeNetwork ---
final provenNetworkParseIPv4 = provenLib
    .lookupFunction<ProvenNetworkParseIPv4C, ProvenNetworkParseIPv4Dart>(
        'proven_network_parse_ipv4');
final provenNetworkIPv4IsPrivate = provenLib.lookupFunction<
    ProvenNetworkIPv4IsPrivateC,
    ProvenNetworkIPv4IsPrivateDart>('proven_network_ipv4_is_private');
final provenNetworkIPv4IsLoopback = provenLib.lookupFunction<
    ProvenNetworkIPv4IsLoopbackC,
    ProvenNetworkIPv4IsLoopbackDart>('proven_network_ipv4_is_loopback');

// --- SafeFloat ---
final provenFloatDiv =
    provenLib.lookupFunction<ProvenFloatDivC, ProvenFloatDivDart>(
        'proven_float_div');
final provenFloatIsFinite =
    provenLib.lookupFunction<ProvenFloatIsFiniteC, ProvenFloatIsFiniteDart>(
        'proven_float_is_finite');
final provenFloatIsNan =
    provenLib.lookupFunction<ProvenFloatIsNanC, ProvenFloatIsNanDart>(
        'proven_float_is_nan');
final provenFloatSqrt =
    provenLib.lookupFunction<ProvenFloatSqrtC, ProvenFloatSqrtDart>(
        'proven_float_sqrt');
final provenFloatLn =
    provenLib.lookupFunction<ProvenFloatLnC, ProvenFloatLnDart>(
        'proven_float_ln');

// --- SafeHex ---
final provenHexEncode =
    provenLib.lookupFunction<ProvenHexEncodeC, ProvenHexEncodeDart>(
        'proven_hex_encode');
final provenHexDecode =
    provenLib.lookupFunction<ProvenHexDecodeC, ProvenHexDecodeDart>(
        'proven_hex_decode');
final provenHexFree =
    provenLib.lookupFunction<ProvenHexFreeC, ProvenHexFreeDart>(
        'proven_hex_free');

// --- SafeUUID ---
final provenUuidV4 =
    provenLib.lookupFunction<ProvenUuidV4C, ProvenUuidV4Dart>('proven_uuid_v4');
final provenUuidToString =
    provenLib.lookupFunction<ProvenUuidToStringC, ProvenUuidToStringDart>(
        'proven_uuid_to_string');
final provenUuidParse =
    provenLib.lookupFunction<ProvenUuidParseC, ProvenUuidParseDart>(
        'proven_uuid_parse');
final provenUuidIsNil =
    provenLib.lookupFunction<ProvenUuidIsNilC, ProvenUuidIsNilDart>(
        'proven_uuid_is_nil');
final provenUuidVersion =
    provenLib.lookupFunction<ProvenUuidVersionC, ProvenUuidVersionDart>(
        'proven_uuid_version');

// --- SafeJson ---
final provenJsonIsValid =
    provenLib.lookupFunction<ProvenJsonIsValidC, ProvenJsonIsValidDart>(
        'proven_json_is_valid');

// --- SafeColor ---
final provenColorParseHex =
    provenLib.lookupFunction<ProvenColorParseHexC, ProvenColorParseHexDart>(
        'proven_color_parse_hex');
final provenColorRgbToHsl =
    provenLib.lookupFunction<ProvenColorRgbToHslC, ProvenColorRgbToHslDart>(
        'proven_color_rgb_to_hsl');
final provenColorToHex =
    provenLib.lookupFunction<ProvenColorToHexC, ProvenColorToHexDart>(
        'proven_color_to_hex');

// --- SafeAngle ---
final provenAngleDegToRad =
    provenLib.lookupFunction<ProvenAngleDegToRadC, ProvenAngleDegToRadDart>(
        'proven_angle_deg_to_rad');
final provenAngleRadToDeg =
    provenLib.lookupFunction<ProvenAngleRadToDegC, ProvenAngleRadToDegDart>(
        'proven_angle_rad_to_deg');
final provenAngleNormalizeDegrees = provenLib.lookupFunction<
    ProvenAngleNormalizeDegreesC,
    ProvenAngleNormalizeDegreesDart>('proven_angle_normalize_degrees');
final provenAngleNormalizeRadians = provenLib.lookupFunction<
    ProvenAngleNormalizeRadiansC,
    ProvenAngleNormalizeRadiansDart>('proven_angle_normalize_radians');

// --- SafeUnit ---
final provenUnitConvertLength = provenLib
    .lookupFunction<ProvenUnitConvertLengthC, ProvenUnitConvertLengthDart>(
        'proven_unit_convert_length');
final provenUnitConvertTemp =
    provenLib.lookupFunction<ProvenUnitConvertTempC, ProvenUnitConvertTempDart>(
        'proven_unit_convert_temp');

// --- SafeVersion ---
final provenVersionParse =
    provenLib.lookupFunction<ProvenVersionParseC, ProvenVersionParseDart>(
        'proven_version_parse');
final provenVersionCompare =
    provenLib.lookupFunction<ProvenVersionCompareC, ProvenVersionCompareDart>(
        'proven_version_compare');
final provenVersionFree =
    provenLib.lookupFunction<ProvenVersionFreeC, ProvenVersionFreeDart>(
        'proven_version_free');

// --- SafeDateTime ---
final provenDatetimeParse =
    provenLib.lookupFunction<ProvenDatetimeParseC, ProvenDatetimeParseDart>(
        'proven_datetime_parse');
final provenDatetimeFormatIso8601 = provenLib.lookupFunction<
    ProvenDatetimeFormatIso8601C,
    ProvenDatetimeFormatIso8601Dart>('proven_datetime_format_iso8601');
final provenDatetimeIsLeapYear = provenLib.lookupFunction<
    ProvenDatetimeIsLeapYearC,
    ProvenDatetimeIsLeapYearDart>('proven_datetime_is_leap_year');
final provenDatetimeDaysInMonth = provenLib.lookupFunction<
    ProvenDatetimeDaysInMonthC,
    ProvenDatetimeDaysInMonthDart>('proven_datetime_days_in_month');

// --- SafeCurrency ---
final provenCurrencyParse =
    provenLib.lookupFunction<ProvenCurrencyParseC, ProvenCurrencyParseDart>(
        'proven_currency_parse');

// --- SafePhone ---
final provenPhoneParse =
    provenLib.lookupFunction<ProvenPhoneParseC, ProvenPhoneParseDart>(
        'proven_phone_parse');
final provenPhoneFormatE164 =
    provenLib.lookupFunction<ProvenPhoneFormatE164C, ProvenPhoneFormatE164Dart>(
        'proven_phone_format_e164');

// ============================================================================
// Marshaling helpers
// ============================================================================

/// Call an FFI string function, convert result to Dart String, and free the
/// C-allocated string. Returns null on error.
String? callStringFfi(StringResult result) {
  if (result.status != ProvenStatus.ok) return null;
  if (result.value == nullptr) return null;
  try {
    final dartString = result.value.toDartString(length: result.length);
    return dartString;
  } finally {
    provenFreeString(result.value);
  }
}

/// Convert a Dart string to a native UTF-8 pointer for FFI calls.
/// Caller must free the returned pointer using calloc.free().
(Pointer<Uint8>, int) toNativeUtf8Bytes(String input) {
  final units = input.toNativeUtf8();
  final len = input.length; // byte length for ASCII; use units.length for full
  // Cast Pointer<Utf8> to Pointer<Uint8> for the C API
  return (units.cast<Uint8>(), units.length);
}
