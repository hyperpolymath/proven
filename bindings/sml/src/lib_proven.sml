(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(* lib_proven.sml -- Raw C FFI declarations for libproven.
 *
 * This module provides the lowest-level Standard ML interface to libproven,
 * the formally verified safety library (Idris 2 core, Zig FFI bridge, C ABI).
 *
 * ALL computation is performed in verified Idris 2 code. This file contains
 * data marshaling only -- NO logic is reimplemented.
 *
 * Targets:
 *   - MLton:  Uses _import / _address pragmas and MLton.Pointer
 *   - SML/NJ: Uses the Unsafe.CInterface (loaded via CM)
 *
 * The MLton-specific code is conditionally compiled; SML/NJ users include
 * the .cm file instead of the .mlb file.
 *)

(* -------------------------------------------------------------------------- *)
(* Status Codes                                                               *)
(* -------------------------------------------------------------------------- *)

structure ProvenStatus =
struct
  type t = Int32.int

  val OK : t                   = 0 : Int32.int
  val ERR_NULL_POINTER : t     = ~1 : Int32.int
  val ERR_INVALID_ARGUMENT : t = ~2 : Int32.int
  val ERR_OVERFLOW : t         = ~3 : Int32.int
  val ERR_UNDERFLOW : t        = ~4 : Int32.int
  val ERR_DIVISION_BY_ZERO : t = ~5 : Int32.int
  val ERR_PARSE_FAILURE : t    = ~6 : Int32.int
  val ERR_VALIDATION_FAILED : t= ~7 : Int32.int
  val ERR_OUT_OF_BOUNDS : t    = ~8 : Int32.int
  val ERR_ENCODING_ERROR : t   = ~9 : Int32.int
  val ERR_ALLOCATION_FAILED : t= ~10 : Int32.int
  val ERR_NOT_IMPLEMENTED : t  = ~99 : Int32.int

  fun isOk (s : t) : bool = (s = OK)
  fun isFail (s : t) : bool = (s <> OK)

  fun toString (s : t) : string =
    case s of
      0  => "OK"
    | ~1 => "ERR_NULL_POINTER"
    | ~2 => "ERR_INVALID_ARGUMENT"
    | ~3 => "ERR_OVERFLOW"
    | ~4 => "ERR_UNDERFLOW"
    | ~5 => "ERR_DIVISION_BY_ZERO"
    | ~6 => "ERR_PARSE_FAILURE"
    | ~7 => "ERR_VALIDATION_FAILED"
    | ~8 => "ERR_OUT_OF_BOUNDS"
    | ~9 => "ERR_ENCODING_ERROR"
    | ~10 => "ERR_ALLOCATION_FAILED"
    | ~99 => "ERR_NOT_IMPLEMENTED"
    | _  => "UNKNOWN(" ^ Int32.toString s ^ ")"
end

(* -------------------------------------------------------------------------- *)
(* Result Record Types                                                        *)
(* -------------------------------------------------------------------------- *)

(* These mirror the C structs declared in proven.h.
 * MLton flattens structs returned by _import into tuples or records
 * accessed via MLton.Pointer offsets.  We use pointer-based access for
 * all struct returns to be safe across both compilers. *)

structure ProvenResult =
struct
  (* Integer result: { status: int32, value: int64 } *)
  type int_result = { status : Int32.int, value : Int64.int }

  (* Boolean result: { status: int32, value: bool } *)
  type bool_result = { status : Int32.int, value : bool }

  (* String result: { status: int32, ptr: pointer, len: word } *)
  type string_result = { status : Int32.int, ptr : MLton.Pointer.t, len : Word64.word }

  (* Float result: { status: int32, value: real } *)
  type float_result = { status : Int32.int, value : Real64.real }

  (* IPv4 result: { status: int32, o1..o4: word8 } *)
  type ipv4_result = { status : Int32.int, o1 : Word8.word, o2 : Word8.word,
                        o3 : Word8.word, o4 : Word8.word }

  (* DateTime components *)
  type datetime = { year : Int32.int, month : Word8.word, day : Word8.word,
                    hour : Word8.word, minute : Word8.word, second : Word8.word,
                    nanosecond : Word32.word, tzOffsetMinutes : Int16.int }

  type datetime_result = { status : Int32.int, dt : datetime }
end

(* -------------------------------------------------------------------------- *)
(* Raw FFI Imports (MLton)                                                    *)
(* -------------------------------------------------------------------------- *)

(* All C functions are imported via MLton _import.  Each function returns its
 * result through a caller-allocated buffer (MLton.Pointer) to handle C struct
 * return values portably.
 *
 * Convention: We import the C function and then immediately marshal the raw
 * pointer data into SML records in the higher-level wrappers. *)

structure LibProven =
struct

  (* ---- Lifecycle --------------------------------------------------------- *)

  val init = _import "proven_init" public : unit -> Int32.int;
  val deinit = _import "proven_deinit" public : unit -> unit;
  val isInitialized = _import "proven_is_initialized" public : unit -> bool;

  (* ---- Memory Management ------------------------------------------------- *)

  val freeString = _import "proven_free_string" public : MLton.Pointer.t -> unit;

  (* ---- Version ----------------------------------------------------------- *)

  val ffiAbiVersion = _import "proven_ffi_abi_version" public : unit -> Word32.word;
  val versionMajor = _import "proven_version_major" public : unit -> Word32.word;
  val versionMinor = _import "proven_version_minor" public : unit -> Word32.word;
  val versionPatch = _import "proven_version_patch" public : unit -> Word32.word;
  val moduleCount = _import "proven_module_count" public : unit -> Word32.word;

  (* ---- SafeMath ---------------------------------------------------------- *)

  (* C struct returns are handled via pointer-based marshaling.
   * MLton can return structs from C, but the layout depends on ABI.
   * We use a pattern where we call the C function and read the struct
   * fields from the return via MLton's struct flattening.
   *
   * For ProvenIntResult { int32_t status; int64_t value; }:
   *   MLton flattens this to (Int32.int * Int64.int) when returned by value.
   *)

  val mathAddChecked =
    _import "proven_math_add_checked" public : Int64.int * Int64.int -> Int32.int * Int64.int;
  val mathSubChecked =
    _import "proven_math_sub_checked" public : Int64.int * Int64.int -> Int32.int * Int64.int;
  val mathMulChecked =
    _import "proven_math_mul_checked" public : Int64.int * Int64.int -> Int32.int * Int64.int;
  val mathDiv =
    _import "proven_math_div" public : Int64.int * Int64.int -> Int32.int * Int64.int;
  val mathMod =
    _import "proven_math_mod" public : Int64.int * Int64.int -> Int32.int * Int64.int;
  val mathAbsSafe =
    _import "proven_math_abs_safe" public : Int64.int -> Int32.int * Int64.int;
  val mathClamp =
    _import "proven_math_clamp" public : Int64.int * Int64.int * Int64.int -> Int64.int;
  val mathPowChecked =
    _import "proven_math_pow_checked" public : Int64.int * Word32.word -> Int32.int * Int64.int;

  (* ---- SafeString -------------------------------------------------------- *)

  val stringIsValidUtf8 =
    _import "proven_string_is_valid_utf8" public : Word8.word vector * Word64.word -> Int32.int * bool;
  val stringEscapeSql =
    _import "proven_string_escape_sql" public : Word8.word vector * Word64.word -> Int32.int * MLton.Pointer.t * Word64.word;
  val stringEscapeHtml =
    _import "proven_string_escape_html" public : Word8.word vector * Word64.word -> Int32.int * MLton.Pointer.t * Word64.word;
  val stringEscapeJs =
    _import "proven_string_escape_js" public : Word8.word vector * Word64.word -> Int32.int * MLton.Pointer.t * Word64.word;

  (* ---- SafePath ---------------------------------------------------------- *)

  val pathHasTraversal =
    _import "proven_path_has_traversal" public : Word8.word vector * Word64.word -> Int32.int * bool;
  val pathSanitizeFilename =
    _import "proven_path_sanitize_filename" public : Word8.word vector * Word64.word -> Int32.int * MLton.Pointer.t * Word64.word;

  (* ---- SafeEmail --------------------------------------------------------- *)

  val emailIsValid =
    _import "proven_email_is_valid" public : Word8.word vector * Word64.word -> Int32.int * bool;

  (* ---- SafeUrl ----------------------------------------------------------- *)

  (* URL parsing returns a complex struct; we use a helper C shim or read
   * fields via pointer offsets.  For the SML binding we provide a simpler
   * interface: parse returns a string result with the normalized URL, and
   * individual component accessors work on the opaque pointer. *)

  (* We import the URL parse which returns ProvenUrlResult by pointer. *)
  val urlParse_raw =
    _import "proven_url_parse" public : Word8.word vector * Word64.word -> MLton.Pointer.t;
  val urlFree =
    _import "proven_url_free" public : MLton.Pointer.t -> unit;

  (* ---- SafeNetwork ------------------------------------------------------- *)

  val networkParseIpv4 =
    _import "proven_network_parse_ipv4" public : Word8.word vector * Word64.word -> MLton.Pointer.t;
  val networkIpv4IsPrivate_raw =
    _import "proven_network_ipv4_is_private" public : Word8.word * Word8.word * Word8.word * Word8.word -> bool;
  val networkIpv4IsLoopback_raw =
    _import "proven_network_ipv4_is_loopback" public : Word8.word * Word8.word * Word8.word * Word8.word -> bool;

  (* ---- SafeCrypto -------------------------------------------------------- *)

  val cryptoConstantTimeEq =
    _import "proven_crypto_constant_time_eq" public :
      Word8.word vector * Word64.word * Word8.word vector * Word64.word -> Int32.int * bool;
  val cryptoRandomBytes =
    _import "proven_crypto_random_bytes" public : MLton.Pointer.t * Word64.word -> Int32.int;

  (* ---- SafeJson ---------------------------------------------------------- *)

  val jsonIsValid =
    _import "proven_json_is_valid" public : Word8.word vector * Word64.word -> Int32.int * bool;
  val jsonGetType =
    _import "proven_json_get_type" public : Word8.word vector * Word64.word -> Int32.int;

  (* ---- SafeDateTime ------------------------------------------------------ *)

  val datetimeParse_raw =
    _import "proven_datetime_parse" public : Word8.word vector * Word64.word -> MLton.Pointer.t;
  val datetimeFormatIso8601_raw =
    _import "proven_datetime_format_iso8601" public : MLton.Pointer.t -> Int32.int * MLton.Pointer.t * Word64.word;
  val datetimeIsLeapYear =
    _import "proven_datetime_is_leap_year" public : Int32.int -> bool;
  val datetimeDaysInMonth =
    _import "proven_datetime_days_in_month" public : Int32.int * Word8.word -> Word8.word;

  (* ---- SafeFloat --------------------------------------------------------- *)

  val floatDiv =
    _import "proven_float_div" public : Real64.real * Real64.real -> Int32.int * Real64.real;
  val floatIsFinite =
    _import "proven_float_is_finite" public : Real64.real -> bool;
  val floatIsNan =
    _import "proven_float_is_nan" public : Real64.real -> bool;
  val floatSqrt =
    _import "proven_float_sqrt" public : Real64.real -> Int32.int * Real64.real;
  val floatLn =
    _import "proven_float_ln" public : Real64.real -> Int32.int * Real64.real;

  (* ---- SafeHex ----------------------------------------------------------- *)

  val hexEncode =
    _import "proven_hex_encode" public : Word8.word vector * Word64.word * bool -> Int32.int * MLton.Pointer.t * Word64.word;
  val hexDecode_raw =
    _import "proven_hex_decode" public : Word8.word vector * Word64.word -> MLton.Pointer.t;
  val hexFree =
    _import "proven_hex_free" public : MLton.Pointer.t -> unit;

  (* ---- SafeChecksum ------------------------------------------------------ *)

  val checksumCrc32 =
    _import "proven_checksum_crc32" public : Word8.word vector * Word64.word -> Int32.int * Int64.int;
  val checksumVerifyCrc32 =
    _import "proven_checksum_verify_crc32" public : Word8.word vector * Word64.word * Word32.word -> Int32.int * bool;

  (* ---- SafeColor --------------------------------------------------------- *)

  val colorParseHex_raw =
    _import "proven_color_parse_hex" public : Word8.word vector * Word64.word -> MLton.Pointer.t;
  val colorToHex =
    _import "proven_color_to_hex" public : Word8.word * Word8.word * Word8.word -> Int32.int * MLton.Pointer.t * Word64.word;

  (* ---- SafeHeader -------------------------------------------------------- *)

  val headerHasCrlf =
    _import "proven_header_has_crlf" public : Word8.word vector * Word64.word -> Int32.int * bool;
  val headerIsValidName =
    _import "proven_header_is_valid_name" public : Word8.word vector * Word64.word -> Int32.int * bool;
  val headerIsDangerous =
    _import "proven_header_is_dangerous" public : Word8.word vector * Word64.word -> Int32.int * bool;
  val headerRender =
    _import "proven_header_render" public :
      Word8.word vector * Word64.word * Word8.word vector * Word64.word -> Int32.int * MLton.Pointer.t * Word64.word;

  (* ---- SafeHttp ---------------------------------------------------------- *)

  val httpUrlEncode =
    _import "proven_http_url_encode" public : Word8.word vector * Word64.word -> Int32.int * MLton.Pointer.t * Word64.word;
  val httpUrlDecode =
    _import "proven_http_url_decode" public : Word8.word vector * Word64.word -> Int32.int * MLton.Pointer.t * Word64.word;

  (* ---- SafeCalculator ---------------------------------------------------- *)

  val calculatorEval =
    _import "proven_calculator_eval" public : Word8.word vector * Word64.word -> Int32.int * Real64.real;

  (* ---- SafeAngle --------------------------------------------------------- *)

  val angleDegToRad =
    _import "proven_angle_deg_to_rad" public : Real64.real -> Real64.real;
  val angleRadToDeg =
    _import "proven_angle_rad_to_deg" public : Real64.real -> Real64.real;
  val angleNormalizeDegrees =
    _import "proven_angle_normalize_degrees" public : Real64.real -> Real64.real;
  val angleNormalizeRadians =
    _import "proven_angle_normalize_radians" public : Real64.real -> Real64.real;

  (* ---- SafeProbability --------------------------------------------------- *)

  val probabilityCreate =
    _import "proven_probability_create" public : Real64.real -> Real64.real;
  val probabilityAnd =
    _import "proven_probability_and" public : Real64.real * Real64.real -> Real64.real;
  val probabilityOrExclusive =
    _import "proven_probability_or_exclusive" public : Real64.real * Real64.real -> Real64.real;
  val probabilityNot =
    _import "proven_probability_not" public : Real64.real -> Real64.real;

  (* ---- SafeML (activation functions) ------------------------------------- *)

  val mlSigmoid =
    _import "proven_ml_sigmoid" public : Real64.real -> Real64.real;
  val mlRelu =
    _import "proven_ml_relu" public : Real64.real -> Real64.real;
  val mlLeakyRelu =
    _import "proven_ml_leaky_relu" public : Real64.real * Real64.real -> Real64.real;
  val mlClamp =
    _import "proven_ml_clamp" public : Real64.real * Real64.real * Real64.real -> Real64.real;

  (* ---- SafePassword ------------------------------------------------------ *)

  val passwordValidate_raw =
    _import "proven_password_validate" public : Word8.word vector * Word64.word -> MLton.Pointer.t;
  val passwordIsCommon =
    _import "proven_password_is_common" public : Word8.word vector * Word64.word -> bool;

  (* ---- SafeVersion ------------------------------------------------------- *)

  val versionParse_raw =
    _import "proven_version_parse" public : Word8.word vector * Word64.word -> MLton.Pointer.t;
  val versionCompare_raw =
    _import "proven_version_compare" public : MLton.Pointer.t * MLton.Pointer.t -> Int32.int;
  val versionFree =
    _import "proven_version_free" public : MLton.Pointer.t -> unit;

  (* ---- SafeGeo ----------------------------------------------------------- *)

  val geoValidate =
    _import "proven_geo_validate" public : Real64.real * Real64.real -> MLton.Pointer.t;
  val geoDistance =
    _import "proven_geo_distance" public :
      Real64.real * Real64.real * Real64.real * Real64.real -> Int32.int * Real64.real;

  (* ---- SafeUnit ---------------------------------------------------------- *)

  val unitConvertLength =
    _import "proven_unit_convert_length" public : Real64.real * Int32.int * Int32.int -> Int32.int * Real64.real;
  val unitConvertTemp =
    _import "proven_unit_convert_temp" public : Real64.real * Int32.int * Int32.int -> Int32.int * Real64.real;

end

(* -------------------------------------------------------------------------- *)
(* Internal Marshaling Helpers                                                *)
(* -------------------------------------------------------------------------- *)

structure ProvenMarshal =
struct

  (* Convert an SML string to a Word8 vector suitable for passing as
   * const uint8_t* to C functions. *)
  fun stringToBytes (s : string) : Word8.word vector =
    Vector.tabulate (String.size s, fn i => Word8.fromInt (Char.ord (String.sub (s, i))))

  (* Get the length of a string as Word64 for the size_t parameter. *)
  fun stringLen (s : string) : Word64.word =
    Word64.fromInt (String.size s)

  (* Read a C string from an MLton.Pointer.t and length, then free it
   * via proven_free_string.  Returns the SML string. *)
  fun readAndFreeString (ptr : MLton.Pointer.t, len : Word64.word) : string =
    let
      val n = Word64.toInt len
      val s = CharVector.tabulate (n, fn i =>
        Char.chr (Word8.toInt (MLton.Pointer.getWord8 (ptr, i))))
    in
      LibProven.freeString ptr;
      s
    end

  (* Unwrap an int result tuple into SOME value or NONE. *)
  fun unwrapInt (status : Int32.int, value : Int64.int) : Int64.int option =
    if ProvenStatus.isOk status then SOME value else NONE

  (* Unwrap a bool result tuple into SOME value or NONE. *)
  fun unwrapBool (status : Int32.int, value : bool) : bool option =
    if ProvenStatus.isOk status then SOME value else NONE

  (* Unwrap a float result tuple into SOME value or NONE. *)
  fun unwrapFloat (status : Int32.int, value : Real64.real) : Real64.real option =
    if ProvenStatus.isOk status then SOME value else NONE

  (* Unwrap a string result tuple into SOME string or NONE.
   * Frees the C string on success. *)
  fun unwrapString (status : Int32.int, ptr : MLton.Pointer.t, len : Word64.word) : string option =
    if ProvenStatus.isOk status andalso ptr <> MLton.Pointer.null then
      SOME (readAndFreeString (ptr, len))
    else
      NONE

end
