! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! proven.ffi - Low-level alien (FFI) declarations for libproven.
! All computation is performed in verified Idris 2 code via the Zig FFI
! layer. This vocabulary declares C function bindings; it does NOT
! reimplement any logic.

USING: alien alien.c-types alien.data alien.libraries alien.syntax
       kernel sequences ;
IN: proven.ffi

! Load the libproven shared library
"proven" "libproven.so" cdecl add-library

! ========================================================================
! Status codes (matching ProvenStatus enum)
! ========================================================================

CONSTANT: PROVEN-OK                     0
CONSTANT: PROVEN-ERR-NULL-POINTER      -1
CONSTANT: PROVEN-ERR-INVALID-ARGUMENT  -2
CONSTANT: PROVEN-ERR-OVERFLOW          -3
CONSTANT: PROVEN-ERR-UNDERFLOW         -4
CONSTANT: PROVEN-ERR-DIVISION-BY-ZERO  -5
CONSTANT: PROVEN-ERR-PARSE-FAILURE     -6
CONSTANT: PROVEN-ERR-VALIDATION-FAILED -7
CONSTANT: PROVEN-ERR-OUT-OF-BOUNDS     -8
CONSTANT: PROVEN-ERR-ENCODING-ERROR    -9
CONSTANT: PROVEN-ERR-ALLOCATION-FAILED -10
CONSTANT: PROVEN-ERR-NOT-IMPLEMENTED   -99

! ========================================================================
! Result struct types
! ========================================================================

! IntResult: { int32_t status; int64_t value; }
STRUCT: ProvenIntResult
    { status int }
    { value longlong } ;

! BoolResult: { int32_t status; int32_t value; }
STRUCT: ProvenBoolResult
    { status int }
    { value int } ;

! StringResult: { int32_t status; char* value; size_t length; }
STRUCT: ProvenStringResult
    { status int }
    { value c-string }
    { length ulong } ;

! FloatResult: { int32_t status; double value; }
STRUCT: ProvenFloatResult
    { status int }
    { value double } ;

! ========================================================================
! Lifecycle functions
! ========================================================================

LIBRARY: proven

FUNCTION: int proven_init ( )
FUNCTION: void proven_deinit ( )
FUNCTION: bool proven_is_initialized ( )
FUNCTION: uint proven_ffi_abi_version ( )
FUNCTION: uint proven_version_major ( )
FUNCTION: uint proven_version_minor ( )
FUNCTION: uint proven_version_patch ( )
FUNCTION: uint proven_module_count ( )

! ========================================================================
! Memory management
! ========================================================================

FUNCTION: void proven_free_string ( c-string ptr )

! ========================================================================
! SafeMath (8 functions)
! ========================================================================

FUNCTION: ProvenIntResult proven_math_add_checked ( longlong a, longlong b )
FUNCTION: ProvenIntResult proven_math_sub_checked ( longlong a, longlong b )
FUNCTION: ProvenIntResult proven_math_mul_checked ( longlong a, longlong b )
FUNCTION: ProvenIntResult proven_math_div ( longlong a, longlong b )
FUNCTION: ProvenIntResult proven_math_mod ( longlong a, longlong b )
FUNCTION: ProvenIntResult proven_math_abs_safe ( longlong n )
FUNCTION: longlong proven_math_clamp ( longlong lo, longlong hi, longlong value )
FUNCTION: ProvenIntResult proven_math_pow_checked ( longlong base, uint exp )

! ========================================================================
! SafeString (4 functions)
! ========================================================================

FUNCTION: ProvenBoolResult proven_string_is_valid_utf8 ( c-string ptr, ulong len )
FUNCTION: ProvenStringResult proven_string_escape_sql ( c-string ptr, ulong len )
FUNCTION: ProvenStringResult proven_string_escape_html ( c-string ptr, ulong len )
FUNCTION: ProvenStringResult proven_string_escape_js ( c-string ptr, ulong len )

! ========================================================================
! SafePath (2 functions)
! ========================================================================

FUNCTION: ProvenBoolResult proven_path_has_traversal ( c-string ptr, ulong len )
FUNCTION: ProvenStringResult proven_path_sanitize_filename ( c-string ptr, ulong len )

! ========================================================================
! SafeEmail (1 function)
! ========================================================================

FUNCTION: ProvenBoolResult proven_email_is_valid ( c-string ptr, ulong len )

! ========================================================================
! SafeNetwork (3 functions)
! ========================================================================

FUNCTION: ProvenBoolResult proven_network_parse_ipv4 ( c-string ptr, ulong len )

! ========================================================================
! SafeCrypto (2 functions)
! ========================================================================

FUNCTION: ProvenBoolResult proven_crypto_constant_time_eq (
    c-string p1, ulong l1, c-string p2, ulong l2 )
FUNCTION: int proven_crypto_random_bytes ( c-string ptr, ulong len )

! ========================================================================
! SafeJson (2 functions)
! ========================================================================

FUNCTION: ProvenBoolResult proven_json_is_valid ( c-string ptr, ulong len )
FUNCTION: int proven_json_get_type ( c-string ptr, ulong len )

! ========================================================================
! SafeFloat (5 functions)
! ========================================================================

FUNCTION: ProvenFloatResult proven_float_div ( double a, double b )
FUNCTION: bool proven_float_is_finite ( double x )
FUNCTION: bool proven_float_is_nan ( double x )
FUNCTION: ProvenFloatResult proven_float_sqrt ( double x )
FUNCTION: ProvenFloatResult proven_float_ln ( double x )

! ========================================================================
! SafeDateTime (4 functions)
! ========================================================================

FUNCTION: bool proven_datetime_is_leap_year ( int year )
FUNCTION: uchar proven_datetime_days_in_month ( int year, uchar month )

! ========================================================================
! SafeHex (2 functions)
! ========================================================================

FUNCTION: ProvenStringResult proven_hex_encode ( c-string ptr, ulong len, bool uppercase )

! ========================================================================
! SafeColor (1 function used directly)
! ========================================================================

FUNCTION: ProvenBoolResult proven_content_type_is_json (
    c-string sub, ulong sl, c-string suf, ulong fl )

! ========================================================================
! HTTP encoding (2 functions)
! ========================================================================

FUNCTION: ProvenStringResult proven_http_url_encode ( c-string ptr, ulong len )
FUNCTION: ProvenStringResult proven_http_url_decode ( c-string ptr, ulong len )
