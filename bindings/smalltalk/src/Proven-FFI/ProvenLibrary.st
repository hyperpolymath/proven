" SPDX-License-Identifier: PMPL-1.0-or-later
  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> "

"
ProvenLibrary - FFI library class for libproven.

This class provides the shared library handle and all FFI function declarations
for the formally verified Proven library. All computation is performed in the
Idris 2 core via the Zig FFI bridge; this class is a thin declaration layer.

Memory management rules:
  - ProvenStringResult values must be freed with proven_free_string().
  - Integer, boolean, and float results do not require freeing.
  - URL results must be freed with proven_url_free().
  - Hex decode results must be freed with proven_hex_free().
"

Class {
	#name : #ProvenLibrary,
	#superclass : #FFILibrary,
	#classVars : [
		'StatusDescriptions'
	],
	#category : #'Proven-FFI'
}

{ #category : #'library path' }
ProvenLibrary >> unixLibraryName [
	"Return the path to the shared library on Unix/Linux."

	^ FFILibraryFinder findLibrary: 'libproven.so'
]

{ #category : #'library path' }
ProvenLibrary >> macLibraryName [
	"Return the path to the shared library on macOS."

	^ FFILibraryFinder findLibrary: 'libproven.dylib'
]

{ #category : #'library path' }
ProvenLibrary >> win32LibraryName [
	"Return the path to the shared library on Windows."

	^ 'proven.dll'
]

{ #category : #'class initialization' }
ProvenLibrary class >> initialize [
	"Initialize status code descriptions."

	StatusDescriptions := Dictionary new.
	StatusDescriptions
		at: 0 put: 'OK';
		at: -1 put: 'Null pointer';
		at: -2 put: 'Invalid argument';
		at: -3 put: 'Overflow';
		at: -4 put: 'Underflow';
		at: -5 put: 'Division by zero';
		at: -6 put: 'Parse failure';
		at: -7 put: 'Validation failed';
		at: -8 put: 'Out of bounds';
		at: -9 put: 'Encoding error';
		at: -10 put: 'Allocation failed';
		at: -99 put: 'Not implemented'
]

{ #category : #'status codes' }
ProvenLibrary class >> statusOk [
	^ 0
]

{ #category : #'status codes' }
ProvenLibrary class >> errNullPointer [
	^ -1
]

{ #category : #'status codes' }
ProvenLibrary class >> errInvalidArgument [
	^ -2
]

{ #category : #'status codes' }
ProvenLibrary class >> errOverflow [
	^ -3
]

{ #category : #'status codes' }
ProvenLibrary class >> errUnderflow [
	^ -4
]

{ #category : #'status codes' }
ProvenLibrary class >> errDivisionByZero [
	^ -5
]

{ #category : #'status codes' }
ProvenLibrary class >> errParseFailure [
	^ -6
]

{ #category : #'status codes' }
ProvenLibrary class >> errValidationFailed [
	^ -7
]

{ #category : #'status codes' }
ProvenLibrary class >> errOutOfBounds [
	^ -8
]

{ #category : #'status codes' }
ProvenLibrary class >> errEncodingError [
	^ -9
]

{ #category : #'status codes' }
ProvenLibrary class >> errAllocationFailed [
	^ -10
]

{ #category : #'status codes' }
ProvenLibrary class >> errNotImplemented [
	^ -99
]

{ #category : #'status codes' }
ProvenLibrary class >> descriptionForStatus: anInteger [
	"Return a human-readable description for a status code."

	^ StatusDescriptions
		at: anInteger
		ifAbsent: [ 'Unknown error (', anInteger printString, ')' ]
]

{ #category : #'status codes' }
ProvenLibrary class >> isSuccess: anInteger [
	"Return true if the status code indicates success."

	^ anInteger = 0
]

{ #category : #lifecycle }
ProvenLibrary >> provenInit [
	"Initialize the Proven runtime. Must be called before any other function.
	 Delegates to proven_init() in libproven."

	^ self ffiCall: #( int32 proven_init ( void ) )
]

{ #category : #lifecycle }
ProvenLibrary >> provenDeinit [
	"Shut down the Proven runtime. Call when done using the library.
	 Delegates to proven_deinit() in libproven."

	^ self ffiCall: #( void proven_deinit ( void ) )
]

{ #category : #lifecycle }
ProvenLibrary >> provenIsInitialized [
	"Check if the Proven runtime is initialized.
	 Delegates to proven_is_initialized() in libproven."

	^ self ffiCall: #( bool proven_is_initialized ( void ) )
]

{ #category : #memory }
ProvenLibrary >> provenFreeString: aPointer [
	"Free a string allocated by Proven functions. Safe to pass NULL.
	 Delegates to proven_free_string() in libproven."

	^ self ffiCall: #( void proven_free_string ( void* aPointer ) )
]

{ #category : #version }
ProvenLibrary >> provenFfiAbiVersion [
	"Get FFI ABI version for compatibility checking.
	 Delegates to proven_ffi_abi_version() in libproven."

	^ self ffiCall: #( uint32 proven_ffi_abi_version ( void ) )
]

{ #category : #version }
ProvenLibrary >> provenVersionMajor [
	"Get major version number.
	 Delegates to proven_version_major() in libproven."

	^ self ffiCall: #( uint32 proven_version_major ( void ) )
]

{ #category : #version }
ProvenLibrary >> provenVersionMinor [
	"Get minor version number.
	 Delegates to proven_version_minor() in libproven."

	^ self ffiCall: #( uint32 proven_version_minor ( void ) )
]

{ #category : #version }
ProvenLibrary >> provenVersionPatch [
	"Get patch version number.
	 Delegates to proven_version_patch() in libproven."

	^ self ffiCall: #( uint32 proven_version_patch ( void ) )
]

{ #category : #version }
ProvenLibrary >> provenModuleCount [
	"Get total module count.
	 Delegates to proven_module_count() in libproven."

	^ self ffiCall: #( uint32 proven_module_count ( void ) )
]

{ #category : #'safe math' }
ProvenLibrary >> provenMathAddChecked: a b: b [
	"Checked addition with overflow detection.
	 Delegates to proven_math_add_checked() in libproven."

	^ self ffiCall: #( ProvenIntResult proven_math_add_checked ( int64 a, int64 b ) )
]

{ #category : #'safe math' }
ProvenLibrary >> provenMathSubChecked: a b: b [
	"Checked subtraction with underflow detection.
	 Delegates to proven_math_sub_checked() in libproven."

	^ self ffiCall: #( ProvenIntResult proven_math_sub_checked ( int64 a, int64 b ) )
]

{ #category : #'safe math' }
ProvenLibrary >> provenMathMulChecked: a b: b [
	"Checked multiplication with overflow detection.
	 Delegates to proven_math_mul_checked() in libproven."

	^ self ffiCall: #( ProvenIntResult proven_math_mul_checked ( int64 a, int64 b ) )
]

{ #category : #'safe math' }
ProvenLibrary >> provenMathDiv: numerator denominator: denominator [
	"Safe integer division. Returns error on division by zero.
	 Delegates to proven_math_div() in libproven."

	^ self ffiCall: #( ProvenIntResult proven_math_div ( int64 numerator, int64 denominator ) )
]

{ #category : #'safe math' }
ProvenLibrary >> provenMathMod: numerator denominator: denominator [
	"Safe modulo operation. Returns error on division by zero.
	 Delegates to proven_math_mod() in libproven."

	^ self ffiCall: #( ProvenIntResult proven_math_mod ( int64 numerator, int64 denominator ) )
]

{ #category : #'safe math' }
ProvenLibrary >> provenMathAbsSafe: n [
	"Safe absolute value. Returns error for INT64_MIN.
	 Delegates to proven_math_abs_safe() in libproven."

	^ self ffiCall: #( ProvenIntResult proven_math_abs_safe ( int64 n ) )
]

{ #category : #'safe math' }
ProvenLibrary >> provenMathClampLo: lo hi: hi value: value [
	"Clamp value to [lo, hi] range.
	 Delegates to proven_math_clamp() in libproven."

	^ self ffiCall: #( int64 proven_math_clamp ( int64 lo, int64 hi, int64 value ) )
]

{ #category : #'safe math' }
ProvenLibrary >> provenMathPowChecked: base exp: exp [
	"Integer exponentiation with overflow checking.
	 Delegates to proven_math_pow_checked() in libproven."

	^ self ffiCall: #( ProvenIntResult proven_math_pow_checked ( int64 base, uint32 exp ) )
]

{ #category : #'safe string' }
ProvenLibrary >> provenStringIsValidUtf8: ptr len: len [
	"Check if bytes are valid UTF-8.
	 Delegates to proven_string_is_valid_utf8() in libproven."

	^ self ffiCall: #( ProvenBoolResult proven_string_is_valid_utf8 ( void* ptr, size_t len ) )
]

{ #category : #'safe string' }
ProvenLibrary >> provenStringEscapeSql: ptr len: len [
	"Escape string for SQL (single quotes).
	 Delegates to proven_string_escape_sql() in libproven.
	 Caller must free the returned string via proven_free_string()."

	^ self ffiCall: #( ProvenStringResult proven_string_escape_sql ( void* ptr, size_t len ) )
]

{ #category : #'safe string' }
ProvenLibrary >> provenStringEscapeHtml: ptr len: len [
	"Escape string for HTML (prevents XSS).
	 Delegates to proven_string_escape_html() in libproven.
	 Caller must free the returned string via proven_free_string()."

	^ self ffiCall: #( ProvenStringResult proven_string_escape_html ( void* ptr, size_t len ) )
]

{ #category : #'safe string' }
ProvenLibrary >> provenStringEscapeJs: ptr len: len [
	"Escape string for JavaScript string literals.
	 Delegates to proven_string_escape_js() in libproven.
	 Caller must free the returned string via proven_free_string()."

	^ self ffiCall: #( ProvenStringResult proven_string_escape_js ( void* ptr, size_t len ) )
]

{ #category : #'safe path' }
ProvenLibrary >> provenPathHasTraversal: ptr len: len [
	"Check if path contains directory traversal sequences.
	 Delegates to proven_path_has_traversal() in libproven."

	^ self ffiCall: #( ProvenBoolResult proven_path_has_traversal ( void* ptr, size_t len ) )
]

{ #category : #'safe path' }
ProvenLibrary >> provenPathSanitizeFilename: ptr len: len [
	"Sanitize a filename by removing dangerous characters.
	 Delegates to proven_path_sanitize_filename() in libproven.
	 Caller must free the returned string via proven_free_string()."

	^ self ffiCall: #( ProvenStringResult proven_path_sanitize_filename ( void* ptr, size_t len ) )
]

{ #category : #'safe email' }
ProvenLibrary >> provenEmailIsValid: ptr len: len [
	"Validate email address (RFC 5321 simplified).
	 Delegates to proven_email_is_valid() in libproven."

	^ self ffiCall: #( ProvenBoolResult proven_email_is_valid ( void* ptr, size_t len ) )
]

{ #category : #'safe url' }
ProvenLibrary >> provenUrlParse: ptr len: len [
	"Parse a URL into components.
	 Delegates to proven_url_parse() in libproven.
	 Caller must free with proven_url_free()."

	^ self ffiCall: #( ProvenUrlResult proven_url_parse ( void* ptr, size_t len ) )
]

{ #category : #'safe url' }
ProvenLibrary >> provenUrlFree: componentsPtr [
	"Free URL components allocated by proven_url_parse().
	 Delegates to proven_url_free() in libproven."

	^ self ffiCall: #( void proven_url_free ( void* componentsPtr ) )
]

{ #category : #'safe network' }
ProvenLibrary >> provenNetworkParseIpv4: ptr len: len [
	"Parse IPv4 address string.
	 Delegates to proven_network_parse_ipv4() in libproven."

	^ self ffiCall: #( ProvenIPv4Result proven_network_parse_ipv4 ( void* ptr, size_t len ) )
]

{ #category : #'safe network' }
ProvenLibrary >> provenNetworkIpv4IsPrivate: addr [
	"Check if IPv4 address is private (RFC 1918).
	 Delegates to proven_network_ipv4_is_private() in libproven."

	^ self ffiCall: #( bool proven_network_ipv4_is_private ( ProvenIPv4Address addr ) )
]

{ #category : #'safe network' }
ProvenLibrary >> provenNetworkIpv4IsLoopback: addr [
	"Check if IPv4 address is loopback (127.0.0.0/8).
	 Delegates to proven_network_ipv4_is_loopback() in libproven."

	^ self ffiCall: #( bool proven_network_ipv4_is_loopback ( ProvenIPv4Address addr ) )
]

{ #category : #'safe crypto' }
ProvenLibrary >> provenCryptoConstantTimeEq: ptr1 len1: len1 ptr2: ptr2 len2: len2 [
	"Constant-time byte comparison (timing-attack safe).
	 Delegates to proven_crypto_constant_time_eq() in libproven."

	^ self ffiCall: #( ProvenBoolResult proven_crypto_constant_time_eq (
		void* ptr1, size_t len1, void* ptr2, size_t len2 ) )
]

{ #category : #'safe crypto' }
ProvenLibrary >> provenCryptoRandomBytes: ptr len: len [
	"Fill buffer with cryptographically secure random bytes.
	 Delegates to proven_crypto_random_bytes() in libproven."

	^ self ffiCall: #( int32 proven_crypto_random_bytes ( void* ptr, size_t len ) )
]

{ #category : #'safe json' }
ProvenLibrary >> provenJsonIsValid: ptr len: len [
	"Check if string is valid JSON.
	 Delegates to proven_json_is_valid() in libproven."

	^ self ffiCall: #( ProvenBoolResult proven_json_is_valid ( void* ptr, size_t len ) )
]

{ #category : #'safe json' }
ProvenLibrary >> provenJsonGetType: ptr len: len [
	"Get JSON value type at root level.
	 Delegates to proven_json_get_type() in libproven."

	^ self ffiCall: #( int32 proven_json_get_type ( void* ptr, size_t len ) )
]

{ #category : #'safe datetime' }
ProvenLibrary >> provenDatetimeParse: ptr len: len [
	"Parse ISO 8601 date string.
	 Delegates to proven_datetime_parse() in libproven."

	^ self ffiCall: #( ProvenDateTimeResult proven_datetime_parse ( void* ptr, size_t len ) )
]

{ #category : #'safe datetime' }
ProvenLibrary >> provenDatetimeFormatIso8601: dt [
	"Format DateTime as ISO 8601 string.
	 Delegates to proven_datetime_format_iso8601() in libproven.
	 Caller must free the returned string via proven_free_string()."

	^ self ffiCall: #( ProvenStringResult proven_datetime_format_iso8601 ( ProvenDateTime dt ) )
]

{ #category : #'safe datetime' }
ProvenLibrary >> provenDatetimeIsLeapYear: year [
	"Check if year is a leap year.
	 Delegates to proven_datetime_is_leap_year() in libproven."

	^ self ffiCall: #( bool proven_datetime_is_leap_year ( int32 year ) )
]

{ #category : #'safe datetime' }
ProvenLibrary >> provenDatetimeDaysInMonth: year month: month [
	"Get number of days in a month.
	 Delegates to proven_datetime_days_in_month() in libproven."

	^ self ffiCall: #( uint8 proven_datetime_days_in_month ( int32 year, uint8 month ) )
]

{ #category : #'safe float' }
ProvenLibrary >> provenFloatDiv: a b: b [
	"Safe floating-point division.
	 Delegates to proven_float_div() in libproven."

	^ self ffiCall: #( ProvenFloatResult proven_float_div ( double a, double b ) )
]

{ #category : #'safe float' }
ProvenLibrary >> provenFloatIsFinite: x [
	"Check if float is finite (not NaN or Inf).
	 Delegates to proven_float_is_finite() in libproven."

	^ self ffiCall: #( bool proven_float_is_finite ( double x ) )
]

{ #category : #'safe float' }
ProvenLibrary >> provenFloatIsNaN: x [
	"Check if float is NaN.
	 Delegates to proven_float_is_nan() in libproven."

	^ self ffiCall: #( bool proven_float_is_nan ( double x ) )
]

{ #category : #'safe float' }
ProvenLibrary >> provenFloatSqrt: x [
	"Safe square root. Returns error for negative input.
	 Delegates to proven_float_sqrt() in libproven."

	^ self ffiCall: #( ProvenFloatResult proven_float_sqrt ( double x ) )
]

{ #category : #'safe float' }
ProvenLibrary >> provenFloatLn: x [
	"Safe natural logarithm. Returns error for non-positive input.
	 Delegates to proven_float_ln() in libproven."

	^ self ffiCall: #( ProvenFloatResult proven_float_ln ( double x ) )
]

{ #category : #'safe hex' }
ProvenLibrary >> provenHexEncode: ptr len: len uppercase: uppercase [
	"Encode bytes to hex string.
	 Delegates to proven_hex_encode() in libproven.
	 Caller must free the returned string via proven_free_string()."

	^ self ffiCall: #( ProvenStringResult proven_hex_encode ( void* ptr, size_t len, bool uppercase ) )
]

{ #category : #'safe hex' }
ProvenLibrary >> provenHexDecode: ptr len: len [
	"Decode hex string to bytes.
	 Delegates to proven_hex_decode() in libproven.
	 Caller must free via proven_hex_free()."

	^ self ffiCall: #( ProvenHexDecodeResult proven_hex_decode ( void* ptr, size_t len ) )
]

{ #category : #'safe hex' }
ProvenLibrary >> provenHexFree: resultPtr [
	"Free hex decode result.
	 Delegates to proven_hex_free() in libproven."

	^ self ffiCall: #( void proven_hex_free ( void* resultPtr ) )
]

{ #category : #'safe checksum' }
ProvenLibrary >> provenChecksumCrc32: ptr len: len [
	"Calculate CRC32 checksum.
	 Delegates to proven_checksum_crc32() in libproven."

	^ self ffiCall: #( ProvenIntResult proven_checksum_crc32 ( void* ptr, size_t len ) )
]

{ #category : #'safe checksum' }
ProvenLibrary >> provenChecksumVerifyCrc32: ptr len: len expected: expected [
	"Verify CRC32 matches expected value.
	 Delegates to proven_checksum_verify_crc32() in libproven."

	^ self ffiCall: #( ProvenBoolResult proven_checksum_verify_crc32 ( void* ptr, size_t len, uint32 expected ) )
]

{ #category : #'http encoding' }
ProvenLibrary >> provenHttpUrlEncode: ptr len: len [
	"URL-encode a string (RFC 3986 percent encoding).
	 Delegates to proven_http_url_encode() in libproven.
	 Caller must free the returned string via proven_free_string()."

	^ self ffiCall: #( ProvenStringResult proven_http_url_encode ( void* ptr, size_t len ) )
]

{ #category : #'http encoding' }
ProvenLibrary >> provenHttpUrlDecode: ptr len: len [
	"URL-decode a percent-encoded string.
	 Delegates to proven_http_url_decode() in libproven.
	 Caller must free the returned string via proven_free_string()."

	^ self ffiCall: #( ProvenStringResult proven_http_url_decode ( void* ptr, size_t len ) )
]
