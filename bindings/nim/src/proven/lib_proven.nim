# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Raw C FFI declarations for libproven.
# These map 1:1 to the C headers.  Higher-level Nim wrappers live in
# sibling safe_*.nim files.

{.passL: "-lproven".}

const ProvenDynlib = "libproven.so"

# ---------------------------------------------------------------------------
# Status codes
# ---------------------------------------------------------------------------
const
  PROVEN_OK* = 0'i32
  PROVEN_ERR_NULL_POINTER* = -1'i32
  PROVEN_ERR_INVALID_ARGUMENT* = -2'i32
  PROVEN_ERR_OVERFLOW* = -3'i32
  PROVEN_ERR_UNDERFLOW* = -4'i32
  PROVEN_ERR_DIVISION_BY_ZERO* = -5'i32
  PROVEN_ERR_PARSE_FAILURE* = -6'i32
  PROVEN_ERR_VALIDATION_FAILED* = -7'i32
  PROVEN_ERR_OUT_OF_BOUNDS* = -8'i32
  PROVEN_ERR_ENCODING_ERROR* = -9'i32
  PROVEN_ERR_ALLOCATION_FAILED* = -10'i32

# ---------------------------------------------------------------------------
# Core result types
# ---------------------------------------------------------------------------
type
  IntResult* {.importc: "ProvenIntResult", header: "proven.h".} = object
    status*: int32
    value*: int64

  BoolResult* {.importc: "ProvenBoolResult", header: "proven.h".} = object
    status*: int32
    value*: bool

  StringResult* {.importc: "ProvenStringResult", header: "proven.h".} = object
    status*: int32
    value*: cstring
    length*: csize_t

  FloatResult* {.importc: "ProvenFloatResult", header: "proven_types.h".} = object
    status*: int32
    value*: float64

  IPv4Address* {.importc: "ProvenIPv4Address", header: "safe_network.h".} = object
    octets*: array[4, uint8]

  IPv4Result* {.importc: "ProvenIPv4Result", header: "safe_network.h".} = object
    status*: int32
    address*: IPv4Address

  UrlComponents* {.importc: "ProvenUrlComponents", header: "safe_url.h".} = object
    scheme*: cstring
    scheme_len*: csize_t
    host*: cstring
    host_len*: csize_t
    port*: uint16
    has_port*: bool
    path*: cstring
    path_len*: csize_t
    query*: cstring
    query_len*: csize_t
    fragment*: cstring
    fragment_len*: csize_t

  UrlResult* {.importc: "ProvenUrlResult", header: "safe_url.h".} = object
    status*: int32
    components*: UrlComponents

  ProvenDateTime* {.importc: "ProvenDateTime", header: "safe_datetime.h".} = object
    year*: int32
    month*: uint8
    day*: uint8
    hour*: uint8
    minute*: uint8
    second*: uint8
    nanosecond*: uint32
    tz_offset_minutes*: int16

  DateTimeResult* {.importc: "ProvenDateTimeResult", header: "safe_datetime.h".} = object
    status*: int32
    datetime*: ProvenDateTime

  SemanticVersion* {.importc: "ProvenSemanticVersion", header: "safe_version.h".} = object
    major*: uint32
    minor*: uint32
    patch*: uint32
    prerelease_len*: csize_t
    prerelease*: cstring

  VersionResult* {.importc: "ProvenVersionResult", header: "safe_version.h".} = object
    status*: int32
    version*: SemanticVersion

  RGBColor* {.importc: "ProvenRGBColor", header: "safe_color.h".} = object
    r*: uint8
    g*: uint8
    b*: uint8

  HSLColor* {.importc: "ProvenHSLColor", header: "safe_color.h".} = object
    h*: float64
    s*: float64
    l*: float64

  ColorResult* {.importc: "ProvenColorResult", header: "safe_color.h".} = object
    status*: int32
    color*: RGBColor

  JsonType* = enum
    jtNull = 0, jtBool = 1, jtNumber = 2, jtString = 3,
    jtArray = 4, jtObject = 5, jtInvalid = -1

  LengthUnit* = enum
    luMeters = 0, luKilometers = 1, luCentimeters = 2, luMillimeters = 3,
    luFeet = 4, luInches = 5, luMiles = 6, luYards = 7

  TempUnit* = enum
    tuCelsius = 0, tuFahrenheit = 1, tuKelvin = 2

# ---------------------------------------------------------------------------
# Runtime
# ---------------------------------------------------------------------------
proc provenInit*(): int32 {.importc: "proven_init", dynlib: ProvenDynlib.}
proc provenDeinit*() {.importc: "proven_deinit", dynlib: ProvenDynlib.}
proc provenIsInitialized*(): bool {.importc: "proven_is_initialized", dynlib: ProvenDynlib.}
proc provenFfiAbiVersion*(): uint32 {.importc: "proven_ffi_abi_version", dynlib: ProvenDynlib.}
proc provenVersionMajor*(): uint32 {.importc: "proven_version_major", dynlib: ProvenDynlib.}
proc provenVersionMinor*(): uint32 {.importc: "proven_version_minor", dynlib: ProvenDynlib.}
proc provenVersionPatch*(): uint32 {.importc: "proven_version_patch", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# Memory
# ---------------------------------------------------------------------------
proc provenFreeString*(p: cstring) {.importc: "proven_free_string", dynlib: ProvenDynlib.}
proc provenUrlFree*(c: ptr UrlComponents) {.importc: "proven_url_free", dynlib: ProvenDynlib.}
proc provenVersionFree*(v: ptr SemanticVersion) {.importc: "proven_version_free", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeMath
# ---------------------------------------------------------------------------
proc provenMathDiv*(a, b: int64): IntResult {.importc: "proven_math_div", dynlib: ProvenDynlib.}
proc provenMathMod*(a, b: int64): IntResult {.importc: "proven_math_mod", dynlib: ProvenDynlib.}
proc provenMathAddChecked*(a, b: int64): IntResult {.importc: "proven_math_add_checked", dynlib: ProvenDynlib.}
proc provenMathSubChecked*(a, b: int64): IntResult {.importc: "proven_math_sub_checked", dynlib: ProvenDynlib.}
proc provenMathMulChecked*(a, b: int64): IntResult {.importc: "proven_math_mul_checked", dynlib: ProvenDynlib.}
proc provenMathAbsSafe*(n: int64): IntResult {.importc: "proven_math_abs_safe", dynlib: ProvenDynlib.}
proc provenMathClamp*(lo, hi, value: int64): int64 {.importc: "proven_math_clamp", dynlib: ProvenDynlib.}
proc provenMathPowChecked*(base: int64, exp: uint32): IntResult {.importc: "proven_math_pow_checked", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeString
# ---------------------------------------------------------------------------
proc provenStringIsValidUtf8*(p: pointer, len: csize_t): BoolResult {.importc: "proven_string_is_valid_utf8", dynlib: ProvenDynlib.}
proc provenStringEscapeSql*(p: pointer, len: csize_t): StringResult {.importc: "proven_string_escape_sql", dynlib: ProvenDynlib.}
proc provenStringEscapeHtml*(p: pointer, len: csize_t): StringResult {.importc: "proven_string_escape_html", dynlib: ProvenDynlib.}
proc provenStringEscapeJs*(p: pointer, len: csize_t): StringResult {.importc: "proven_string_escape_js", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafePath
# ---------------------------------------------------------------------------
proc provenPathHasTraversal*(p: pointer, len: csize_t): BoolResult {.importc: "proven_path_has_traversal", dynlib: ProvenDynlib.}
proc provenPathSanitizeFilename*(p: pointer, len: csize_t): StringResult {.importc: "proven_path_sanitize_filename", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeEmail
# ---------------------------------------------------------------------------
proc provenEmailIsValid*(p: pointer, len: csize_t): BoolResult {.importc: "proven_email_is_valid", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeNetwork
# ---------------------------------------------------------------------------
proc provenNetworkParseIpv4*(p: pointer, len: csize_t): IPv4Result {.importc: "proven_network_parse_ipv4", dynlib: ProvenDynlib.}
proc provenNetworkIpv4IsPrivate*(addr: IPv4Address): bool {.importc: "proven_network_ipv4_is_private", dynlib: ProvenDynlib.}
proc provenNetworkIpv4IsLoopback*(addr: IPv4Address): bool {.importc: "proven_network_ipv4_is_loopback", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeCrypto
# ---------------------------------------------------------------------------
proc provenCryptoConstantTimeEq*(p1: pointer, l1: csize_t, p2: pointer, l2: csize_t): BoolResult {.importc: "proven_crypto_constant_time_eq", dynlib: ProvenDynlib.}
proc provenCryptoRandomBytes*(p: pointer, len: csize_t): int32 {.importc: "proven_crypto_random_bytes", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeUrl
# ---------------------------------------------------------------------------
proc provenUrlParse*(p: pointer, len: csize_t): UrlResult {.importc: "proven_url_parse", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeJson
# ---------------------------------------------------------------------------
proc provenJsonIsValid*(p: pointer, len: csize_t): BoolResult {.importc: "proven_json_is_valid", dynlib: ProvenDynlib.}
proc provenJsonGetType*(p: pointer, len: csize_t): int32 {.importc: "proven_json_get_type", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeDateTime
# ---------------------------------------------------------------------------
proc provenDatetimeParse*(p: pointer, len: csize_t): DateTimeResult {.importc: "proven_datetime_parse", dynlib: ProvenDynlib.}
proc provenDatetimeFormatIso8601*(dt: ProvenDateTime): StringResult {.importc: "proven_datetime_format_iso8601", dynlib: ProvenDynlib.}
proc provenDatetimeIsLeapYear*(year: int32): bool {.importc: "proven_datetime_is_leap_year", dynlib: ProvenDynlib.}
proc provenDatetimeDaysInMonth*(year: int32, month: uint8): uint8 {.importc: "proven_datetime_days_in_month", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeFloat
# ---------------------------------------------------------------------------
proc provenFloatDiv*(a, b: float64): FloatResult {.importc: "proven_float_div", dynlib: ProvenDynlib.}
proc provenFloatSqrt*(x: float64): FloatResult {.importc: "proven_float_sqrt", dynlib: ProvenDynlib.}
proc provenFloatLn*(x: float64): FloatResult {.importc: "proven_float_ln", dynlib: ProvenDynlib.}
proc provenFloatIsFinite*(x: float64): bool {.importc: "proven_float_is_finite", dynlib: ProvenDynlib.}
proc provenFloatIsNan*(x: float64): bool {.importc: "proven_float_is_nan", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeVersion
# ---------------------------------------------------------------------------
proc provenVersionParse*(p: pointer, len: csize_t): VersionResult {.importc: "proven_version_parse", dynlib: ProvenDynlib.}
proc provenVersionCompare*(a, b: SemanticVersion): int32 {.importc: "proven_version_compare", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeColor
# ---------------------------------------------------------------------------
proc provenColorParseHex*(p: pointer, len: csize_t): ColorResult {.importc: "proven_color_parse_hex", dynlib: ProvenDynlib.}
proc provenColorRgbToHsl*(rgb: RGBColor): HSLColor {.importc: "proven_color_rgb_to_hsl", dynlib: ProvenDynlib.}
proc provenColorToHex*(rgb: RGBColor): StringResult {.importc: "proven_color_to_hex", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeAngle
# ---------------------------------------------------------------------------
proc provenAngleDegToRad*(degrees: float64): float64 {.importc: "proven_angle_deg_to_rad", dynlib: ProvenDynlib.}
proc provenAngleRadToDeg*(radians: float64): float64 {.importc: "proven_angle_rad_to_deg", dynlib: ProvenDynlib.}
proc provenAngleNormalizeDegrees*(degrees: float64): float64 {.importc: "proven_angle_normalize_degrees", dynlib: ProvenDynlib.}
proc provenAngleNormalizeRadians*(radians: float64): float64 {.importc: "proven_angle_normalize_radians", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeUnit
# ---------------------------------------------------------------------------
proc provenUnitConvertLength*(value: float64, frm: int32, to: int32): FloatResult {.importc: "proven_unit_convert_length", dynlib: ProvenDynlib.}
proc provenUnitConvertTemp*(value: float64, frm: int32, to: int32): FloatResult {.importc: "proven_unit_convert_temp", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeCalculator
# ---------------------------------------------------------------------------
proc provenCalculatorEval*(p: pointer, len: csize_t): FloatResult {.importc: "proven_calculator_eval", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeHex
# ---------------------------------------------------------------------------
type
  HexDecodeResult* {.importc: "ProvenHexDecodeResult", header: "proven.h".} = object
    status*: int32
    data*: ptr UncheckedArray[uint8]
    length*: csize_t

proc provenHexEncode*(p: pointer, len: csize_t, uppercase: bool): StringResult {.importc: "proven_hex_encode", dynlib: ProvenDynlib.}
proc provenHexDecode*(p: pointer, len: csize_t): HexDecodeResult {.importc: "proven_hex_decode", dynlib: ProvenDynlib.}
proc provenHexFree*(res: ptr HexDecodeResult) {.importc: "proven_hex_free", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeCurrency
# ---------------------------------------------------------------------------
type
  CurrencyResult* {.importc: "ProvenCurrencyResult", header: "proven.h".} = object
    status*: int32
    amount_minor*: int64
    currency_code*: array[3, uint8]
    decimal_places*: uint8

proc provenCurrencyParse*(p: pointer, len: csize_t): CurrencyResult {.importc: "proven_currency_parse", dynlib: ProvenDynlib.}
proc provenCurrencyFormat*(amount_minor: int64, code: array[3, uint8], decimal_places: uint8): StringResult {.importc: "proven_currency_format", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafePhone
# ---------------------------------------------------------------------------
type
  PhoneResult* {.importc: "ProvenPhoneResult", header: "proven.h".} = object
    status*: int32
    country_code*: uint16
    national_number*: uint64
    is_valid*: bool

proc provenPhoneParse*(p: pointer, len: csize_t): PhoneResult {.importc: "proven_phone_parse", dynlib: ProvenDynlib.}
proc provenPhoneFormatE164*(country_code: uint16, national_number: uint64): StringResult {.importc: "proven_phone_format_e164", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeUUID
# ---------------------------------------------------------------------------
type
  ProvenUUID* {.importc: "ProvenUUID", header: "proven.h".} = object
    bytes*: array[16, uint8]

  UUIDResult* {.importc: "ProvenUUIDResult", header: "proven.h".} = object
    status*: int32
    uuid*: ProvenUUID

proc provenUuidV4*(): UUIDResult {.importc: "proven_uuid_v4", dynlib: ProvenDynlib.}
proc provenUuidToString*(uuid: ProvenUUID): StringResult {.importc: "proven_uuid_to_string", dynlib: ProvenDynlib.}
proc provenUuidParse*(p: pointer, len: csize_t): UUIDResult {.importc: "proven_uuid_parse", dynlib: ProvenDynlib.}
proc provenUuidIsNil*(uuid: ProvenUUID): bool {.importc: "proven_uuid_is_nil", dynlib: ProvenDynlib.}
proc provenUuidVersion*(uuid: ProvenUUID): uint8 {.importc: "proven_uuid_version", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeBloom
# ---------------------------------------------------------------------------
type
  ProvenBloomFilter* {.importc: "ProvenBloomFilter", header: "proven.h".} = object
    bits*: pointer
    bit_count*: csize_t
    hash_count*: uint32

proc provenBloomCreate*(expected_elements: csize_t, false_positive_rate: float64): ptr ProvenBloomFilter {.importc: "proven_bloom_create", dynlib: ProvenDynlib.}
proc provenBloomAdd*(filter: ptr ProvenBloomFilter, p: pointer, len: csize_t) {.importc: "proven_bloom_add", dynlib: ProvenDynlib.}
proc provenBloomContains*(filter: ptr ProvenBloomFilter, p: pointer, len: csize_t): bool {.importc: "proven_bloom_contains", dynlib: ProvenDynlib.}
proc provenBloomFree*(filter: ptr ProvenBloomFilter) {.importc: "proven_bloom_free", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeBuffer
# ---------------------------------------------------------------------------
type
  ProvenBoundedBuffer* {.importc: "ProvenBoundedBuffer", header: "proven.h".} = object
    data*: pointer
    capacity*: csize_t
    length*: csize_t

  BufferResult* {.importc: "ProvenBufferResult", header: "proven.h".} = object
    status*: int32
    buffer*: ptr ProvenBoundedBuffer

proc provenBufferCreate*(capacity: csize_t): BufferResult {.importc: "proven_buffer_create", dynlib: ProvenDynlib.}
proc provenBufferAppend*(buffer: ptr ProvenBoundedBuffer, p: pointer, len: csize_t): int32 {.importc: "proven_buffer_append", dynlib: ProvenDynlib.}
proc provenBufferGet*(buffer: ptr ProvenBoundedBuffer, out_ptr: ptr pointer, out_len: ptr csize_t): int32 {.importc: "proven_buffer_get", dynlib: ProvenDynlib.}
proc provenBufferFree*(buffer: ptr ProvenBoundedBuffer) {.importc: "proven_buffer_free", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeLru
# ---------------------------------------------------------------------------
type
  ProvenLRUEntry* {.importc: "ProvenLRUEntry", header: "proven.h".} = object
    key*: uint64
    value*: int64
    prev*: csize_t
    next*: csize_t
    valid*: bool

  ProvenLRUCache* {.importc: "ProvenLRUCache", header: "proven.h".} = object
    entries*: ptr ProvenLRUEntry
    capacity*: csize_t
    head*: csize_t
    tail*: csize_t
    count*: csize_t

proc provenLruCreate*(capacity: csize_t): ptr ProvenLRUCache {.importc: "proven_lru_create", dynlib: ProvenDynlib.}
proc provenLruGet*(cache: ptr ProvenLRUCache, key: uint64): IntResult {.importc: "proven_lru_get", dynlib: ProvenDynlib.}
proc provenLruPut*(cache: ptr ProvenLRUCache, key: uint64, value: int64): int32 {.importc: "proven_lru_put", dynlib: ProvenDynlib.}
proc provenLruFree*(cache: ptr ProvenLRUCache) {.importc: "proven_lru_free", dynlib: ProvenDynlib.}

# ---------------------------------------------------------------------------
# SafeQueue
# ---------------------------------------------------------------------------
type
  ProvenBoundedQueue* {.importc: "ProvenBoundedQueue", header: "proven.h".} = object
    data*: pointer
    capacity*: csize_t
    head*: csize_t
    tail*: csize_t
    count*: csize_t

proc provenQueueCreate*(capacity: csize_t): ptr ProvenBoundedQueue {.importc: "proven_queue_create", dynlib: ProvenDynlib.}
proc provenQueuePush*(queue: ptr ProvenBoundedQueue, value: int64): bool {.importc: "proven_queue_push", dynlib: ProvenDynlib.}
proc provenQueuePop*(queue: ptr ProvenBoundedQueue): IntResult {.importc: "proven_queue_pop", dynlib: ProvenDynlib.}
proc provenQueueSize*(queue: ptr ProvenBoundedQueue): csize_t {.importc: "proven_queue_size", dynlib: ProvenDynlib.}
proc provenQueueFree*(queue: ptr ProvenBoundedQueue) {.importc: "proven_queue_free", dynlib: ProvenDynlib.}
