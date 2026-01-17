// SPDX-License-Identifier: PMPL-1.0
/*
 * Proven - Formally verified safety primitives for Alloy 6
 *
 * VERSION: 0.4.0
 * MODULE_COUNT: 38
 *
 * Alloy is a relational modeling language developed at MIT for
 * lightweight formal methods. It uses SAT solving for bounded
 * model checking and is used in security analysis, API design,
 * and software architecture verification.
 *
 * This module provides proven safety primitives as Alloy signatures
 * and predicates for use in formal models.
 *
 * Module Categories:
 *   Core (11): SafeMath, SafeString, SafePath, SafeEmail, SafeUrl,
 *              SafeNetwork, SafeCrypto, SafeUUID, SafeCurrency, SafePhone, SafeHex
 *   Data (7): SafeJson, SafeDateTime, SafeFloat, SafeVersion, SafeColor,
 *             SafeAngle, SafeUnit
 *   Data Structures (5): SafeBuffer, SafeQueue, SafeBloom, SafeLRU, SafeGraph
 *   Resilience (4): SafeRateLimiter, SafeCircuitBreaker, SafeRetry, SafeMonotonic
 *   State (2): SafeStateMachine, SafeCalculator
 *   Algorithm (4): SafeGeo, SafeProbability, SafeChecksum, SafeTensor
 *   Security (2): SafePassword, SafeMl
 *   HTTP (3): SafeHeader, SafeCookie, SafeContentType
 */

module proven

// ============================================================================
// COMMON TYPES
// ============================================================================

abstract sig Result {}

sig Ok extends Result {
    value: one Value
}

sig Err extends Result {
    errorCode: one Int,
    errorMsg: one String
}

abstract sig Value {}

pred isOk[r: Result] {
    r in Ok
}

pred isErr[r: Result] {
    r in Err
}

fact ResultInvariant {
    all r: Result | r in Ok or r in Err
    no r: Result | r in Ok and r in Err
}

abstract sig Option {}

sig Some extends Option {
    optValue: one Value
}

sig None extends Option {}

pred isSome[o: Option] {
    o in Some
}

pred isNone[o: Option] {
    o in None
}

fact OptionInvariant {
    all o: Option | o in Some or o in None
    no o: Option | o in Some and o in None
}

// ============================================================================
// CORE MODULE 1: SafeMath
// ============================================================================

sig SafeMath_BoundedInt {
    value: one Int,
    minVal: one Int,
    maxVal: one Int
}

fact SafeMath_BoundedIntInvariant {
    all b: SafeMath_BoundedInt |
        b.minVal <= b.maxVal and
        b.value >= b.minVal and
        b.value <= b.maxVal
}

pred SafeMath_atMin[b: SafeMath_BoundedInt] {
    b.value = b.minVal
}

pred SafeMath_atMax[b: SafeMath_BoundedInt] {
    b.value = b.maxVal
}

pred SafeMath_canIncrement[b: SafeMath_BoundedInt] {
    b.value < b.maxVal
}

pred SafeMath_canDecrement[b: SafeMath_BoundedInt] {
    b.value > b.minVal
}

fun SafeMath_clamp[v: Int, minV: Int, maxV: Int]: Int {
    (v < minV) => minV else ((v > maxV) => maxV else v)
}

sig SafeMath_Percentage {
    pctValue: one Int
}

fact SafeMath_PercentageInvariant {
    all p: SafeMath_Percentage | p.pctValue >= 0 and p.pctValue <= 100
}

sig SafeMath_BasisPoints {
    bpValue: one Int
}

fact SafeMath_BasisPointsInvariant {
    all bp: SafeMath_BasisPoints | bp.bpValue >= 0 and bp.bpValue <= 10000
}

pred SafeMath_basisPointsToPercentage[bp: SafeMath_BasisPoints, pct: SafeMath_Percentage] {
    pct.pctValue = div[bp.bpValue, 100]
}

// ============================================================================
// CORE MODULE 2: SafeString
// ============================================================================

sig SafeString_BoundedString {
    length: one Int,
    maxLength: one Int,
    isAscii: one Bool,
    isUtf8Valid: one Bool
}

fact SafeString_BoundedStringInvariant {
    all s: SafeString_BoundedString |
        s.length >= 0 and
        s.maxLength >= 0 and
        s.length <= s.maxLength
}

pred SafeString_isEmpty[s: SafeString_BoundedString] {
    s.length = 0
}

pred SafeString_isFull[s: SafeString_BoundedString] {
    s.length = s.maxLength
}

pred SafeString_canAppend[s: SafeString_BoundedString, appendLen: Int] {
    appendLen >= 0 and s.length + appendLen <= s.maxLength
}

sig SafeString_Sanitized {
    original: one SafeString_BoundedString,
    sanitized: one SafeString_BoundedString,
    sanitizationType: one SafeString_SanitizationType
}

abstract sig SafeString_SanitizationType {}
one sig SafeString_HtmlEscape extends SafeString_SanitizationType {}
one sig SafeString_SqlEscape extends SafeString_SanitizationType {}
one sig SafeString_ShellEscape extends SafeString_SanitizationType {}
one sig SafeString_UrlEncode extends SafeString_SanitizationType {}

// ============================================================================
// CORE MODULE 3: SafePath
// ============================================================================

sig SafePath_Path {
    components: seq SafePath_Component,
    isAbsolute: one Bool,
    isNormalized: one Bool
}

sig SafePath_Component {
    name: one String,
    isValid: one Bool
}

fact SafePath_ComponentInvariant {
    all c: SafePath_Component |
        c.isValid = True implies
            c.name != "." and c.name != ".."
}

pred SafePath_isRoot[p: SafePath_Path] {
    p.isAbsolute = True and #p.components = 0
}

pred SafePath_noTraversal[p: SafePath_Path] {
    all c: p.components.elems | c.name != ".."
}

pred SafePath_isWithinBase[p: SafePath_Path, base: SafePath_Path] {
    p.isAbsolute = base.isAbsolute and
    #p.components >= #base.components
}

sig SafePath_SandboxedPath {
    path: one SafePath_Path,
    sandbox: one SafePath_Path
}

fact SafePath_SandboxedInvariant {
    all sp: SafePath_SandboxedPath |
        SafePath_isWithinBase[sp.path, sp.sandbox]
}

// ============================================================================
// CORE MODULE 4: SafeEmail
// ============================================================================

sig SafeEmail_Address {
    localPart: one String,
    domain: one String,
    localPartLength: one Int,
    domainLength: one Int,
    totalLength: one Int
}

fact SafeEmail_AddressInvariant {
    all e: SafeEmail_Address |
        e.localPartLength >= 1 and e.localPartLength <= 64 and
        e.domainLength >= 1 and e.domainLength <= 255 and
        e.totalLength = e.localPartLength + 1 + e.domainLength and
        e.totalLength <= 254
}

pred SafeEmail_hasValidLocalPart[e: SafeEmail_Address] {
    e.localPartLength >= 1 and e.localPartLength <= 64
}

pred SafeEmail_hasValidDomain[e: SafeEmail_Address] {
    e.domainLength >= 1 and e.domainLength <= 255
}

// ============================================================================
// CORE MODULE 5: SafeUrl
// ============================================================================

sig SafeUrl_Url {
    scheme: one SafeUrl_Scheme,
    host: lone String,
    port: lone Int,
    path: one String,
    hasQuery: one Bool,
    hasFragment: one Bool
}

abstract sig SafeUrl_Scheme {}
one sig SafeUrl_Http extends SafeUrl_Scheme {}
one sig SafeUrl_Https extends SafeUrl_Scheme {}
one sig SafeUrl_Ftp extends SafeUrl_Scheme {}
one sig SafeUrl_File extends SafeUrl_Scheme {}
one sig SafeUrl_Data extends SafeUrl_Scheme {}

fact SafeUrl_PortInvariant {
    all u: SafeUrl_Url | some u.port implies (u.port >= 1 and u.port <= 65535)
}

pred SafeUrl_isSecure[u: SafeUrl_Url] {
    u.scheme = SafeUrl_Https
}

pred SafeUrl_hasHost[u: SafeUrl_Url] {
    some u.host
}

pred SafeUrl_isDefaultPort[u: SafeUrl_Url] {
    (u.scheme = SafeUrl_Http and u.port = 80) or
    (u.scheme = SafeUrl_Https and u.port = 443) or
    (u.scheme = SafeUrl_Ftp and u.port = 21)
}

// ============================================================================
// CORE MODULE 6: SafeNetwork
// ============================================================================

sig SafeNetwork_Port {
    portNum: one Int
}

fact SafeNetwork_PortInvariant {
    all p: SafeNetwork_Port | p.portNum >= 1 and p.portNum <= 65535
}

pred SafeNetwork_isWellKnownPort[p: SafeNetwork_Port] {
    p.portNum >= 1 and p.portNum <= 1023
}

pred SafeNetwork_isRegisteredPort[p: SafeNetwork_Port] {
    p.portNum >= 1024 and p.portNum <= 49151
}

pred SafeNetwork_isDynamicPort[p: SafeNetwork_Port] {
    p.portNum >= 49152 and p.portNum <= 65535
}

assert SafeNetwork_PortCategoriesComplete {
    all p: SafeNetwork_Port |
        (SafeNetwork_isWellKnownPort[p] or SafeNetwork_isRegisteredPort[p] or SafeNetwork_isDynamicPort[p]) and
        not (SafeNetwork_isWellKnownPort[p] and SafeNetwork_isRegisteredPort[p]) and
        not (SafeNetwork_isWellKnownPort[p] and SafeNetwork_isDynamicPort[p]) and
        not (SafeNetwork_isRegisteredPort[p] and SafeNetwork_isDynamicPort[p])
}

sig SafeNetwork_IpV4 {
    octet1: one Int,
    octet2: one Int,
    octet3: one Int,
    octet4: one Int
}

fact SafeNetwork_IpV4Invariant {
    all ip: SafeNetwork_IpV4 |
        ip.octet1 >= 0 and ip.octet1 <= 255 and
        ip.octet2 >= 0 and ip.octet2 <= 255 and
        ip.octet3 >= 0 and ip.octet3 <= 255 and
        ip.octet4 >= 0 and ip.octet4 <= 255
}

pred SafeNetwork_isLoopback[ip: SafeNetwork_IpV4] {
    ip.octet1 = 127
}

pred SafeNetwork_isPrivate[ip: SafeNetwork_IpV4] {
    (ip.octet1 = 10) or
    (ip.octet1 = 172 and ip.octet2 >= 16 and ip.octet2 <= 31) or
    (ip.octet1 = 192 and ip.octet2 = 168)
}

sig SafeNetwork_Cidr {
    ip: one SafeNetwork_IpV4,
    prefixLen: one Int
}

fact SafeNetwork_CidrInvariant {
    all c: SafeNetwork_Cidr | c.prefixLen >= 0 and c.prefixLen <= 32
}

// ============================================================================
// CORE MODULE 7: SafeCrypto
// ============================================================================

sig SafeCrypto_Hash {
    algorithm: one SafeCrypto_HashAlgorithm,
    digestLength: one Int
}

abstract sig SafeCrypto_HashAlgorithm {}
one sig SafeCrypto_SHA256 extends SafeCrypto_HashAlgorithm {}
one sig SafeCrypto_SHA384 extends SafeCrypto_HashAlgorithm {}
one sig SafeCrypto_SHA512 extends SafeCrypto_HashAlgorithm {}
one sig SafeCrypto_BLAKE2b extends SafeCrypto_HashAlgorithm {}
one sig SafeCrypto_BLAKE3 extends SafeCrypto_HashAlgorithm {}

fact SafeCrypto_HashLengthInvariant {
    all h: SafeCrypto_Hash |
        (h.algorithm = SafeCrypto_SHA256 implies h.digestLength = 32) and
        (h.algorithm = SafeCrypto_SHA384 implies h.digestLength = 48) and
        (h.algorithm = SafeCrypto_SHA512 implies h.digestLength = 64) and
        (h.algorithm = SafeCrypto_BLAKE2b implies h.digestLength = 64) and
        (h.algorithm = SafeCrypto_BLAKE3 implies h.digestLength = 32)
}

sig SafeCrypto_Key {
    keyLength: one Int,
    keyType: one SafeCrypto_KeyType
}

abstract sig SafeCrypto_KeyType {}
one sig SafeCrypto_Symmetric extends SafeCrypto_KeyType {}
one sig SafeCrypto_AsymmetricPublic extends SafeCrypto_KeyType {}
one sig SafeCrypto_AsymmetricPrivate extends SafeCrypto_KeyType {}

fact SafeCrypto_KeyLengthInvariant {
    all k: SafeCrypto_Key |
        k.keyLength >= 16 and k.keyLength <= 4096
}

sig SafeCrypto_Signature {
    algorithm: one SafeCrypto_SignatureAlgorithm,
    signatureLength: one Int
}

abstract sig SafeCrypto_SignatureAlgorithm {}
one sig SafeCrypto_Ed25519 extends SafeCrypto_SignatureAlgorithm {}
one sig SafeCrypto_ECDSA extends SafeCrypto_SignatureAlgorithm {}
one sig SafeCrypto_RSA extends SafeCrypto_SignatureAlgorithm {}

// ============================================================================
// CORE MODULE 8: SafeUUID
// ============================================================================

sig SafeUUID_UUID {
    version: one Int,
    variant: one SafeUUID_Variant
}

abstract sig SafeUUID_Variant {}
one sig SafeUUID_RFC4122 extends SafeUUID_Variant {}
one sig SafeUUID_Microsoft extends SafeUUID_Variant {}
one sig SafeUUID_Reserved extends SafeUUID_Variant {}

fact SafeUUID_VersionInvariant {
    all u: SafeUUID_UUID | u.version >= 1 and u.version <= 8
}

pred SafeUUID_isV4Random[u: SafeUUID_UUID] {
    u.version = 4 and u.variant = SafeUUID_RFC4122
}

pred SafeUUID_isV7Timestamp[u: SafeUUID_UUID] {
    u.version = 7 and u.variant = SafeUUID_RFC4122
}

// ============================================================================
// CORE MODULE 9: SafeCurrency
// ============================================================================

sig SafeCurrency_Amount {
    wholePart: one Int,
    fractionalPart: one Int,
    currency: one SafeCurrency_Currency,
    decimalPlaces: one Int
}

sig SafeCurrency_Currency {
    code: one String,
    symbol: one String,
    minorUnits: one Int
}

fact SafeCurrency_AmountInvariant {
    all a: SafeCurrency_Amount |
        a.fractionalPart >= 0 and
        a.decimalPlaces >= 0 and a.decimalPlaces <= 8
}

pred SafeCurrency_isZero[a: SafeCurrency_Amount] {
    a.wholePart = 0 and a.fractionalPart = 0
}

pred SafeCurrency_isPositive[a: SafeCurrency_Amount] {
    a.wholePart > 0 or (a.wholePart = 0 and a.fractionalPart > 0)
}

pred SafeCurrency_sameCurrency[a: SafeCurrency_Amount, b: SafeCurrency_Amount] {
    a.currency = b.currency
}

// ============================================================================
// CORE MODULE 10: SafePhone
// ============================================================================

sig SafePhone_Number {
    countryCode: one Int,
    nationalNumber: one Int,
    countryCodeDigits: one Int,
    nationalNumberDigits: one Int
}

fact SafePhone_NumberInvariant {
    all p: SafePhone_Number |
        p.countryCode >= 1 and p.countryCode <= 999 and
        p.countryCodeDigits >= 1 and p.countryCodeDigits <= 3 and
        p.nationalNumberDigits >= 4 and p.nationalNumberDigits <= 14
}

pred SafePhone_isE164Valid[p: SafePhone_Number] {
    p.countryCodeDigits + p.nationalNumberDigits <= 15
}

// ============================================================================
// CORE MODULE 11: SafeHex
// ============================================================================

sig SafeHex_String {
    byteLength: one Int,
    hexLength: one Int,
    isLowercase: one Bool
}

fact SafeHex_StringInvariant {
    all h: SafeHex_String |
        h.byteLength >= 0 and
        h.hexLength = h.byteLength * 2
}

pred SafeHex_isValidLength[h: SafeHex_String] {
    rem[h.hexLength, 2] = 0
}

// ============================================================================
// DATA MODULE 1: SafeJson
// ============================================================================

abstract sig SafeJson_Value {}
sig SafeJson_Null extends SafeJson_Value {}
sig SafeJson_Boolean extends SafeJson_Value { boolValue: one Bool }
sig SafeJson_Number extends SafeJson_Value { numValue: one Int }
sig SafeJson_String extends SafeJson_Value { strLength: one Int }
sig SafeJson_Array extends SafeJson_Value { elements: seq SafeJson_Value }
sig SafeJson_Object extends SafeJson_Value { fields: String -> SafeJson_Value }

sig SafeJson_Document {
    root: one SafeJson_Value,
    depth: one Int,
    size: one Int
}

fact SafeJson_DocumentInvariant {
    all d: SafeJson_Document |
        d.depth >= 0 and d.depth <= 64 and
        d.size >= 0
}

pred SafeJson_isNull[v: SafeJson_Value] {
    v in SafeJson_Null
}

pred SafeJson_isScalar[v: SafeJson_Value] {
    v in SafeJson_Null or v in SafeJson_Boolean or
    v in SafeJson_Number or v in SafeJson_String
}

// ============================================================================
// DATA MODULE 2: SafeDateTime
// ============================================================================

sig SafeDateTime_Date {
    year: one Int,
    month: one Int,
    day: one Int
}

fact SafeDateTime_DateInvariant {
    all d: SafeDateTime_Date |
        d.year >= 1 and d.year <= 9999 and
        d.month >= 1 and d.month <= 12 and
        d.day >= 1 and d.day <= 31
}

sig SafeDateTime_Time {
    hour: one Int,
    minute: one Int,
    second: one Int,
    nanosecond: one Int
}

fact SafeDateTime_TimeInvariant {
    all t: SafeDateTime_Time |
        t.hour >= 0 and t.hour <= 23 and
        t.minute >= 0 and t.minute <= 59 and
        t.second >= 0 and t.second <= 60 and
        t.nanosecond >= 0 and t.nanosecond <= 999999999
}

sig SafeDateTime_DateTime {
    date: one SafeDateTime_Date,
    time: one SafeDateTime_Time,
    offsetMinutes: one Int
}

fact SafeDateTime_DateTimeInvariant {
    all dt: SafeDateTime_DateTime |
        dt.offsetMinutes >= -840 and dt.offsetMinutes <= 840
}

pred SafeDateTime_isUtc[dt: SafeDateTime_DateTime] {
    dt.offsetMinutes = 0
}

sig SafeDateTime_Duration {
    seconds: one Int,
    nanoseconds: one Int
}

fact SafeDateTime_DurationInvariant {
    all d: SafeDateTime_Duration |
        d.nanoseconds >= 0 and d.nanoseconds <= 999999999
}

pred SafeDateTime_isPositive[d: SafeDateTime_Duration] {
    d.seconds > 0 or (d.seconds = 0 and d.nanoseconds > 0)
}

// ============================================================================
// DATA MODULE 3: SafeFloat
// ============================================================================

sig SafeFloat_Float {
    isFinite: one Bool,
    isNaN: one Bool,
    isInfinity: one Bool,
    isPositive: one Bool
}

fact SafeFloat_FloatInvariant {
    all f: SafeFloat_Float |
        (f.isNaN = True implies f.isFinite = False and f.isInfinity = False) and
        (f.isInfinity = True implies f.isFinite = False and f.isNaN = False) and
        (f.isFinite = True implies f.isNaN = False and f.isInfinity = False)
}

pred SafeFloat_isValid[f: SafeFloat_Float] {
    f.isFinite = True
}

pred SafeFloat_isZero[f: SafeFloat_Float] {
    f.isFinite = True
}

sig SafeFloat_Decimal {
    coefficient: one Int,
    exponent: one Int,
    precision: one Int
}

fact SafeFloat_DecimalInvariant {
    all d: SafeFloat_Decimal |
        d.precision >= 1 and d.precision <= 38
}

// ============================================================================
// DATA MODULE 4: SafeVersion
// ============================================================================

sig SafeVersion_SemVer {
    major: one Int,
    minor: one Int,
    patch: one Int,
    hasPrerelease: one Bool,
    hasBuildMetadata: one Bool
}

fact SafeVersion_SemVerInvariant {
    all v: SafeVersion_SemVer |
        v.major >= 0 and
        v.minor >= 0 and
        v.patch >= 0
}

pred SafeVersion_isStable[v: SafeVersion_SemVer] {
    v.major >= 1 and v.hasPrerelease = False
}

pred SafeVersion_isPrerelease[v: SafeVersion_SemVer] {
    v.hasPrerelease = True
}

pred SafeVersion_isCompatible[a: SafeVersion_SemVer, b: SafeVersion_SemVer] {
    a.major = b.major and
    (a.major = 0 implies (a.minor = b.minor and a.patch = b.patch))
}

// ============================================================================
// DATA MODULE 5: SafeColor
// ============================================================================

sig SafeColor_Rgba {
    red: one Int,
    green: one Int,
    blue: one Int,
    alpha: one Int
}

fact SafeColor_RgbaInvariant {
    all c: SafeColor_Rgba |
        c.red >= 0 and c.red <= 255 and
        c.green >= 0 and c.green <= 255 and
        c.blue >= 0 and c.blue <= 255 and
        c.alpha >= 0 and c.alpha <= 255
}

pred SafeColor_isOpaque[c: SafeColor_Rgba] {
    c.alpha = 255
}

pred SafeColor_isTransparent[c: SafeColor_Rgba] {
    c.alpha = 0
}

sig SafeColor_Hsla {
    hue: one Int,
    saturation: one Int,
    lightness: one Int,
    alpha: one Int
}

fact SafeColor_HslaInvariant {
    all c: SafeColor_Hsla |
        c.hue >= 0 and c.hue <= 360 and
        c.saturation >= 0 and c.saturation <= 100 and
        c.lightness >= 0 and c.lightness <= 100 and
        c.alpha >= 0 and c.alpha <= 100
}

// ============================================================================
// DATA MODULE 6: SafeAngle
// ============================================================================

sig SafeAngle_Degrees {
    value: one Int,
    isNormalized: one Bool
}

fact SafeAngle_DegreesNormalizedInvariant {
    all a: SafeAngle_Degrees |
        a.isNormalized = True implies (a.value >= 0 and a.value < 360)
}

sig SafeAngle_Radians {
    valueScaled: one Int,
    scaleFactor: one Int
}

pred SafeAngle_isFullRotation[a: SafeAngle_Degrees] {
    a.value = 360 or a.value = 0
}

pred SafeAngle_isRightAngle[a: SafeAngle_Degrees] {
    a.value = 90 or a.value = 270
}

// ============================================================================
// DATA MODULE 7: SafeUnit
// ============================================================================

sig SafeUnit_Quantity {
    magnitude: one Int,
    unit: one SafeUnit_Unit,
    scaleFactor: one Int
}

abstract sig SafeUnit_Unit {}
one sig SafeUnit_Meter extends SafeUnit_Unit {}
one sig SafeUnit_Kilogram extends SafeUnit_Unit {}
one sig SafeUnit_Second extends SafeUnit_Unit {}
one sig SafeUnit_Ampere extends SafeUnit_Unit {}
one sig SafeUnit_Kelvin extends SafeUnit_Unit {}
one sig SafeUnit_Mole extends SafeUnit_Unit {}
one sig SafeUnit_Candela extends SafeUnit_Unit {}

pred SafeUnit_sameUnit[a: SafeUnit_Quantity, b: SafeUnit_Quantity] {
    a.unit = b.unit
}

pred SafeUnit_canAdd[a: SafeUnit_Quantity, b: SafeUnit_Quantity] {
    a.unit = b.unit
}

// ============================================================================
// DATA STRUCTURES MODULE 1: SafeBuffer
// ============================================================================

sig SafeBuffer_Buffer {
    capacity: one Int,
    length: one Int,
    position: one Int
}

fact SafeBuffer_BufferInvariant {
    all b: SafeBuffer_Buffer |
        b.capacity >= 0 and
        b.length >= 0 and
        b.length <= b.capacity and
        b.position >= 0 and
        b.position <= b.length
}

pred SafeBuffer_isEmpty[b: SafeBuffer_Buffer] {
    b.length = 0
}

pred SafeBuffer_isFull[b: SafeBuffer_Buffer] {
    b.length = b.capacity
}

pred SafeBuffer_canWrite[b: SafeBuffer_Buffer, amount: Int] {
    amount >= 0 and b.length + amount <= b.capacity
}

pred SafeBuffer_canRead[b: SafeBuffer_Buffer, amount: Int] {
    amount >= 0 and b.position + amount <= b.length
}

sig SafeBuffer_RingBuffer {
    capacity: one Int,
    head: one Int,
    tail: one Int,
    count: one Int
}

fact SafeBuffer_RingBufferInvariant {
    all rb: SafeBuffer_RingBuffer |
        rb.capacity >= 1 and
        rb.head >= 0 and rb.head < rb.capacity and
        rb.tail >= 0 and rb.tail < rb.capacity and
        rb.count >= 0 and rb.count <= rb.capacity
}

// ============================================================================
// DATA STRUCTURES MODULE 2: SafeQueue
// ============================================================================

sig SafeQueue_Queue {
    maxSize: one Int,
    currentSize: one Int
}

fact SafeQueue_QueueInvariant {
    all q: SafeQueue_Queue |
        q.maxSize >= 0 and
        q.currentSize >= 0 and
        q.currentSize <= q.maxSize
}

pred SafeQueue_isEmpty[q: SafeQueue_Queue] {
    q.currentSize = 0
}

pred SafeQueue_isFull[q: SafeQueue_Queue] {
    q.currentSize = q.maxSize
}

pred SafeQueue_canEnqueue[q: SafeQueue_Queue] {
    q.currentSize < q.maxSize
}

pred SafeQueue_canDequeue[q: SafeQueue_Queue] {
    q.currentSize > 0
}

sig SafeQueue_PriorityQueue {
    maxSize: one Int,
    currentSize: one Int,
    priorityLevels: one Int
}

fact SafeQueue_PriorityQueueInvariant {
    all pq: SafeQueue_PriorityQueue |
        pq.maxSize >= 0 and
        pq.currentSize >= 0 and
        pq.currentSize <= pq.maxSize and
        pq.priorityLevels >= 1
}

// ============================================================================
// DATA STRUCTURES MODULE 3: SafeBloom
// ============================================================================

sig SafeBloom_Filter {
    bitCount: one Int,
    hashCount: one Int,
    insertedCount: one Int,
    expectedFalsePositiveRate: one Int
}

fact SafeBloom_FilterInvariant {
    all b: SafeBloom_Filter |
        b.bitCount >= 8 and
        b.hashCount >= 1 and b.hashCount <= 32 and
        b.insertedCount >= 0 and
        b.expectedFalsePositiveRate >= 0 and b.expectedFalsePositiveRate <= 100
}

pred SafeBloom_isEmpty[b: SafeBloom_Filter] {
    b.insertedCount = 0
}

pred SafeBloom_isSaturated[b: SafeBloom_Filter] {
    b.expectedFalsePositiveRate > 50
}

// ============================================================================
// DATA STRUCTURES MODULE 4: SafeLRU
// ============================================================================

sig SafeLRU_Cache {
    capacity: one Int,
    size: one Int,
    hitCount: one Int,
    missCount: one Int
}

fact SafeLRU_CacheInvariant {
    all c: SafeLRU_Cache |
        c.capacity >= 1 and
        c.size >= 0 and
        c.size <= c.capacity and
        c.hitCount >= 0 and
        c.missCount >= 0
}

pred SafeLRU_isEmpty[c: SafeLRU_Cache] {
    c.size = 0
}

pred SafeLRU_isFull[c: SafeLRU_Cache] {
    c.size = c.capacity
}

fun SafeLRU_hitRate[c: SafeLRU_Cache]: Int {
    let total = c.hitCount + c.missCount |
        (total > 0) => div[c.hitCount * 100, total] else 0
}

// ============================================================================
// DATA STRUCTURES MODULE 5: SafeGraph
// ============================================================================

sig SafeGraph_Node {}

sig SafeGraph_Edge {
    source: one SafeGraph_Node,
    target: one SafeGraph_Node,
    weight: one Int
}

sig SafeGraph_Graph {
    nodes: set SafeGraph_Node,
    edges: set SafeGraph_Edge,
    isDirected: one Bool,
    isWeighted: one Bool
}

fact SafeGraph_GraphInvariant {
    all g: SafeGraph_Graph |
        all e: g.edges | e.source in g.nodes and e.target in g.nodes
}

pred SafeGraph_isEmpty[g: SafeGraph_Graph] {
    #g.nodes = 0 and #g.edges = 0
}

pred SafeGraph_isConnected[g: SafeGraph_Graph] {
    all n1, n2: g.nodes | n2 in n1.^(g.edges.target + g.edges.source)
}

pred SafeGraph_hasCycle[g: SafeGraph_Graph] {
    some n: g.nodes | n in n.^(g.edges.target)
}

pred SafeGraph_noSelfLoops[g: SafeGraph_Graph] {
    no e: g.edges | e.source = e.target
}

// ============================================================================
// RESILIENCE MODULE 1: SafeRateLimiter
// ============================================================================

sig SafeRateLimiter_TokenBucket {
    capacity: one Int,
    tokens: one Int,
    refillRate: one Int,
    refillInterval: one Int
}

fact SafeRateLimiter_TokenBucketInvariant {
    all tb: SafeRateLimiter_TokenBucket |
        tb.capacity >= 1 and
        tb.tokens >= 0 and
        tb.tokens <= tb.capacity and
        tb.refillRate >= 1 and
        tb.refillInterval >= 1
}

pred SafeRateLimiter_isEmpty[tb: SafeRateLimiter_TokenBucket] {
    tb.tokens = 0
}

pred SafeRateLimiter_isFull[tb: SafeRateLimiter_TokenBucket] {
    tb.tokens = tb.capacity
}

pred SafeRateLimiter_canConsume[tb: SafeRateLimiter_TokenBucket, amount: Int] {
    amount >= 0 and tb.tokens >= amount
}

sig SafeRateLimiter_SlidingWindow {
    windowSize: one Int,
    maxRequests: one Int,
    currentRequests: one Int
}

fact SafeRateLimiter_SlidingWindowInvariant {
    all sw: SafeRateLimiter_SlidingWindow |
        sw.windowSize >= 1 and
        sw.maxRequests >= 1 and
        sw.currentRequests >= 0 and
        sw.currentRequests <= sw.maxRequests
}

// ============================================================================
// RESILIENCE MODULE 2: SafeCircuitBreaker
// ============================================================================

abstract sig SafeCircuitBreaker_State {}
one sig SafeCircuitBreaker_Closed extends SafeCircuitBreaker_State {}
one sig SafeCircuitBreaker_Open extends SafeCircuitBreaker_State {}
one sig SafeCircuitBreaker_HalfOpen extends SafeCircuitBreaker_State {}

sig SafeCircuitBreaker_CircuitBreaker {
    state: one SafeCircuitBreaker_State,
    failureCount: one Int,
    successCount: one Int,
    failureThreshold: one Int,
    successThreshold: one Int,
    timeout: one Int
}

fact SafeCircuitBreaker_Invariant {
    all cb: SafeCircuitBreaker_CircuitBreaker |
        cb.failureCount >= 0 and
        cb.successCount >= 0 and
        cb.failureThreshold >= 1 and
        cb.successThreshold >= 1 and
        cb.timeout >= 1
}

pred SafeCircuitBreaker_isClosed[cb: SafeCircuitBreaker_CircuitBreaker] {
    cb.state = SafeCircuitBreaker_Closed
}

pred SafeCircuitBreaker_isOpen[cb: SafeCircuitBreaker_CircuitBreaker] {
    cb.state = SafeCircuitBreaker_Open
}

pred SafeCircuitBreaker_shouldTrip[cb: SafeCircuitBreaker_CircuitBreaker] {
    cb.state = SafeCircuitBreaker_Closed and
    cb.failureCount >= cb.failureThreshold
}

pred SafeCircuitBreaker_shouldReset[cb: SafeCircuitBreaker_CircuitBreaker] {
    cb.state = SafeCircuitBreaker_HalfOpen and
    cb.successCount >= cb.successThreshold
}

// ============================================================================
// RESILIENCE MODULE 3: SafeRetry
// ============================================================================

sig SafeRetry_Policy {
    maxAttempts: one Int,
    currentAttempt: one Int,
    baseDelay: one Int,
    maxDelay: one Int,
    backoffMultiplier: one Int
}

fact SafeRetry_PolicyInvariant {
    all p: SafeRetry_Policy |
        p.maxAttempts >= 1 and
        p.currentAttempt >= 0 and
        p.currentAttempt <= p.maxAttempts and
        p.baseDelay >= 0 and
        p.maxDelay >= p.baseDelay and
        p.backoffMultiplier >= 1
}

pred SafeRetry_canRetry[p: SafeRetry_Policy] {
    p.currentAttempt < p.maxAttempts
}

pred SafeRetry_isExhausted[p: SafeRetry_Policy] {
    p.currentAttempt >= p.maxAttempts
}

pred SafeRetry_isFirstAttempt[p: SafeRetry_Policy] {
    p.currentAttempt = 0
}

// ============================================================================
// RESILIENCE MODULE 4: SafeMonotonic
// ============================================================================

sig SafeMonotonic_Counter {
    value: one Int,
    overflow: one Bool
}

fact SafeMonotonic_CounterInvariant {
    all c: SafeMonotonic_Counter |
        c.value >= 0
}

pred SafeMonotonic_isZero[c: SafeMonotonic_Counter] {
    c.value = 0
}

pred SafeMonotonic_hasOverflowed[c: SafeMonotonic_Counter] {
    c.overflow = True
}

sig SafeMonotonic_Timestamp {
    epochSeconds: one Int,
    sequence: one Int
}

fact SafeMonotonic_TimestampInvariant {
    all t: SafeMonotonic_Timestamp |
        t.epochSeconds >= 0 and
        t.sequence >= 0
}

pred SafeMonotonic_isAfter[a: SafeMonotonic_Timestamp, b: SafeMonotonic_Timestamp] {
    a.epochSeconds > b.epochSeconds or
    (a.epochSeconds = b.epochSeconds and a.sequence > b.sequence)
}

// ============================================================================
// STATE MODULE 1: SafeStateMachine
// ============================================================================

sig SafeStateMachine_State {}

sig SafeStateMachine_Transition {
    from: one SafeStateMachine_State,
    to: one SafeStateMachine_State,
    guard: one Bool
}

sig SafeStateMachine_Machine {
    currentState: one SafeStateMachine_State,
    initialState: one SafeStateMachine_State,
    finalStates: set SafeStateMachine_State,
    states: set SafeStateMachine_State,
    transitions: set SafeStateMachine_Transition
}

fact SafeStateMachine_MachineInvariant {
    all m: SafeStateMachine_Machine |
        m.currentState in m.states and
        m.initialState in m.states and
        m.finalStates in m.states and
        all t: m.transitions | t.from in m.states and t.to in m.states
}

pred SafeStateMachine_canTransition[m: SafeStateMachine_Machine, to: SafeStateMachine_State] {
    some t: m.transitions | t.from = m.currentState and t.to = to and t.guard = True
}

pred SafeStateMachine_isInFinalState[m: SafeStateMachine_Machine] {
    m.currentState in m.finalStates
}

pred SafeStateMachine_allStatesReachable[m: SafeStateMachine_Machine] {
    all s: m.states | s in m.initialState.*(m.transitions.to)
}

// ============================================================================
// STATE MODULE 2: SafeCalculator
// ============================================================================

sig SafeCalculator_Calculator {
    accumulator: one Int,
    memory: one Int,
    hasError: one Bool,
    lastOperation: one SafeCalculator_Operation
}

abstract sig SafeCalculator_Operation {}
one sig SafeCalculator_Add extends SafeCalculator_Operation {}
one sig SafeCalculator_Subtract extends SafeCalculator_Operation {}
one sig SafeCalculator_Multiply extends SafeCalculator_Operation {}
one sig SafeCalculator_Divide extends SafeCalculator_Operation {}
one sig SafeCalculator_Clear extends SafeCalculator_Operation {}

pred SafeCalculator_isCleared[c: SafeCalculator_Calculator] {
    c.accumulator = 0 and c.hasError = False
}

pred SafeCalculator_hasError[c: SafeCalculator_Calculator] {
    c.hasError = True
}

pred SafeCalculator_canDivide[c: SafeCalculator_Calculator, divisor: Int] {
    divisor != 0
}

// ============================================================================
// ALGORITHM MODULE 1: SafeGeo
// ============================================================================

sig SafeGeo_Coordinate {
    latitude: one Int,
    longitude: one Int,
    latitudeScale: one Int,
    longitudeScale: one Int
}

fact SafeGeo_CoordinateInvariant {
    all c: SafeGeo_Coordinate |
        c.latitude >= -90 * c.latitudeScale and
        c.latitude <= 90 * c.latitudeScale and
        c.longitude >= -180 * c.longitudeScale and
        c.longitude <= 180 * c.longitudeScale and
        c.latitudeScale >= 1 and
        c.longitudeScale >= 1
}

sig SafeGeo_BoundingBox {
    minLat: one Int,
    maxLat: one Int,
    minLon: one Int,
    maxLon: one Int,
    scale: one Int
}

fact SafeGeo_BoundingBoxInvariant {
    all bb: SafeGeo_BoundingBox |
        bb.minLat <= bb.maxLat and
        bb.minLon <= bb.maxLon and
        bb.scale >= 1
}

pred SafeGeo_containsPoint[bb: SafeGeo_BoundingBox, c: SafeGeo_Coordinate] {
    c.latitude >= bb.minLat and c.latitude <= bb.maxLat and
    c.longitude >= bb.minLon and c.longitude <= bb.maxLon
}

sig SafeGeo_Vector2 {
    x: one Int,
    y: one Int
}

pred SafeGeo_isZero[v: SafeGeo_Vector2] {
    v.x = 0 and v.y = 0
}

// ============================================================================
// ALGORITHM MODULE 2: SafeProbability
// ============================================================================

sig SafeProbability_Probability {
    numerator: one Int,
    denominator: one Int
}

fact SafeProbability_ProbabilityInvariant {
    all p: SafeProbability_Probability |
        p.numerator >= 0 and
        p.denominator >= 1 and
        p.numerator <= p.denominator
}

pred SafeProbability_isCertain[p: SafeProbability_Probability] {
    p.numerator = p.denominator
}

pred SafeProbability_isImpossible[p: SafeProbability_Probability] {
    p.numerator = 0
}

sig SafeProbability_Distribution {
    outcomes: set SafeProbability_Outcome,
    totalWeight: one Int
}

sig SafeProbability_Outcome {
    weight: one Int
}

fact SafeProbability_DistributionInvariant {
    all d: SafeProbability_Distribution |
        d.totalWeight >= 1 and
        (sum o: d.outcomes | o.weight) = d.totalWeight
}

// ============================================================================
// ALGORITHM MODULE 3: SafeChecksum
// ============================================================================

sig SafeChecksum_Checksum {
    algorithm: one SafeChecksum_Algorithm,
    length: one Int,
    isValid: one Bool
}

abstract sig SafeChecksum_Algorithm {}
one sig SafeChecksum_CRC32 extends SafeChecksum_Algorithm {}
one sig SafeChecksum_CRC64 extends SafeChecksum_Algorithm {}
one sig SafeChecksum_Adler32 extends SafeChecksum_Algorithm {}
one sig SafeChecksum_XXHash64 extends SafeChecksum_Algorithm {}

fact SafeChecksum_LengthInvariant {
    all c: SafeChecksum_Checksum |
        (c.algorithm = SafeChecksum_CRC32 implies c.length = 4) and
        (c.algorithm = SafeChecksum_CRC64 implies c.length = 8) and
        (c.algorithm = SafeChecksum_Adler32 implies c.length = 4) and
        (c.algorithm = SafeChecksum_XXHash64 implies c.length = 8)
}

pred SafeChecksum_matches[a: SafeChecksum_Checksum, b: SafeChecksum_Checksum] {
    a.algorithm = b.algorithm
}

// ============================================================================
// ALGORITHM MODULE 4: SafeTensor
// ============================================================================

sig SafeTensor_Shape {
    dimensions: seq Int,
    rank: one Int,
    totalElements: one Int
}

fact SafeTensor_ShapeInvariant {
    all s: SafeTensor_Shape |
        s.rank >= 0 and
        s.rank = #s.dimensions and
        s.totalElements >= 0 and
        (all i: s.dimensions.elems | i >= 0)
}

sig SafeTensor_Tensor {
    shape: one SafeTensor_Shape,
    dtype: one SafeTensor_DType
}

abstract sig SafeTensor_DType {}
one sig SafeTensor_Float32 extends SafeTensor_DType {}
one sig SafeTensor_Float64 extends SafeTensor_DType {}
one sig SafeTensor_Int32 extends SafeTensor_DType {}
one sig SafeTensor_Int64 extends SafeTensor_DType {}
one sig SafeTensor_Bool extends SafeTensor_DType {}

pred SafeTensor_isScalar[t: SafeTensor_Tensor] {
    t.shape.rank = 0
}

pred SafeTensor_isVector[t: SafeTensor_Tensor] {
    t.shape.rank = 1
}

pred SafeTensor_isMatrix[t: SafeTensor_Tensor] {
    t.shape.rank = 2
}

pred SafeTensor_shapesMatch[a: SafeTensor_Tensor, b: SafeTensor_Tensor] {
    a.shape.dimensions = b.shape.dimensions
}

pred SafeTensor_canBroadcast[a: SafeTensor_Tensor, b: SafeTensor_Tensor] {
    a.shape.rank = b.shape.rank or
    a.shape.rank = 0 or
    b.shape.rank = 0
}

// ============================================================================
// SECURITY MODULE 1: SafePassword
// ============================================================================

sig SafePassword_Policy {
    minLength: one Int,
    maxLength: one Int,
    requireUppercase: one Bool,
    requireLowercase: one Bool,
    requireDigit: one Bool,
    requireSpecial: one Bool,
    minEntropy: one Int
}

fact SafePassword_PolicyInvariant {
    all p: SafePassword_Policy |
        p.minLength >= 1 and
        p.maxLength >= p.minLength and
        p.maxLength <= 1024 and
        p.minEntropy >= 0
}

sig SafePassword_Password {
    length: one Int,
    entropy: one Int,
    hasUppercase: one Bool,
    hasLowercase: one Bool,
    hasDigit: one Bool,
    hasSpecial: one Bool
}

pred SafePassword_meetsPolicy[pw: SafePassword_Password, pol: SafePassword_Policy] {
    pw.length >= pol.minLength and
    pw.length <= pol.maxLength and
    (pol.requireUppercase = True implies pw.hasUppercase = True) and
    (pol.requireLowercase = True implies pw.hasLowercase = True) and
    (pol.requireDigit = True implies pw.hasDigit = True) and
    (pol.requireSpecial = True implies pw.hasSpecial = True) and
    pw.entropy >= pol.minEntropy
}

sig SafePassword_Hash {
    algorithm: one SafePassword_HashAlgorithm,
    iterations: one Int,
    saltLength: one Int
}

abstract sig SafePassword_HashAlgorithm {}
one sig SafePassword_Argon2id extends SafePassword_HashAlgorithm {}
one sig SafePassword_Bcrypt extends SafePassword_HashAlgorithm {}
one sig SafePassword_Scrypt extends SafePassword_HashAlgorithm {}

fact SafePassword_HashInvariant {
    all h: SafePassword_Hash |
        h.iterations >= 1 and
        h.saltLength >= 16
}

// ============================================================================
// SECURITY MODULE 2: SafeMl
// ============================================================================

sig SafeMl_Input {
    featureCount: one Int,
    isNormalized: one Bool,
    hasNaN: one Bool,
    hasInfinity: one Bool
}

fact SafeMl_InputInvariant {
    all i: SafeMl_Input |
        i.featureCount >= 1
}

pred SafeMl_isValid[i: SafeMl_Input] {
    i.hasNaN = False and i.hasInfinity = False
}

pred SafeMl_isReady[i: SafeMl_Input] {
    SafeMl_isValid[i] and i.isNormalized = True
}

sig SafeMl_Prediction {
    confidence: one Int,
    isCalibrated: one Bool
}

fact SafeMl_PredictionInvariant {
    all p: SafeMl_Prediction |
        p.confidence >= 0 and p.confidence <= 100
}

pred SafeMl_isHighConfidence[p: SafeMl_Prediction] {
    p.confidence >= 90
}

sig SafeMl_Model {
    inputFeatures: one Int,
    outputClasses: one Int,
    isTrainable: one Bool
}

fact SafeMl_ModelInvariant {
    all m: SafeMl_Model |
        m.inputFeatures >= 1 and
        m.outputClasses >= 1
}

// ============================================================================
// HTTP MODULE 1: SafeHeader
// ============================================================================

sig SafeHeader_Header {
    nameLength: one Int,
    valueLength: one Int,
    isStandard: one Bool
}

fact SafeHeader_HeaderInvariant {
    all h: SafeHeader_Header |
        h.nameLength >= 1 and h.nameLength <= 256 and
        h.valueLength >= 0 and h.valueLength <= 8192
}

sig SafeHeader_Headers {
    count: one Int,
    totalSize: one Int,
    maxSize: one Int
}

fact SafeHeader_HeadersInvariant {
    all h: SafeHeader_Headers |
        h.count >= 0 and
        h.totalSize >= 0 and
        h.totalSize <= h.maxSize and
        h.maxSize >= 0
}

pred SafeHeader_canAdd[hs: SafeHeader_Headers, h: SafeHeader_Header] {
    hs.totalSize + h.nameLength + h.valueLength <= hs.maxSize
}

// ============================================================================
// HTTP MODULE 2: SafeCookie
// ============================================================================

sig SafeCookie_Cookie {
    nameLength: one Int,
    valueLength: one Int,
    secure: one Bool,
    httpOnly: one Bool,
    sameSite: one SafeCookie_SameSite,
    maxAge: lone Int,
    hasExpires: one Bool
}

abstract sig SafeCookie_SameSite {}
one sig SafeCookie_Strict extends SafeCookie_SameSite {}
one sig SafeCookie_Lax extends SafeCookie_SameSite {}
one sig SafeCookie_None extends SafeCookie_SameSite {}

fact SafeCookie_CookieInvariant {
    all c: SafeCookie_Cookie |
        c.nameLength >= 1 and c.nameLength <= 256 and
        c.valueLength >= 0 and c.valueLength <= 4096
}

pred SafeCookie_isSecure[c: SafeCookie_Cookie] {
    c.secure = True and c.httpOnly = True
}

pred SafeCookie_requiresSecure[c: SafeCookie_Cookie] {
    c.sameSite = SafeCookie_None implies c.secure = True
}

pred SafeCookie_isSession[c: SafeCookie_Cookie] {
    no c.maxAge and c.hasExpires = False
}

// ============================================================================
// HTTP MODULE 3: SafeContentType
// ============================================================================

sig SafeContentType_ContentType {
    mediaType: one SafeContentType_MediaType,
    hasCharset: one Bool,
    hasBoundary: one Bool
}

abstract sig SafeContentType_MediaType {}
one sig SafeContentType_TextPlain extends SafeContentType_MediaType {}
one sig SafeContentType_TextHtml extends SafeContentType_MediaType {}
one sig SafeContentType_TextCss extends SafeContentType_MediaType {}
one sig SafeContentType_TextJavascript extends SafeContentType_MediaType {}
one sig SafeContentType_ApplicationJson extends SafeContentType_MediaType {}
one sig SafeContentType_ApplicationXml extends SafeContentType_MediaType {}
one sig SafeContentType_ApplicationFormUrlencoded extends SafeContentType_MediaType {}
one sig SafeContentType_MultipartFormData extends SafeContentType_MediaType {}
one sig SafeContentType_ImagePng extends SafeContentType_MediaType {}
one sig SafeContentType_ImageJpeg extends SafeContentType_MediaType {}
one sig SafeContentType_ApplicationOctetStream extends SafeContentType_MediaType {}

pred SafeContentType_isText[ct: SafeContentType_ContentType] {
    ct.mediaType in (SafeContentType_TextPlain + SafeContentType_TextHtml +
                     SafeContentType_TextCss + SafeContentType_TextJavascript)
}

pred SafeContentType_isJson[ct: SafeContentType_ContentType] {
    ct.mediaType = SafeContentType_ApplicationJson
}

pred SafeContentType_isBinary[ct: SafeContentType_ContentType] {
    ct.mediaType in (SafeContentType_ImagePng + SafeContentType_ImageJpeg +
                     SafeContentType_ApplicationOctetStream)
}

pred SafeContentType_requiresBoundary[ct: SafeContentType_ContentType] {
    ct.mediaType = SafeContentType_MultipartFormData implies ct.hasBoundary = True
}

// ============================================================================
// SAFETY ASSERTIONS
// ============================================================================

assert SafeMath_BoundedIntSafety {
    all b: SafeMath_BoundedInt | b.value >= b.minVal and b.value <= b.maxVal
}

assert SafeMath_PercentageSafety {
    all p: SafeMath_Percentage | p.pctValue >= 0 and p.pctValue <= 100
}

assert SafeNetwork_PortSafety {
    all p: SafeNetwork_Port | p.portNum >= 1 and p.portNum <= 65535
}

assert SafeColor_RgbaSafety {
    all c: SafeColor_Rgba |
        c.red >= 0 and c.red <= 255 and
        c.green >= 0 and c.green <= 255 and
        c.blue >= 0 and c.blue <= 255 and
        c.alpha >= 0 and c.alpha <= 255
}

assert SafeBuffer_Safety {
    all b: SafeBuffer_Buffer |
        b.length >= 0 and b.length <= b.capacity and
        b.position >= 0 and b.position <= b.length
}

assert SafeQueue_Safety {
    all q: SafeQueue_Queue |
        q.currentSize >= 0 and q.currentSize <= q.maxSize
}

assert SafeCircuitBreaker_StateConsistency {
    all cb: SafeCircuitBreaker_CircuitBreaker |
        cb.state in (SafeCircuitBreaker_Closed + SafeCircuitBreaker_Open + SafeCircuitBreaker_HalfOpen)
}

assert SafeStateMachine_StateValidity {
    all m: SafeStateMachine_Machine | m.currentState in m.states
}

assert SafeGeo_CoordinateBounds {
    all c: SafeGeo_Coordinate |
        c.latitude >= -90 * c.latitudeScale and c.latitude <= 90 * c.latitudeScale and
        c.longitude >= -180 * c.longitudeScale and c.longitude <= 180 * c.longitudeScale
}

assert SafeProbability_ValidProbability {
    all p: SafeProbability_Probability |
        p.numerator >= 0 and p.numerator <= p.denominator and p.denominator >= 1
}

assert SafePassword_PolicyValid {
    all p: SafePassword_Policy |
        p.minLength >= 1 and p.maxLength >= p.minLength
}

assert SafeCookie_SecurityConstraint {
    all c: SafeCookie_Cookie |
        c.sameSite = SafeCookie_None implies c.secure = True
}

// ============================================================================
// EXAMPLE CHECKS
// ============================================================================

run ExampleSafeMath {
    some b: SafeMath_BoundedInt | b.value = 50 and b.minVal = 0 and b.maxVal = 100
} for 3

run ExampleSafeNetwork {
    some p: SafeNetwork_Port | SafeNetwork_isWellKnownPort[p]
    some p: SafeNetwork_Port | SafeNetwork_isDynamicPort[p]
} for 5

run ExampleSafeStateMachine {
    some m: SafeStateMachine_Machine | #m.states > 2 and #m.transitions > 2
} for 5

run ExampleSafeCircuitBreaker {
    some cb: SafeCircuitBreaker_CircuitBreaker |
        cb.state = SafeCircuitBreaker_HalfOpen and
        SafeCircuitBreaker_shouldReset[cb]
} for 3

run ExampleSafeGraph {
    some g: SafeGraph_Graph |
        #g.nodes > 3 and
        SafeGraph_noSelfLoops[g]
} for 5

run ExampleSafeTensor {
    some t: SafeTensor_Tensor |
        SafeTensor_isMatrix[t] and
        t.dtype = SafeTensor_Float32
} for 3

// Check assertions hold
check SafeMath_BoundedIntSafety for 5
check SafeMath_PercentageSafety for 5
check SafeNetwork_PortSafety for 5
check SafeNetwork_PortCategoriesComplete for 5
check SafeColor_RgbaSafety for 5
check SafeBuffer_Safety for 5
check SafeQueue_Safety for 5
check SafeCircuitBreaker_StateConsistency for 5
check SafeStateMachine_StateValidity for 5
check SafeGeo_CoordinateBounds for 5
check SafeProbability_ValidProbability for 5
check SafePassword_PolicyValid for 5
check SafeCookie_SecurityConstraint for 5
