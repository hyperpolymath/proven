// SPDX-License-Identifier: PMPL-1.0
// Proven - Safety primitives for Q# (Microsoft Quantum SDK)
// Version: 0.4.0 | Modules: 38

namespace Proven {
    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Arrays;

    // ========================================================================
    // VERSION AND MODULE COUNT
    // ========================================================================

    function VERSION() : String { return "0.4.0"; }
    function MODULE_COUNT() : Int { return 38; }

    // ========================================================================
    // RESULT TYPE
    // ========================================================================

    newtype QuantumResult<'T> = (IsOk: Bool, Value: 'T, ErrorCode: Int, ErrorMsg: String);

    function Ok<'T>(value: 'T) : QuantumResult<'T> {
        return QuantumResult(true, value, 0, "");
    }

    function Err<'T>(code: Int, msg: String, defaultValue: 'T) : QuantumResult<'T> {
        return QuantumResult(false, defaultValue, code, msg);
    }

    function UnwrapOr<'T>(result: QuantumResult<'T>, defaultValue: 'T) : 'T {
        if result::IsOk { return result::Value; }
        return defaultValue;
    }

    function IsOk<'T>(result: QuantumResult<'T>) : Bool {
        return result::IsOk;
    }

    function IsErr<'T>(result: QuantumResult<'T>) : Bool {
        return not result::IsOk;
    }

    // ========================================================================
    // OPTION TYPE
    // ========================================================================

    newtype Option<'T> = (IsSome: Bool, Value: 'T);

    function Some<'T>(value: 'T) : Option<'T> { return Option(true, value); }
    function None<'T>(defaultValue: 'T) : Option<'T> { return Option(false, defaultValue); }

    function IsSome<'T>(opt: Option<'T>) : Bool { return opt::IsSome; }
    function IsNone<'T>(opt: Option<'T>) : Bool { return not opt::IsSome; }

    function OptionUnwrapOr<'T>(opt: Option<'T>, defaultValue: 'T) : 'T {
        if opt::IsSome { return opt::Value; }
        return defaultValue;
    }

    // ========================================================================
    // CONSTANTS
    // ========================================================================

    function TAU() : Double { return 2.0 * PI(); }
    function ProbEpsilon() : Double { return 1e-10; }
    function FidelityThreshold() : Double { return 0.99; }
    function ErrorThreshold() : Double { return 0.01; }
    function MaxShots() : Int { return 1000000; }
    function MaxDepth() : Int { return 10000; }
    function MaxInt64() : Int { return 9223372036854775807; }
    function MinInt64() : Int { return -9223372036854775808; }

    // ========================================================================
    // MODULE 1: SAFE MATH
    // ========================================================================

    function SafeAdd(a: Int, b: Int) : QuantumResult<Int> {
        if b > 0 and a > MaxInt64() - b {
            return Err(1, "Integer overflow in addition", 0);
        }
        if b < 0 and a < MinInt64() - b {
            return Err(2, "Integer underflow in addition", 0);
        }
        return Ok(a + b);
    }

    function SafeSub(a: Int, b: Int) : QuantumResult<Int> {
        if b < 0 and a > MaxInt64() + b {
            return Err(1, "Integer overflow in subtraction", 0);
        }
        if b > 0 and a < MinInt64() + b {
            return Err(2, "Integer underflow in subtraction", 0);
        }
        return Ok(a - b);
    }

    function SafeMul(a: Int, b: Int) : QuantumResult<Int> {
        if a == 0 or b == 0 { return Ok(0); }
        let result = a * b;
        if result / a != b {
            return Err(1, "Integer overflow in multiplication", 0);
        }
        return Ok(result);
    }

    function SafeDiv(a: Int, b: Int) : QuantumResult<Int> {
        if b == 0 {
            return Err(1, "Division by zero", 0);
        }
        if a == MinInt64() and b == -1 {
            return Err(2, "Integer overflow in division", 0);
        }
        return Ok(a / b);
    }

    function SafeMod(a: Int, b: Int) : QuantumResult<Int> {
        if b == 0 {
            return Err(1, "Modulo by zero", 0);
        }
        return Ok(a % b);
    }

    function SafeNegate(a: Int) : QuantumResult<Int> {
        if a == MinInt64() {
            return Err(1, "Negation overflow", 0);
        }
        return Ok(-a);
    }

    function SafeAbs(a: Int) : QuantumResult<Int> {
        if a == MinInt64() {
            return Err(1, "Absolute value overflow", 0);
        }
        if a < 0 { return Ok(-a); }
        return Ok(a);
    }

    function ClampInt(value: Int, minVal: Int, maxVal: Int) : Int {
        if value < minVal { return minVal; }
        if value > maxVal { return maxVal; }
        return value;
    }

    function SafePow(base: Int, exp: Int) : QuantumResult<Int> {
        if exp < 0 { return Err(1, "Negative exponent", 0); }
        if exp == 0 { return Ok(1); }
        if exp > 62 { return Err(2, "Exponent too large", 0); }
        mutable result = 1;
        mutable currentBase = base;
        mutable currentExp = exp;
        while currentExp > 0 {
            if currentExp % 2 == 1 {
                set result = result * currentBase;
            }
            set currentBase = currentBase * currentBase;
            set currentExp = currentExp / 2;
        }
        return Ok(result);
    }

    // ========================================================================
    // MODULE 2: SAFE STRING (Quantum Classical Hybrid Representations)
    // ========================================================================

    newtype SafeStringLength = (Value: Int, MaxLength: Int);

    function CreateSafeStringLength(length: Int, maxLength: Int) : QuantumResult<SafeStringLength> {
        if length < 0 {
            return Err(1, "Negative string length", SafeStringLength(0, maxLength));
        }
        if length > maxLength {
            return Err(2, "String exceeds maximum length", SafeStringLength(maxLength, maxLength));
        }
        return Ok(SafeStringLength(length, maxLength));
    }

    function TruncateStringLength(len: SafeStringLength, newMax: Int) : SafeStringLength {
        let clampedLength = ClampInt(len::Value, 0, newMax);
        return SafeStringLength(clampedLength, newMax);
    }

    function ConcatStringLengths(a: SafeStringLength, b: SafeStringLength) : QuantumResult<SafeStringLength> {
        let combined = a::Value + b::Value;
        let maxLen = a::MaxLength;
        if combined > maxLen {
            return Err(1, "Concatenation exceeds max length", SafeStringLength(maxLen, maxLen));
        }
        return Ok(SafeStringLength(combined, maxLen));
    }

    // ========================================================================
    // MODULE 3: SAFE PATH
    // ========================================================================

    newtype PathDepth = (Value: Int, MaxDepth: Int);

    function CreatePathDepth(depth: Int, maxDepth: Int) : QuantumResult<PathDepth> {
        if depth < 0 {
            return Err(1, "Negative path depth", PathDepth(0, maxDepth));
        }
        if depth > maxDepth {
            return Err(2, "Path too deep", PathDepth(maxDepth, maxDepth));
        }
        return Ok(PathDepth(depth, maxDepth));
    }

    function IsValidPathDepth(depth: PathDepth) : Bool {
        return depth::Value >= 0 and depth::Value <= depth::MaxDepth;
    }

    function TraversalCheck(currentDepth: Int, traversals: Int) : QuantumResult<Int> {
        let newDepth = currentDepth - traversals;
        if newDepth < 0 {
            return Err(1, "Path traversal attack detected", 0);
        }
        return Ok(newDepth);
    }

    // ========================================================================
    // MODULE 4: SAFE EMAIL
    // ========================================================================

    newtype EmailParts = (LocalLength: Int, DomainLength: Int, MaxLocalLength: Int, MaxDomainLength: Int);

    function CreateEmailParts(localLen: Int, domainLen: Int) : QuantumResult<EmailParts> {
        let maxLocal = 64;
        let maxDomain = 255;
        if localLen < 1 or localLen > maxLocal {
            return Err(1, "Invalid local part length", EmailParts(0, 0, maxLocal, maxDomain));
        }
        if domainLen < 1 or domainLen > maxDomain {
            return Err(2, "Invalid domain length", EmailParts(0, 0, maxLocal, maxDomain));
        }
        return Ok(EmailParts(localLen, domainLen, maxLocal, maxDomain));
    }

    function IsValidEmailLength(email: EmailParts) : Bool {
        let totalLength = email::LocalLength + 1 + email::DomainLength;
        return totalLength <= 254;
    }

    // ========================================================================
    // MODULE 5: SAFE URL
    // ========================================================================

    newtype UrlComponents = (SchemeLen: Int, HostLen: Int, PathLen: Int, QueryLen: Int, MaxTotal: Int);

    function CreateUrlComponents(scheme: Int, host: Int, path: Int, query: Int, maxTotal: Int) : QuantumResult<UrlComponents> {
        let total = scheme + host + path + query + 6;
        if total > maxTotal {
            return Err(1, "URL exceeds maximum length", UrlComponents(0, 0, 0, 0, maxTotal));
        }
        if scheme < 1 {
            return Err(2, "Invalid scheme length", UrlComponents(0, 0, 0, 0, maxTotal));
        }
        return Ok(UrlComponents(scheme, host, path, query, maxTotal));
    }

    function UrlTotalLength(url: UrlComponents) : Int {
        return url::SchemeLen + url::HostLen + url::PathLen + url::QueryLen + 6;
    }

    // ========================================================================
    // MODULE 6: SAFE NETWORK
    // ========================================================================

    newtype IPv4Address = (Octet1: Int, Octet2: Int, Octet3: Int, Octet4: Int);

    function CreateIPv4(o1: Int, o2: Int, o3: Int, o4: Int) : QuantumResult<IPv4Address> {
        if o1 < 0 or o1 > 255 or o2 < 0 or o2 > 255 or o3 < 0 or o3 > 255 or o4 < 0 or o4 > 255 {
            return Err(1, "Invalid IPv4 octet", IPv4Address(0, 0, 0, 0));
        }
        return Ok(IPv4Address(o1, o2, o3, o4));
    }

    function IsPrivateIPv4(ip: IPv4Address) : Bool {
        if ip::Octet1 == 10 { return true; }
        if ip::Octet1 == 172 and ip::Octet2 >= 16 and ip::Octet2 <= 31 { return true; }
        if ip::Octet1 == 192 and ip::Octet2 == 168 { return true; }
        return false;
    }

    function IsLoopbackIPv4(ip: IPv4Address) : Bool {
        return ip::Octet1 == 127;
    }

    newtype Port = (Value: Int);

    function CreatePort(port: Int) : QuantumResult<Port> {
        if port < 0 or port > 65535 {
            return Err(1, "Invalid port number", Port(0));
        }
        return Ok(Port(port));
    }

    function IsPrivilegedPort(port: Port) : Bool {
        return port::Value < 1024;
    }

    // ========================================================================
    // MODULE 7: SAFE CRYPTO (Quantum-Specific)
    // ========================================================================

    newtype HashLength = (Bits: Int);

    function SHA3_256_Length() : HashLength { return HashLength(256); }
    function SHA3_512_Length() : HashLength { return HashLength(512); }
    function BLAKE3_Length() : HashLength { return HashLength(256); }

    function IsQuantumResistantHash(hash: HashLength) : Bool {
        return hash::Bits >= 256;
    }

    newtype KeyStrength = (Bits: Int, IsPostQuantum: Bool);

    function CreateKeyStrength(bits: Int, postQuantum: Bool) : QuantumResult<KeyStrength> {
        if bits < 128 {
            return Err(1, "Key strength too weak", KeyStrength(0, false));
        }
        return Ok(KeyStrength(bits, postQuantum));
    }

    function IsQuantumSafe(key: KeyStrength) : Bool {
        return key::IsPostQuantum or key::Bits >= 256;
    }

    function ConstantTimeCompareLength(a: Int, b: Int) : Bool {
        return a == b;
    }

    // ========================================================================
    // MODULE 8: SAFE UUID
    // ========================================================================

    newtype UUIDVersion = (Version: Int);

    function CreateUUIDVersion(version: Int) : QuantumResult<UUIDVersion> {
        if version < 1 or version > 8 {
            return Err(1, "Invalid UUID version", UUIDVersion(4));
        }
        return Ok(UUIDVersion(version));
    }

    function IsRandomUUID(uuid: UUIDVersion) : Bool {
        return uuid::Version == 4;
    }

    function UUIDBitLength() : Int { return 128; }

    // ========================================================================
    // MODULE 9: SAFE CURRENCY
    // ========================================================================

    newtype Money = (Amount: Int, Decimals: Int, CurrencyCode: Int);

    function CreateMoney(amount: Int, decimals: Int, code: Int) : QuantumResult<Money> {
        if decimals < 0 or decimals > 18 {
            return Err(1, "Invalid decimal places", Money(0, 2, 840));
        }
        return Ok(Money(amount, decimals, code));
    }

    function AddMoney(a: Money, b: Money) : QuantumResult<Money> {
        if a::CurrencyCode != b::CurrencyCode {
            return Err(1, "Currency mismatch", a);
        }
        if a::Decimals != b::Decimals {
            return Err(2, "Decimal mismatch", a);
        }
        let sumResult = SafeAdd(a::Amount, b::Amount);
        if IsErr(sumResult) {
            return Err(3, "Amount overflow", a);
        }
        return Ok(Money(UnwrapOr(sumResult, 0), a::Decimals, a::CurrencyCode));
    }

    function IsValidAmount(money: Money) : Bool {
        return money::Amount >= 0;
    }

    // ========================================================================
    // MODULE 10: SAFE PHONE
    // ========================================================================

    newtype PhoneNumber = (CountryCode: Int, Number: Int, Length: Int);

    function CreatePhoneNumber(countryCode: Int, number: Int, length: Int) : QuantumResult<PhoneNumber> {
        if countryCode < 1 or countryCode > 999 {
            return Err(1, "Invalid country code", PhoneNumber(1, 0, 0));
        }
        if length < 4 or length > 15 {
            return Err(2, "Invalid phone number length", PhoneNumber(1, 0, 0));
        }
        return Ok(PhoneNumber(countryCode, number, length));
    }

    function IsE164Compliant(phone: PhoneNumber) : Bool {
        let totalDigits = 1 + phone::Length;
        return totalDigits <= 15;
    }

    // ========================================================================
    // MODULE 11: SAFE HEX
    // ========================================================================

    function HexDigitValue(digit: Int) : QuantumResult<Int> {
        if digit >= 48 and digit <= 57 { return Ok(digit - 48); }
        if digit >= 65 and digit <= 70 { return Ok(digit - 55); }
        if digit >= 97 and digit <= 102 { return Ok(digit - 87); }
        return Err(1, "Invalid hex digit", 0);
    }

    function ByteToHexLength(byteCount: Int) : Int {
        return byteCount * 2;
    }

    function HexToByteLength(hexLength: Int) : QuantumResult<Int> {
        if hexLength % 2 != 0 {
            return Err(1, "Odd hex string length", 0);
        }
        return Ok(hexLength / 2);
    }

    // ========================================================================
    // MODULE 12: SAFE JSON
    // ========================================================================

    newtype JsonDepth = (Current: Int, Max: Int);

    function CreateJsonDepth(current: Int, maxDepth: Int) : QuantumResult<JsonDepth> {
        if current < 0 {
            return Err(1, "Negative depth", JsonDepth(0, maxDepth));
        }
        if current > maxDepth {
            return Err(2, "JSON nesting too deep", JsonDepth(maxDepth, maxDepth));
        }
        return Ok(JsonDepth(current, maxDepth));
    }

    function IncrementJsonDepth(depth: JsonDepth) : QuantumResult<JsonDepth> {
        return CreateJsonDepth(depth::Current + 1, depth::Max);
    }

    function DecrementJsonDepth(depth: JsonDepth) : QuantumResult<JsonDepth> {
        if depth::Current <= 0 {
            return Err(1, "Cannot decrement below zero", depth);
        }
        return Ok(JsonDepth(depth::Current - 1, depth::Max));
    }

    // ========================================================================
    // MODULE 13: SAFE DATETIME
    // ========================================================================

    newtype DateTime = (Year: Int, Month: Int, Day: Int, Hour: Int, Minute: Int, Second: Int);

    function CreateDateTime(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int) : QuantumResult<DateTime> {
        if month < 1 or month > 12 {
            return Err(1, "Invalid month", DateTime(1970, 1, 1, 0, 0, 0));
        }
        if day < 1 or day > 31 {
            return Err(2, "Invalid day", DateTime(1970, 1, 1, 0, 0, 0));
        }
        if hour < 0 or hour > 23 {
            return Err(3, "Invalid hour", DateTime(1970, 1, 1, 0, 0, 0));
        }
        if minute < 0 or minute > 59 {
            return Err(4, "Invalid minute", DateTime(1970, 1, 1, 0, 0, 0));
        }
        if second < 0 or second > 59 {
            return Err(5, "Invalid second", DateTime(1970, 1, 1, 0, 0, 0));
        }
        return Ok(DateTime(year, month, day, hour, minute, second));
    }

    function IsLeapYear(year: Int) : Bool {
        if year % 400 == 0 { return true; }
        if year % 100 == 0 { return false; }
        return year % 4 == 0;
    }

    function DaysInMonth(year: Int, month: Int) : Int {
        if month == 2 {
            if IsLeapYear(year) { return 29; }
            return 28;
        }
        if month == 4 or month == 6 or month == 9 or month == 11 {
            return 30;
        }
        return 31;
    }

    // ========================================================================
    // MODULE 14: SAFE FLOAT
    // ========================================================================

    function ClampDouble(value: Double, minVal: Double, maxVal: Double) : Double {
        if value < minVal { return minVal; }
        if value > maxVal { return maxVal; }
        return value;
    }

    function SafeDivDouble(a: Double, b: Double) : QuantumResult<Double> {
        if AbsD(b) < ProbEpsilon() {
            return Err(1, "Division by near-zero", 0.0);
        }
        return Ok(a / b);
    }

    function IsFiniteDouble(value: Double) : Bool {
        return value == value and AbsD(value) < 1.0e308;
    }

    function SafeSqrt(value: Double) : QuantumResult<Double> {
        if value < 0.0 {
            return Err(1, "Square root of negative number", 0.0);
        }
        return Ok(Sqrt(value));
    }

    function SafeLog(value: Double) : QuantumResult<Double> {
        if value <= 0.0 {
            return Err(1, "Logarithm of non-positive number", 0.0);
        }
        return Ok(Log(value));
    }

    // ========================================================================
    // MODULE 15: SAFE VERSION
    // ========================================================================

    newtype SemanticVersion = (Major: Int, Minor: Int, Patch: Int);

    function CreateVersion(major: Int, minor: Int, patch: Int) : QuantumResult<SemanticVersion> {
        if major < 0 or minor < 0 or patch < 0 {
            return Err(1, "Negative version component", SemanticVersion(0, 0, 0));
        }
        return Ok(SemanticVersion(major, minor, patch));
    }

    function CompareVersions(a: SemanticVersion, b: SemanticVersion) : Int {
        if a::Major != b::Major { return a::Major - b::Major; }
        if a::Minor != b::Minor { return a::Minor - b::Minor; }
        return a::Patch - b::Patch;
    }

    function IsCompatible(current: SemanticVersion, required: SemanticVersion) : Bool {
        if current::Major != required::Major { return false; }
        if current::Minor < required::Minor { return false; }
        return true;
    }

    // ========================================================================
    // MODULE 16: SAFE COLOR
    // ========================================================================

    newtype Rgb = (Red: Int, Green: Int, Blue: Int);
    newtype Rgba = (Red: Int, Green: Int, Blue: Int, Alpha: Int);

    function CreateRgb(r: Int, g: Int, b: Int) : QuantumResult<Rgb> {
        if r < 0 or r > 255 or g < 0 or g > 255 or b < 0 or b > 255 {
            return Err(1, "RGB component out of range", Rgb(0, 0, 0));
        }
        return Ok(Rgb(r, g, b));
    }

    function CreateRgba(r: Int, g: Int, b: Int, a: Int) : QuantumResult<Rgba> {
        if r < 0 or r > 255 or g < 0 or g > 255 or b < 0 or b > 255 or a < 0 or a > 255 {
            return Err(1, "RGBA component out of range", Rgba(0, 0, 0, 255));
        }
        return Ok(Rgba(r, g, b, a));
    }

    function RelativeLuminance(color: Rgb) : Double {
        let r = IntAsDouble(color::Red) / 255.0;
        let g = IntAsDouble(color::Green) / 255.0;
        let b = IntAsDouble(color::Blue) / 255.0;
        return 0.2126 * r + 0.7152 * g + 0.0722 * b;
    }

    function ContrastRatio(fg: Rgb, bg: Rgb) : Double {
        let l1 = RelativeLuminance(fg) + 0.05;
        let l2 = RelativeLuminance(bg) + 0.05;
        if l1 > l2 { return l1 / l2; }
        return l2 / l1;
    }

    function MeetsWCAGAA(fg: Rgb, bg: Rgb) : Bool {
        return ContrastRatio(fg, bg) >= 4.5;
    }

    // ========================================================================
    // MODULE 17: SAFE ANGLE
    // ========================================================================

    newtype Degrees = (Value: Double);
    newtype Radians = (Value: Double);

    function CreateDegrees(value: Double) : Degrees {
        return Degrees(value);
    }

    function CreateRadians(value: Double) : Radians {
        return Radians(value);
    }

    function WrapAngle(angle: Double) : Double {
        mutable wrapped = angle;
        while wrapped > PI() { set wrapped = wrapped - TAU(); }
        while wrapped < -PI() { set wrapped = wrapped + TAU(); }
        return wrapped;
    }

    function ClampAngle(angle: Double) : Double {
        if angle < -PI() { return -PI(); }
        if angle > PI() { return PI(); }
        return angle;
    }

    function DegreesToRadians(deg: Double) : Double {
        return WrapAngle(deg * PI() / 180.0);
    }

    function RadiansToDegrees(rad: Double) : Double {
        return rad * 180.0 / PI();
    }

    function NormalizeDegrees(deg: Double) : Double {
        mutable normalized = deg;
        while normalized >= 360.0 { set normalized = normalized - 360.0; }
        while normalized < 0.0 { set normalized = normalized + 360.0; }
        return normalized;
    }

    function LerpAngleDegrees(a: Double, b: Double, t: Double) : Double {
        let diff = NormalizeDegrees(b - a);
        mutable delta = diff;
        if diff > 180.0 { set delta = diff - 360.0; }
        return NormalizeDegrees(a + delta * t);
    }

    // ========================================================================
    // MODULE 18: SAFE UNIT
    // ========================================================================

    function ConvertLength(value: Double, fromUnit: Int, toUnit: Int) : Double {
        let metersPerUnit = [1.0, 0.001, 0.01, 1000.0, 0.0254, 0.3048, 0.9144, 1609.344];
        if fromUnit < 0 or fromUnit >= 8 or toUnit < 0 or toUnit >= 8 {
            return value;
        }
        let inMeters = value * metersPerUnit[fromUnit];
        return inMeters / metersPerUnit[toUnit];
    }

    function ConvertMass(value: Double, fromUnit: Int, toUnit: Int) : Double {
        let kgPerUnit = [1.0, 0.001, 1000.0, 0.453592, 0.0283495];
        if fromUnit < 0 or fromUnit >= 5 or toUnit < 0 or toUnit >= 5 {
            return value;
        }
        let inKg = value * kgPerUnit[fromUnit];
        return inKg / kgPerUnit[toUnit];
    }

    function ConvertTemperature(value: Double, fromUnit: Int, toUnit: Int) : Double {
        mutable celsius = value;
        if fromUnit == 1 { set celsius = (value - 32.0) * 5.0 / 9.0; }
        if fromUnit == 2 { set celsius = value - 273.15; }
        if toUnit == 0 { return celsius; }
        if toUnit == 1 { return celsius * 9.0 / 5.0 + 32.0; }
        if toUnit == 2 { return celsius + 273.15; }
        return celsius;
    }

    // ========================================================================
    // MODULE 19: SAFE BUFFER
    // ========================================================================

    newtype BoundedBufferState = (Length: Int, Capacity: Int);

    function CreateBoundedBuffer(capacity: Int) : QuantumResult<BoundedBufferState> {
        if capacity <= 0 {
            return Err(1, "Invalid buffer capacity", BoundedBufferState(0, 1));
        }
        return Ok(BoundedBufferState(0, capacity));
    }

    function BufferPush(buffer: BoundedBufferState) : QuantumResult<BoundedBufferState> {
        if buffer::Length >= buffer::Capacity {
            return Err(1, "Buffer full", buffer);
        }
        return Ok(BoundedBufferState(buffer::Length + 1, buffer::Capacity));
    }

    function BufferPop(buffer: BoundedBufferState) : QuantumResult<BoundedBufferState> {
        if buffer::Length <= 0 {
            return Err(1, "Buffer empty", buffer);
        }
        return Ok(BoundedBufferState(buffer::Length - 1, buffer::Capacity));
    }

    function IsBufferFull(buffer: BoundedBufferState) : Bool {
        return buffer::Length >= buffer::Capacity;
    }

    function IsBufferEmpty(buffer: BoundedBufferState) : Bool {
        return buffer::Length <= 0;
    }

    function BufferRemaining(buffer: BoundedBufferState) : Int {
        return buffer::Capacity - buffer::Length;
    }

    // ========================================================================
    // MODULE 20: SAFE QUEUE
    // ========================================================================

    newtype QueueState = (Length: Int, Capacity: Int, Head: Int, Tail: Int);

    function CreateQueue(capacity: Int) : QuantumResult<QueueState> {
        if capacity <= 0 {
            return Err(1, "Invalid queue capacity", QueueState(0, 1, 0, 0));
        }
        return Ok(QueueState(0, capacity, 0, 0));
    }

    function Enqueue(queue: QueueState) : QuantumResult<QueueState> {
        if queue::Length >= queue::Capacity {
            return Err(1, "Queue full", queue);
        }
        let newTail = (queue::Tail + 1) % queue::Capacity;
        return Ok(QueueState(queue::Length + 1, queue::Capacity, queue::Head, newTail));
    }

    function Dequeue(queue: QueueState) : QuantumResult<QueueState> {
        if queue::Length <= 0 {
            return Err(1, "Queue empty", queue);
        }
        let newHead = (queue::Head + 1) % queue::Capacity;
        return Ok(QueueState(queue::Length - 1, queue::Capacity, newHead, queue::Tail));
    }

    function IsQueueFull(queue: QueueState) : Bool {
        return queue::Length >= queue::Capacity;
    }

    function IsQueueEmpty(queue: QueueState) : Bool {
        return queue::Length <= 0;
    }

    // ========================================================================
    // MODULE 21: SAFE BLOOM
    // ========================================================================

    newtype BloomFilterParams = (BitCount: Int, HashCount: Int, ItemCount: Int);

    function CreateBloomFilter(expectedItems: Int, falsePositiveRate: Double) : QuantumResult<BloomFilterParams> {
        if expectedItems <= 0 {
            return Err(1, "Invalid expected items", BloomFilterParams(64, 3, 0));
        }
        if falsePositiveRate <= 0.0 or falsePositiveRate >= 1.0 {
            return Err(2, "Invalid false positive rate", BloomFilterParams(64, 3, 0));
        }
        let m = Ceiling(-IntAsDouble(expectedItems) * Log(falsePositiveRate) / (Log(2.0) * Log(2.0)));
        let k = Ceiling(IntAsDouble(Truncate(m)) / IntAsDouble(expectedItems) * Log(2.0));
        return Ok(BloomFilterParams(Truncate(m), Truncate(k), 0));
    }

    function BloomFalsePositiveRate(filter: BloomFilterParams) : Double {
        let k = IntAsDouble(filter::HashCount);
        let m = IntAsDouble(filter::BitCount);
        let n = IntAsDouble(filter::ItemCount);
        return PowD(1.0 - E() ^ (-k * n / m), k);
    }

    // ========================================================================
    // MODULE 22: SAFE LRU
    // ========================================================================

    newtype LruCacheState = (ItemCount: Int, Capacity: Int, HitCount: Int, MissCount: Int);

    function CreateLruCache(capacity: Int) : QuantumResult<LruCacheState> {
        if capacity <= 0 {
            return Err(1, "Invalid cache capacity", LruCacheState(0, 1, 0, 0));
        }
        return Ok(LruCacheState(0, capacity, 0, 0));
    }

    function LruInsert(cache: LruCacheState) : LruCacheState {
        let newCount = if cache::ItemCount >= cache::Capacity {
            cache::Capacity
        } else {
            cache::ItemCount + 1
        };
        return LruCacheState(newCount, cache::Capacity, cache::HitCount, cache::MissCount);
    }

    function LruHit(cache: LruCacheState) : LruCacheState {
        return LruCacheState(cache::ItemCount, cache::Capacity, cache::HitCount + 1, cache::MissCount);
    }

    function LruMiss(cache: LruCacheState) : LruCacheState {
        return LruCacheState(cache::ItemCount, cache::Capacity, cache::HitCount, cache::MissCount + 1);
    }

    function LruHitRate(cache: LruCacheState) : Double {
        let total = cache::HitCount + cache::MissCount;
        if total == 0 { return 0.0; }
        return IntAsDouble(cache::HitCount) / IntAsDouble(total);
    }

    // ========================================================================
    // MODULE 23: SAFE GRAPH
    // ========================================================================

    newtype GraphState = (NodeCount: Int, EdgeCount: Int, MaxNodes: Int);

    function CreateGraph(maxNodes: Int) : QuantumResult<GraphState> {
        if maxNodes <= 0 {
            return Err(1, "Invalid max nodes", GraphState(0, 0, 1));
        }
        return Ok(GraphState(0, 0, maxNodes));
    }

    function AddNode(graph: GraphState) : QuantumResult<GraphState> {
        if graph::NodeCount >= graph::MaxNodes {
            return Err(1, "Graph full", graph);
        }
        return Ok(GraphState(graph::NodeCount + 1, graph::EdgeCount, graph::MaxNodes));
    }

    function AddEdge(graph: GraphState) : GraphState {
        return GraphState(graph::NodeCount, graph::EdgeCount + 1, graph::MaxNodes);
    }

    function GraphDensity(graph: GraphState) : Double {
        let n = graph::NodeCount;
        if n < 2 { return 0.0; }
        let maxEdges = n * (n - 1);
        return IntAsDouble(graph::EdgeCount) / IntAsDouble(maxEdges);
    }

    // ========================================================================
    // MODULE 24: SAFE RATE LIMITER
    // ========================================================================

    newtype TokenBucket = (Tokens: Int, Capacity: Int, RefillRate: Int, LastRefill: Int);

    function CreateTokenBucket(capacity: Int, refillRate: Int) : QuantumResult<TokenBucket> {
        if capacity <= 0 or refillRate <= 0 {
            return Err(1, "Invalid bucket parameters", TokenBucket(0, 1, 1, 0));
        }
        return Ok(TokenBucket(capacity, capacity, refillRate, 0));
    }

    function RefillBucket(bucket: TokenBucket, currentTime: Int) : TokenBucket {
        let elapsed = currentTime - bucket::LastRefill;
        let newTokens = bucket::RefillRate * elapsed;
        let tokens = ClampInt(bucket::Tokens + newTokens, 0, bucket::Capacity);
        return TokenBucket(tokens, bucket::Capacity, bucket::RefillRate, currentTime);
    }

    function TryAcquire(bucket: TokenBucket, count: Int, currentTime: Int) : (Bool, TokenBucket) {
        let refilled = RefillBucket(bucket, currentTime);
        if refilled::Tokens >= count {
            let newBucket = TokenBucket(
                refilled::Tokens - count,
                refilled::Capacity,
                refilled::RefillRate,
                refilled::LastRefill
            );
            return (true, newBucket);
        }
        return (false, refilled);
    }

    // ========================================================================
    // MODULE 25: SAFE CIRCUIT BREAKER
    // ========================================================================

    newtype CircuitState = (State: Int, FailureCount: Int, SuccessCount: Int, LastStateChange: Int);

    function CircuitClosed() : Int { return 0; }
    function CircuitOpen() : Int { return 1; }
    function CircuitHalfOpen() : Int { return 2; }

    function CreateCircuitBreaker() : CircuitState {
        return CircuitState(CircuitClosed(), 0, 0, 0);
    }

    function RecordSuccess(circuit: CircuitState, currentTime: Int) : CircuitState {
        if circuit::State == CircuitHalfOpen() {
            return CircuitState(CircuitClosed(), 0, 0, currentTime);
        }
        return CircuitState(circuit::State, 0, circuit::SuccessCount + 1, circuit::LastStateChange);
    }

    function RecordFailure(circuit: CircuitState, threshold: Int, currentTime: Int) : CircuitState {
        let newFailures = circuit::FailureCount + 1;
        if newFailures >= threshold {
            return CircuitState(CircuitOpen(), newFailures, 0, currentTime);
        }
        return CircuitState(circuit::State, newFailures, circuit::SuccessCount, circuit::LastStateChange);
    }

    function ShouldAllowRequest(circuit: CircuitState, timeout: Int, currentTime: Int) : Bool {
        if circuit::State == CircuitClosed() { return true; }
        if circuit::State == CircuitOpen() {
            return currentTime - circuit::LastStateChange >= timeout;
        }
        return true;
    }

    // ========================================================================
    // MODULE 26: SAFE RETRY
    // ========================================================================

    newtype RetryState = (Attempt: Int, MaxAttempts: Int, BaseDelay: Int, MaxDelay: Int);

    function CreateRetryState(maxAttempts: Int, baseDelay: Int, maxDelay: Int) : QuantumResult<RetryState> {
        if maxAttempts <= 0 {
            return Err(1, "Invalid max attempts", RetryState(0, 1, 1000, 30000));
        }
        return Ok(RetryState(0, maxAttempts, baseDelay, maxDelay));
    }

    function NextRetry(state: RetryState) : QuantumResult<RetryState> {
        if state::Attempt >= state::MaxAttempts {
            return Err(1, "Max retries exceeded", state);
        }
        return Ok(RetryState(state::Attempt + 1, state::MaxAttempts, state::BaseDelay, state::MaxDelay));
    }

    function ExponentialBackoff(state: RetryState) : Int {
        let delay = state::BaseDelay * (1 <<< state::Attempt);
        return ClampInt(delay, state::BaseDelay, state::MaxDelay);
    }

    function ShouldRetry(state: RetryState) : Bool {
        return state::Attempt < state::MaxAttempts;
    }

    // ========================================================================
    // MODULE 27: SAFE MONOTONIC
    // ========================================================================

    newtype MonotonicCounter = (Value: Int, MaxValue: Int);

    function CreateMonotonicCounter(maxValue: Int) : QuantumResult<MonotonicCounter> {
        if maxValue <= 0 {
            return Err(1, "Invalid max value", MonotonicCounter(0, MaxInt64()));
        }
        return Ok(MonotonicCounter(0, maxValue));
    }

    function IncrementMonotonic(counter: MonotonicCounter) : QuantumResult<MonotonicCounter> {
        if counter::Value >= counter::MaxValue {
            return Err(1, "Counter overflow", counter);
        }
        return Ok(MonotonicCounter(counter::Value + 1, counter::MaxValue));
    }

    function GetMonotonicValue(counter: MonotonicCounter) : Int {
        return counter::Value;
    }

    newtype HighWaterMark = (Current: Int, Maximum: Int);

    function CreateHighWaterMark() : HighWaterMark {
        return HighWaterMark(0, 0);
    }

    function UpdateHighWaterMark(mark: HighWaterMark, value: Int) : HighWaterMark {
        if value > mark::Maximum {
            return HighWaterMark(value, value);
        }
        return HighWaterMark(value, mark::Maximum);
    }

    // ========================================================================
    // MODULE 28: SAFE STATE MACHINE
    // ========================================================================

    newtype StateMachineState = (CurrentState: Int, PreviousState: Int, TransitionCount: Int);

    function CreateStateMachine(initialState: Int) : StateMachineState {
        return StateMachineState(initialState, -1, 0);
    }

    function TransitionTo(machine: StateMachineState, newState: Int) : StateMachineState {
        return StateMachineState(newState, machine::CurrentState, machine::TransitionCount + 1);
    }

    function GetCurrentState(machine: StateMachineState) : Int {
        return machine::CurrentState;
    }

    function GetPreviousState(machine: StateMachineState) : Option<Int> {
        if machine::PreviousState < 0 {
            return None(-1);
        }
        return Some(machine::PreviousState);
    }

    // ========================================================================
    // MODULE 29: SAFE CALCULATOR
    // ========================================================================

    newtype CalculatorState = (Accumulator: Int, LastOperand: Int, Operation: Int);

    function OpNone() : Int { return 0; }
    function OpAdd() : Int { return 1; }
    function OpSub() : Int { return 2; }
    function OpMul() : Int { return 3; }
    function OpDiv() : Int { return 4; }

    function CreateCalculator() : CalculatorState {
        return CalculatorState(0, 0, OpNone());
    }

    function SetValue(calc: CalculatorState, value: Int) : CalculatorState {
        return CalculatorState(value, calc::LastOperand, calc::Operation);
    }

    function SetOperation(calc: CalculatorState, op: Int) : CalculatorState {
        return CalculatorState(calc::Accumulator, calc::Accumulator, op);
    }

    function Calculate(calc: CalculatorState, value: Int) : QuantumResult<CalculatorState> {
        if calc::Operation == OpAdd() {
            let result = SafeAdd(calc::LastOperand, value);
            if IsErr(result) { return Err(1, "Overflow", calc); }
            return Ok(CalculatorState(UnwrapOr(result, 0), value, OpNone()));
        }
        if calc::Operation == OpSub() {
            let result = SafeSub(calc::LastOperand, value);
            if IsErr(result) { return Err(1, "Underflow", calc); }
            return Ok(CalculatorState(UnwrapOr(result, 0), value, OpNone()));
        }
        if calc::Operation == OpMul() {
            let result = SafeMul(calc::LastOperand, value);
            if IsErr(result) { return Err(1, "Overflow", calc); }
            return Ok(CalculatorState(UnwrapOr(result, 0), value, OpNone()));
        }
        if calc::Operation == OpDiv() {
            let result = SafeDiv(calc::LastOperand, value);
            if IsErr(result) { return Err(1, "Division error", calc); }
            return Ok(CalculatorState(UnwrapOr(result, 0), value, OpNone()));
        }
        return Ok(CalculatorState(value, value, OpNone()));
    }

    // ========================================================================
    // MODULE 30: SAFE GEO
    // ========================================================================

    newtype Coordinate = (Latitude: Double, Longitude: Double);

    function CreateCoordinate(lat: Double, lon: Double) : QuantumResult<Coordinate> {
        if lat < -90.0 or lat > 90.0 {
            return Err(1, "Invalid latitude", Coordinate(0.0, 0.0));
        }
        if lon < -180.0 or lon > 180.0 {
            return Err(2, "Invalid longitude", Coordinate(0.0, 0.0));
        }
        return Ok(Coordinate(lat, lon));
    }

    function HaversineDistance(a: Coordinate, b: Coordinate) : Double {
        let earthRadiusKm = 6371.0;
        let dLat = DegreesToRadians(b::Latitude - a::Latitude);
        let dLon = DegreesToRadians(b::Longitude - a::Longitude);
        let lat1 = DegreesToRadians(a::Latitude);
        let lat2 = DegreesToRadians(b::Latitude);
        let sinDLat = Sin(dLat / 2.0);
        let sinDLon = Sin(dLon / 2.0);
        let h = sinDLat * sinDLat + Cos(lat1) * Cos(lat2) * sinDLon * sinDLon;
        return 2.0 * earthRadiusKm * ArcSin(Sqrt(h));
    }

    function Bearing(from: Coordinate, to: Coordinate) : Double {
        let dLon = DegreesToRadians(to::Longitude - from::Longitude);
        let lat1 = DegreesToRadians(from::Latitude);
        let lat2 = DegreesToRadians(to::Latitude);
        let y = Sin(dLon) * Cos(lat2);
        let x = Cos(lat1) * Sin(lat2) - Sin(lat1) * Cos(lat2) * Cos(dLon);
        return NormalizeDegrees(RadiansToDegrees(ArcTan2(y, x)));
    }

    newtype BoundingBox = (MinLat: Double, MinLon: Double, MaxLat: Double, MaxLon: Double);

    function CreateBoundingBox(minLat: Double, minLon: Double, maxLat: Double, maxLon: Double) : QuantumResult<BoundingBox> {
        if minLat > maxLat or minLon > maxLon {
            return Err(1, "Invalid bounding box", BoundingBox(-90.0, -180.0, 90.0, 180.0));
        }
        return Ok(BoundingBox(minLat, minLon, maxLat, maxLon));
    }

    function ContainsCoordinate(box: BoundingBox, coord: Coordinate) : Bool {
        return coord::Latitude >= box::MinLat and
               coord::Latitude <= box::MaxLat and
               coord::Longitude >= box::MinLon and
               coord::Longitude <= box::MaxLon;
    }

    // ========================================================================
    // MODULE 31: SAFE PROBABILITY
    // ========================================================================

    newtype Probability = (Value: Double);

    function ClampProbability(p: Double) : Double {
        if p < 0.0 { return 0.0; }
        if p > 1.0 { return 1.0; }
        return p;
    }

    function CreateProbability(p: Double) : Probability {
        return Probability(ClampProbability(p));
    }

    function IsValidProbability(p: Double) : Bool {
        return p >= 0.0 and p <= 1.0;
    }

    function AmplitudeToProbability(re: Double, im: Double) : Probability {
        return Probability(ClampProbability(re * re + im * im));
    }

    function CombineProbabilities(p1: Probability, p2: Probability) : Probability {
        return Probability(ClampProbability(p1::Value * p2::Value));
    }

    function ComplementProbability(p: Probability) : Probability {
        return Probability(1.0 - p::Value);
    }

    function UnionProbability(p1: Probability, p2: Probability) : Probability {
        return Probability(ClampProbability(p1::Value + p2::Value - p1::Value * p2::Value));
    }

    // ========================================================================
    // MODULE 32: SAFE CHECKSUM
    // ========================================================================

    function Crc32Polynomial() : Int { return 79764919; }
    function Adler32Modulo() : Int { return 65521; }

    function UpdateAdler32(a: Int, b: Int, byte: Int) : (Int, Int) {
        let newA = (a + byte) % Adler32Modulo();
        let newB = (b + newA) % Adler32Modulo();
        return (newA, newB);
    }

    function FinalizeAdler32(a: Int, b: Int) : Int {
        return (b * 65536) + a;
    }

    function Fnv1aOffset32() : Int { return 2166136261; }
    function Fnv1aPrime32() : Int { return 16777619; }

    function Fnv1a32Update(hash: Int, byte: Int) : Int {
        return ((hash ^^^ byte) * Fnv1aPrime32()) % 4294967296;
    }

    function LuhnCheckDigit(partialNumber: Int[]) : Int {
        mutable sum = 0;
        let len = Length(partialNumber);
        for idx in 0..len-1 {
            mutable digit = partialNumber[len - 1 - idx];
            if idx % 2 == 0 {
                set digit = digit * 2;
                if digit > 9 { set digit = digit - 9; }
            }
            set sum = sum + digit;
        }
        return (10 - (sum % 10)) % 10;
    }

    function ValidateLuhn(number: Int[]) : Bool {
        if Length(number) < 2 { return false; }
        let checkDigit = number[Length(number) - 1];
        let partial = number[0..Length(number)-2];
        return LuhnCheckDigit(partial) == checkDigit;
    }

    // ========================================================================
    // MODULE 33: SAFE TENSOR
    // ========================================================================

    newtype TensorShape = (Dims: Int[]);

    function CreateTensorShape(dims: Int[]) : QuantumResult<TensorShape> {
        for dim in dims {
            if dim <= 0 {
                return Err(1, "Invalid dimension", TensorShape([1]));
            }
        }
        return Ok(TensorShape(dims));
    }

    function TensorElementCount(shape: TensorShape) : Int {
        mutable count = 1;
        for dim in shape::Dims {
            set count = count * dim;
        }
        return count;
    }

    function TensorRank(shape: TensorShape) : Int {
        return Length(shape::Dims);
    }

    function IsValidTensorIndex(shape: TensorShape, indices: Int[]) : Bool {
        if Length(indices) != Length(shape::Dims) { return false; }
        for idx in 0..Length(indices)-1 {
            if indices[idx] < 0 or indices[idx] >= shape::Dims[idx] {
                return false;
            }
        }
        return true;
    }

    function FlattenIndex(shape: TensorShape, indices: Int[]) : QuantumResult<Int> {
        if not IsValidTensorIndex(shape, indices) {
            return Err(1, "Invalid index", 0);
        }
        mutable flat = 0;
        mutable stride = 1;
        let n = Length(indices);
        for i in 0..n-1 {
            let idx = n - 1 - i;
            set flat = flat + indices[idx] * stride;
            set stride = stride * shape::Dims[idx];
        }
        return Ok(flat);
    }

    // ========================================================================
    // MODULE 34: SAFE PASSWORD
    // ========================================================================

    newtype PasswordPolicy = (MinLength: Int, MaxLength: Int, RequireDigit: Bool, RequireSpecial: Bool);

    function CreatePasswordPolicy(minLen: Int, maxLen: Int, digit: Bool, special: Bool) : QuantumResult<PasswordPolicy> {
        if minLen < 1 or maxLen < minLen {
            return Err(1, "Invalid length constraints", PasswordPolicy(8, 128, true, true));
        }
        return Ok(PasswordPolicy(minLen, maxLen, digit, special));
    }

    function ValidatePasswordLength(policy: PasswordPolicy, length: Int) : Bool {
        return length >= policy::MinLength and length <= policy::MaxLength;
    }

    function PasswordEntropy(length: Int, charsetSize: Int) : Double {
        if length <= 0 or charsetSize <= 1 { return 0.0; }
        return IntAsDouble(length) * Log(IntAsDouble(charsetSize)) / Log(2.0);
    }

    function IsStrongPassword(entropy: Double) : Bool {
        return entropy >= 60.0;
    }

    // ========================================================================
    // MODULE 35: SAFE ML
    // ========================================================================

    function Softmax(logits: Double[]) : Double[] {
        let n = Length(logits);
        if n == 0 { return []; }
        mutable maxLogit = logits[0];
        for i in 1..n-1 {
            if logits[i] > maxLogit { set maxLogit = logits[i]; }
        }
        mutable expSum = 0.0;
        mutable exps = [0.0, size = n];
        for i in 0..n-1 {
            set exps w/= i <- E() ^ (logits[i] - maxLogit);
            set expSum = expSum + exps[i];
        }
        mutable result = [0.0, size = n];
        for i in 0..n-1 {
            set result w/= i <- exps[i] / expSum;
        }
        return result;
    }

    function CrossEntropyLoss(predicted: Double[], target: Int) : QuantumResult<Double> {
        if target < 0 or target >= Length(predicted) {
            return Err(1, "Invalid target index", 0.0);
        }
        let p = predicted[target];
        if p <= 0.0 {
            return Err(2, "Log of non-positive", 100.0);
        }
        return Ok(-Log(p));
    }

    function Relu(x: Double) : Double {
        if x < 0.0 { return 0.0; }
        return x;
    }

    function Sigmoid(x: Double) : Double {
        return 1.0 / (1.0 + E() ^ (-x));
    }

    function Tanh(x: Double) : Double {
        let exp2x = E() ^ (2.0 * x);
        return (exp2x - 1.0) / (exp2x + 1.0);
    }

    function LeakyRelu(x: Double, alpha: Double) : Double {
        if x < 0.0 { return alpha * x; }
        return x;
    }

    function Gelu(x: Double) : Double {
        return 0.5 * x * (1.0 + Tanh(Sqrt(2.0 / PI()) * (x + 0.044715 * x * x * x)));
    }

    // ========================================================================
    // MODULE 36: SAFE HEADER
    // ========================================================================

    newtype HttpHeader = (NameLength: Int, ValueLength: Int, MaxNameLength: Int, MaxValueLength: Int);

    function CreateHttpHeader(nameLen: Int, valueLen: Int) : QuantumResult<HttpHeader> {
        let maxName = 256;
        let maxValue = 8192;
        if nameLen < 1 or nameLen > maxName {
            return Err(1, "Invalid header name length", HttpHeader(0, 0, maxName, maxValue));
        }
        if valueLen < 0 or valueLen > maxValue {
            return Err(2, "Invalid header value length", HttpHeader(0, 0, maxName, maxValue));
        }
        return Ok(HttpHeader(nameLen, valueLen, maxName, maxValue));
    }

    function HeaderTotalSize(header: HttpHeader) : Int {
        return header::NameLength + header::ValueLength + 4;
    }

    function ContainsCRLF(containsCarriageReturn: Bool, containsNewline: Bool) : Bool {
        return containsCarriageReturn or containsNewline;
    }

    // ========================================================================
    // MODULE 37: SAFE COOKIE
    // ========================================================================

    newtype Cookie = (NameLength: Int, ValueLength: Int, MaxAge: Int, Secure: Bool, HttpOnly: Bool);

    function CreateCookie(nameLen: Int, valueLen: Int, maxAge: Int, secure: Bool, httpOnly: Bool) : QuantumResult<Cookie> {
        if nameLen < 1 or nameLen > 256 {
            return Err(1, "Invalid cookie name length", Cookie(0, 0, 0, true, true));
        }
        if valueLen < 0 or valueLen > 4096 {
            return Err(2, "Invalid cookie value length", Cookie(0, 0, 0, true, true));
        }
        return Ok(Cookie(nameLen, valueLen, maxAge, secure, httpOnly));
    }

    function IsSecureCookie(cookie: Cookie) : Bool {
        return cookie::Secure and cookie::HttpOnly;
    }

    function IsPersistentCookie(cookie: Cookie) : Bool {
        return cookie::MaxAge > 0;
    }

    function IsSessionCookie(cookie: Cookie) : Bool {
        return cookie::MaxAge <= 0;
    }

    // ========================================================================
    // MODULE 38: SAFE CONTENT TYPE
    // ========================================================================

    newtype ContentType = (TypeCode: Int, SubtypeLength: Int, CharsetLength: Int);

    function TypeText() : Int { return 1; }
    function TypeImage() : Int { return 2; }
    function TypeAudio() : Int { return 3; }
    function TypeVideo() : Int { return 4; }
    function TypeApplication() : Int { return 5; }
    function TypeMultipart() : Int { return 6; }

    function CreateContentType(typeCode: Int, subtypeLen: Int, charsetLen: Int) : QuantumResult<ContentType> {
        if typeCode < 1 or typeCode > 6 {
            return Err(1, "Invalid content type", ContentType(5, 0, 0));
        }
        if subtypeLen < 1 or subtypeLen > 127 {
            return Err(2, "Invalid subtype length", ContentType(5, 0, 0));
        }
        return Ok(ContentType(typeCode, subtypeLen, charsetLen));
    }

    function IsTextContent(ct: ContentType) : Bool {
        return ct::TypeCode == TypeText();
    }

    function IsMediaContent(ct: ContentType) : Bool {
        return ct::TypeCode == TypeImage() or
               ct::TypeCode == TypeAudio() or
               ct::TypeCode == TypeVideo();
    }

    function RequiresSniffingPrevention(ct: ContentType) : Bool {
        return ct::TypeCode == TypeText() or ct::TypeCode == TypeApplication();
    }

    // ========================================================================
    // QUANTUM-SPECIFIC SAFETY PRIMITIVES
    // ========================================================================

    function IsValidQubitIndex(idx: Int, size: Int) : Bool {
        return idx >= 0 and idx < size;
    }

    function ClampQubitIndex(idx: Int, size: Int) : Int {
        if idx < 0 { return 0; }
        if idx >= size { return size - 1; }
        return idx;
    }

    function AreQubitsDistinct(a: Int, b: Int) : Bool {
        return a != b;
    }

    function AreQubitsDistinct3(a: Int, b: Int, c: Int) : Bool {
        return a != b and b != c and a != c;
    }

    newtype BoundedShots = (Value: Int);

    function CreateBoundedShots(n: Int) : BoundedShots {
        let clamped = ClampInt(n, 1, MaxShots());
        return BoundedShots(clamped);
    }

    newtype BoundedDepth = (Value: Int);

    function CreateBoundedDepth(d: Int) : BoundedDepth {
        let clamped = ClampInt(d, 1, MaxDepth());
        return BoundedDepth(clamped);
    }

    newtype Fidelity = (Value: Double);

    function CreateFidelity(f: Double) : Fidelity {
        return Fidelity(ClampProbability(f));
    }

    function IsHighFidelity(f: Fidelity) : Bool {
        return f::Value >= FidelityThreshold();
    }

    newtype ErrorRate = (Value: Double);

    function CreateErrorRate(e: Double) : ErrorRate {
        return ErrorRate(ClampProbability(e));
    }

    function IsLowError(e: ErrorRate) : Bool {
        return e::Value <= ErrorThreshold();
    }

    newtype Concurrence = (Value: Double);

    function CreateConcurrence(c: Double) : Concurrence {
        return Concurrence(ClampProbability(c));
    }

    function IsEntangled(c: Concurrence) : Bool {
        return c::Value > ProbEpsilon();
    }

    // ========================================================================
    // SAFE QUANTUM GATE OPERATIONS
    // ========================================================================

    operation SafeRx(qubit: Qubit, angle: Double) : Unit {
        Rx(WrapAngle(angle), qubit);
    }

    operation SafeRy(qubit: Qubit, angle: Double) : Unit {
        Ry(WrapAngle(angle), qubit);
    }

    operation SafeRz(qubit: Qubit, angle: Double) : Unit {
        Rz(WrapAngle(angle), qubit);
    }

    operation SafeH(qubits: Qubit[], idx: Int) : QuantumResult<Unit> {
        if not IsValidQubitIndex(idx, Length(qubits)) {
            return Err(1, "Invalid qubit index", ());
        }
        H(qubits[idx]);
        return Ok(());
    }

    operation SafeCNOT(qubits: Qubit[], ctrl: Int, tgt: Int) : QuantumResult<Unit> {
        let n = Length(qubits);
        if not IsValidQubitIndex(ctrl, n) {
            return Err(1, "Invalid control index", ());
        }
        if not IsValidQubitIndex(tgt, n) {
            return Err(2, "Invalid target index", ());
        }
        if not AreQubitsDistinct(ctrl, tgt) {
            return Err(3, "Control and target must be distinct", ());
        }
        CNOT(qubits[ctrl], qubits[tgt]);
        return Ok(());
    }

    operation SafeCZ(qubits: Qubit[], a: Int, b: Int) : QuantumResult<Unit> {
        let n = Length(qubits);
        if not IsValidQubitIndex(a, n) {
            return Err(1, "Invalid first qubit index", ());
        }
        if not IsValidQubitIndex(b, n) {
            return Err(2, "Invalid second qubit index", ());
        }
        if not AreQubitsDistinct(a, b) {
            return Err(3, "Qubits must be distinct", ());
        }
        CZ(qubits[a], qubits[b]);
        return Ok(());
    }

    operation SafeSWAP(qubits: Qubit[], a: Int, b: Int) : QuantumResult<Unit> {
        let n = Length(qubits);
        if not IsValidQubitIndex(a, n) {
            return Err(1, "Invalid first qubit index", ());
        }
        if not IsValidQubitIndex(b, n) {
            return Err(2, "Invalid second qubit index", ());
        }
        if not AreQubitsDistinct(a, b) {
            return Err(3, "Qubits must be distinct", ());
        }
        SWAP(qubits[a], qubits[b]);
        return Ok(());
    }

    operation SafeToffoli(qubits: Qubit[], c1: Int, c2: Int, tgt: Int) : QuantumResult<Unit> {
        let n = Length(qubits);
        if not IsValidQubitIndex(c1, n) {
            return Err(1, "Invalid first control index", ());
        }
        if not IsValidQubitIndex(c2, n) {
            return Err(2, "Invalid second control index", ());
        }
        if not IsValidQubitIndex(tgt, n) {
            return Err(3, "Invalid target index", ());
        }
        if not AreQubitsDistinct3(c1, c2, tgt) {
            return Err(4, "All qubits must be distinct", ());
        }
        CCNOT(qubits[c1], qubits[c2], qubits[tgt]);
        return Ok(());
    }

    // ========================================================================
    // UTILITY FUNCTIONS
    // ========================================================================

    function SafePow2(exp: Int) : Int {
        if exp < 0 { return 1; }
        if exp > 62 { return 4611686018427387904; }
        return 1 <<< exp;
    }

    function HilbertDimension(qubits: Int) : Int {
        return SafePow2(qubits);
    }

    function Log2Floor(n: Int) : Int {
        if n <= 0 { return 0; }
        mutable result = 0;
        mutable value = n;
        while value > 1 {
            set value = value / 2;
            set result = result + 1;
        }
        return result;
    }

    function IsPowerOf2(n: Int) : Bool {
        return n > 0 and (n &&& (n - 1)) == 0;
    }

    function NextPowerOf2(n: Int) : Int {
        if n <= 0 { return 1; }
        if IsPowerOf2(n) { return n; }
        return SafePow2(Log2Floor(n) + 1);
    }

    function MinInt(a: Int, b: Int) : Int {
        if a < b { return a; }
        return b;
    }

    function MaxInt(a: Int, b: Int) : Int {
        if a > b { return a; }
        return b;
    }

    function MinDouble(a: Double, b: Double) : Double {
        if a < b { return a; }
        return b;
    }

    function MaxDouble(a: Double, b: Double) : Double {
        if a > b { return a; }
        return b;
    }
}
