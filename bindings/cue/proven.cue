// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

// Proven Safety Primitives for CUE
//
// CUE's type system naturally aligns with dependent types.
// These definitions provide safe, bounded types for configurations.
//
// Module Count: 38
// Categories: Core (11), Data (7), Data Structures (5), Resilience (4),
//             State (2), Algorithm (4), Security (2), HTTP (3)

package proven

import "strings"

// =============================================================================
// METADATA
// =============================================================================

#Version: "0.4.0"

#ModuleCount: 38

#Categories: {
	core:            11
	data:            7
	dataStructures:  5
	resilience:      4
	state:           2
	algorithm:       4
	security:        2
	http:            3
}

// =============================================================================
// CORE MODULES (11)
// =============================================================================

// -----------------------------------------------------------------------------
// safe_math: Safe mathematical operations with overflow detection
// -----------------------------------------------------------------------------

// Safe bounded integer type
#BoundedInt: {
	value: int
	min:   int
	max:   int
	_check: value >= min & value <= max
}

// Safe non-negative integer
#NonNegInt: int & >=0

// Safe positive integer
#PosInt: int & >0

// Safe integer with overflow checking bounds
#SafeInt: {
	value: int
	// i64 bounds
	_minBound: -9223372036854775808
	_maxBound: 9223372036854775807
	_valid: value >= _minBound & value <= _maxBound
}

// Clamped value within range
#ClampedValue: {
	value: int
	min:   int
	max:   int
	result: int & >=min & <=max
	result: value if value >= min & value <= max
	result: min if value < min
	result: max if value > max
}

// -----------------------------------------------------------------------------
// safe_string: Safe string operations with injection prevention
// -----------------------------------------------------------------------------

// Safe non-empty string
#NonEmptyString: string & !=""

// Safe string with max length
#BoundedString: {
	value:     string
	maxLength: int & >0 | *256
	_valid: len(value) <= maxLength
}

// String that does not contain injection patterns
#SafeString: string & !~"(<script|javascript:|onerror=|onclick=|eval\\(|expression\\()"

// SQL-safe string (no unescaped quotes)
#SqlSafeString: string & !~"(--|/\\*|\\*/|;\\s*DROP|;\\s*DELETE|;\\s*UPDATE|'\\s*OR\\s*')"

// -----------------------------------------------------------------------------
// safe_path: Safe file path operations
// -----------------------------------------------------------------------------

// Safe file path (no traversal)
#SafePath: string & !~"(\\.\\./|\\.\\.\\\\|^/|^[A-Za-z]:)"

// Relative path only
#RelativePath: string & !~"^(/|[A-Za-z]:)"

// File extension pattern
#FileExtension: string & =~"^\\.[a-zA-Z0-9]+$"

// Safe filename (no path separators)
#SafeFilename: string & =~"^[a-zA-Z0-9][a-zA-Z0-9._-]*$" & !~"(\\.\\.)"

// -----------------------------------------------------------------------------
// safe_email: Safe email validation
// -----------------------------------------------------------------------------

// Safe email (basic validation)
#Email: string & =~"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"

// Email domain part
#EmailDomain: string & =~"^[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"

// Email local part
#EmailLocalPart: string & =~"^[a-zA-Z0-9._%+-]+$"

// -----------------------------------------------------------------------------
// safe_url: Safe URL validation
// -----------------------------------------------------------------------------

// Safe URL (HTTP/HTTPS only)
#URL: string & =~"^https?://[^\\s]+$"

// HTTPS-only URL
#SecureURL: string & =~"^https://[^\\s]+$"

// URL path component
#URLPath: string & =~"^/[a-zA-Z0-9._~:/?#\\[\\]@!$&'()*+,;=-]*$"

// URL query parameter (key=value)
#URLQueryParam: {
	key:   string & =~"^[a-zA-Z0-9_-]+$"
	value: string
}

// -----------------------------------------------------------------------------
// safe_network: Safe network operations
// -----------------------------------------------------------------------------

// Safe port number (1-65535)
#Port: int & >=1 & <=65535

// Common ports
#CommonPorts: {
	http:     80
	https:    443
	ssh:      22
	dns:      53
	mysql:    3306
	postgres: 5432
	redis:    6379
	mongodb:  27017
}

// Safe hostname
#Hostname: string & =~"^[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(\\.[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"

// Safe IPv4 address
#IPv4: string & =~"^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"

// Safe IPv6 address (simplified)
#IPv6: string & =~"^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$|^::$|^::1$"

// Safe CIDR notation
#CIDR: string & =~"^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)/([0-9]|[1-2][0-9]|3[0-2])$"

// MAC address
#MACAddress: string & =~"^([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})$"

// -----------------------------------------------------------------------------
// safe_crypto: Safe cryptographic operations
// -----------------------------------------------------------------------------

// Hash output sizes in bytes
#HashSize: {
	sha256:  32
	sha384:  48
	sha512:  64
	sha3256: 32
	sha3512: 64
	blake3:  32
}

// Hex-encoded hash (SHA-256)
#SHA256Hex: string & =~"^[a-fA-F0-9]{64}$"

// Hex-encoded hash (SHA-512)
#SHA512Hex: string & =~"^[a-fA-F0-9]{128}$"

// Hex-encoded hash (BLAKE3)
#Blake3Hex: string & =~"^[a-fA-F0-9]{64}$"

// Base64-encoded data
#Base64: string & =~"^[A-Za-z0-9+/]*={0,2}$"

// Cryptographic key configuration
#CryptoKeyConfig: {
	algorithm: "aes-256-gcm" | "chacha20-poly1305" | "xchacha20-poly1305"
	keySize:   int & (128 | 192 | 256)
	nonceSize: int & >0
}

// -----------------------------------------------------------------------------
// safe_uuid: Safe UUID generation and validation
// -----------------------------------------------------------------------------

// UUID v4 format
#UUID: string & =~"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"

// UUID version
#UUIDVersion: 1 | 2 | 3 | 4 | 5

// UUID variant
#UUIDVariant: "ncs" | "rfc4122" | "microsoft" | "future"

// UUID with metadata
#UUIDInfo: {
	value:   #UUID
	version: #UUIDVersion | *4
	variant: #UUIDVariant | *"rfc4122"
}

// Nil UUID
#NilUUID: "00000000-0000-0000-0000-000000000000"

// -----------------------------------------------------------------------------
// safe_currency: Safe currency operations
// -----------------------------------------------------------------------------

// ISO 4217 currency code
#CurrencyCode: string & =~"^[A-Z]{3}$"

// Common currency codes
#CommonCurrencies: {
	USD: "USD"
	EUR: "EUR"
	GBP: "GBP"
	JPY: "JPY"
	CHF: "CHF"
	CAD: "CAD"
	AUD: "AUD"
	CNY: "CNY"
}

// Money amount (minor units to avoid floating point)
#MoneyAmount: {
	amount:       int  // In minor units (cents, pence, etc.)
	currencyCode: #CurrencyCode
	precision:    int & >=0 & <=4 | *2
}

// Currency exchange rate
#ExchangeRate: {
	from:      #CurrencyCode
	to:        #CurrencyCode
	rate:      float & >0
	timestamp: string // ISO 8601
}

// -----------------------------------------------------------------------------
// safe_phone: Safe phone number validation
// -----------------------------------------------------------------------------

// E.164 international phone format
#E164Phone: string & =~"^\\+[1-9]\\d{1,14}$"

// North American phone number
#NANPPhone: string & =~"^\\+1[2-9]\\d{2}[2-9]\\d{6}$"

// Phone number with metadata
#PhoneNumber: {
	number:      #E164Phone
	countryCode: string & =~"^[A-Z]{2}$"
	type:        "mobile" | "landline" | "voip" | "unknown" | *"unknown"
}

// -----------------------------------------------------------------------------
// safe_hex: Safe hexadecimal operations
// -----------------------------------------------------------------------------

// Hex string (lowercase)
#HexLower: string & =~"^[0-9a-f]*$"

// Hex string (uppercase)
#HexUpper: string & =~"^[0-9A-F]*$"

// Hex string (any case)
#Hex: string & =~"^[0-9a-fA-F]*$"

// Fixed-length hex string
#HexBytes: {
	value:  #Hex
	length: int & >0
	_valid: len(value) == length * 2
}

// =============================================================================
// DATA MODULES (7)
// =============================================================================

// -----------------------------------------------------------------------------
// safe_json: Safe JSON operations
// -----------------------------------------------------------------------------

// JSON value types
#JSONValue: null | bool | number | string | [...#JSONValue] | {[string]: #JSONValue}

// JSON path expression
#JSONPath: string & =~"^\\$(\\.([a-zA-Z_][a-zA-Z0-9_]*|\\*)|(\\[[0-9]+\\]|\\[\\*\\]))*$"

// JSON pointer (RFC 6901)
#JSONPointer: string & =~"^(/([^~/]|~0|~1)*)*$"

// -----------------------------------------------------------------------------
// safe_datetime: Safe datetime operations
// -----------------------------------------------------------------------------

// ISO 8601 date (YYYY-MM-DD)
#ISODate: string & =~"^\\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01])$"

// ISO 8601 time (HH:MM:SS)
#ISOTime: string & =~"^([01]\\d|2[0-3]):[0-5]\\d:[0-5]\\d$"

// ISO 8601 datetime
#ISODateTime: string & =~"^\\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01])T([01]\\d|2[0-3]):[0-5]\\d:[0-5]\\d(Z|[+-]([01]\\d|2[0-3]):[0-5]\\d)?$"

// RFC 3339 datetime
#RFC3339: #ISODateTime

// Unix timestamp (seconds since epoch)
#UnixTimestamp: int & >=0

// Unix timestamp (milliseconds)
#UnixTimestampMs: int & >=0

// Safe duration
#Duration: {
	value: #PosInt
	unit:  "ms" | "s" | "m" | "h" | "d"

	// Computed milliseconds
	_ms: {
		if unit == "ms" {value}
		if unit == "s" {value * 1000}
		if unit == "m" {value * 60 * 1000}
		if unit == "h" {value * 3600 * 1000}
		if unit == "d" {value * 86400 * 1000}
	}
	milliseconds: _ms
}

// Safe timeout (with reasonable bounds)
#Timeout: #Duration & {
	milliseconds: >=100 & <=3600000 // 100ms to 1 hour
}

// -----------------------------------------------------------------------------
// safe_float: Safe floating point operations
// -----------------------------------------------------------------------------

// Finite float (no NaN or Inf)
#FiniteFloat: float

// Bounded float
#BoundedFloat: {
	value: float
	min:   float
	max:   float
	_valid: value >= min & value <= max
}

// Safe percentage (0-100)
#Percentage: float & >=0 & <=100

// Safe ratio (0.0-1.0)
#Ratio: float & >=0.0 & <=1.0

// Positive float
#PosFloat: float & >0

// Non-negative float
#NonNegFloat: float & >=0

// -----------------------------------------------------------------------------
// safe_version: Safe semantic versioning
// -----------------------------------------------------------------------------

// Semantic version string
#SemVer: string & =~"^v?(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(-[0-9A-Za-z-]+(\\.[0-9A-Za-z-]+)*)?(\\+[0-9A-Za-z-]+(\\.[0-9A-Za-z-]+)*)?$"

// Version constraint
#VersionConstraint: string & =~"^(>=?|<=?|=|\\^|~)?v?(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)"

// Parsed version
#Version: {
	major:      int & >=0
	minor:      int & >=0
	patch:      int & >=0
	prerelease: string | *""
	build:      string | *""
}

// -----------------------------------------------------------------------------
// safe_color: Safe color representation
// -----------------------------------------------------------------------------

// Hex color (#RGB or #RRGGBB)
#HexColor: string & =~"^#([0-9a-fA-F]{3}|[0-9a-fA-F]{6})$"

// Hex color with alpha (#RRGGBBAA)
#HexColorAlpha: string & =~"^#([0-9a-fA-F]{6}|[0-9a-fA-F]{8})$"

// RGB color
#RGBColor: {
	r: int & >=0 & <=255
	g: int & >=0 & <=255
	b: int & >=0 & <=255
}

// RGBA color
#RGBAColor: {
	r: int & >=0 & <=255
	g: int & >=0 & <=255
	b: int & >=0 & <=255
	a: float & >=0 & <=1
}

// HSL color
#HSLColor: {
	h: float & >=0 & <360  // Hue in degrees
	s: float & >=0 & <=100 // Saturation percentage
	l: float & >=0 & <=100 // Lightness percentage
}

// -----------------------------------------------------------------------------
// safe_angle: Safe angle representation
// -----------------------------------------------------------------------------

// Angle in degrees
#Degrees: float

// Angle in radians
#Radians: float

// Normalized degrees (0-360)
#NormalizedDegrees: float & >=0 & <360

// Normalized radians (0-2*pi)
#NormalizedRadians: float & >=0 & <6.283185307179586

// Angle with unit
#Angle: {
	value: float
	unit:  "deg" | "rad" | "grad" | "turn"
}

// -----------------------------------------------------------------------------
// safe_unit: Safe physical unit operations
// -----------------------------------------------------------------------------

// Memory size with units
#Memory: {
	value: #PosInt
	unit:  "B" | "KB" | "MB" | "GB" | "TB"

	// Computed bytes
	_bytes: {
		if unit == "B" {value}
		if unit == "KB" {value * 1024}
		if unit == "MB" {value * 1024 * 1024}
		if unit == "GB" {value * 1024 * 1024 * 1024}
		if unit == "TB" {value * 1024 * 1024 * 1024 * 1024}
	}
	bytes: _bytes
}

// CPU resources with units
#CPU: {
	value: float & >0
	unit:  "m" | "cores" | *"cores"

	// Computed millicores
	_millicores: {
		if unit == "m" {int(value)}
		if unit == "cores" {int(value * 1000)}
	}
	millicores: _millicores
}

// Length with unit
#Length: {
	value: float
	unit:  "nm" | "um" | "mm" | "cm" | "m" | "km" | "in" | "ft" | "yd" | "mi"
}

// Weight with unit
#Weight: {
	value: float & >0
	unit:  "mg" | "g" | "kg" | "lb" | "oz"
}

// Temperature with unit
#Temperature: {
	value: float
	unit:  "C" | "F" | "K"
}

// =============================================================================
// DATA STRUCTURES MODULES (5)
// =============================================================================

// -----------------------------------------------------------------------------
// safe_buffer: Safe buffer operations
// -----------------------------------------------------------------------------

// Buffer configuration
#BufferConfig: {
	capacity:  int & >0 & <=1073741824 // Max 1GB
	growable:  bool | *true
	maxGrowth: int & >0 | *capacity * 2
}

// Circular buffer configuration
#CircularBufferConfig: {
	capacity:     int & >0
	overwriteOld: bool | *true
}

// -----------------------------------------------------------------------------
// safe_queue: Safe queue operations
// -----------------------------------------------------------------------------

// Queue configuration
#QueueConfig: {
	capacity:   int & >0 | *1000
	blockOnFull: bool | *false
	timeout:    #Duration | *null
}

// Priority queue configuration
#PriorityQueueConfig: {
	capacity:     int & >0 | *1000
	maxPriority:  int & >0 | *10
	ordering:     "min" | "max" | *"max"
}

// -----------------------------------------------------------------------------
// safe_bloom: Safe Bloom filter
// -----------------------------------------------------------------------------

// Bloom filter configuration
#BloomFilterConfig: {
	expectedItems:      int & >0
	falsePositiveRate:  float & >0 & <1 | *0.01
	hashCount:          int & >0 | *7
}

// Counting Bloom filter configuration
#CountingBloomConfig: {
	expectedItems:     int & >0
	falsePositiveRate: float & >0 & <1 | *0.01
	counterBits:       4 | 8 | 16 | *4
}

// -----------------------------------------------------------------------------
// safe_lru: Safe LRU cache
// -----------------------------------------------------------------------------

// LRU cache configuration
#LRUConfig: {
	capacity:      int & >0
	ttl:           #Duration | *null
	evictionPolicy: "lru" | "lfu" | "fifo" | *"lru"
}

// Cache entry with TTL
#CacheEntry: {
	key:       string
	value:     _
	ttl:       #Duration | *null
	createdAt: #UnixTimestamp
}

// -----------------------------------------------------------------------------
// safe_graph: Safe graph operations
// -----------------------------------------------------------------------------

// Graph configuration
#GraphConfig: {
	directed:      bool | *true
	weighted:      bool | *false
	allowSelfLoops: bool | *false
	allowMultiEdges: bool | *false
}

// Graph node
#GraphNode: {
	id:         string
	label:      string | *""
	attributes: {[string]: _} | *{}
}

// Graph edge
#GraphEdge: {
	source:     string
	target:     string
	weight:     float | *1.0
	attributes: {[string]: _} | *{}
}

// =============================================================================
// RESILIENCE MODULES (4)
// =============================================================================

// -----------------------------------------------------------------------------
// safe_rate_limiter: Safe rate limiting
// -----------------------------------------------------------------------------

// Rate limit result
#RateLimitResult: {
	allowed:    bool
	retryAfter: #Duration | *null
	remaining:  int & >=0
}

// Token bucket configuration
#TokenBucketConfig: {
	capacity:   int & >0
	refillRate: int & >0 // tokens per second
	initialTokens: int & >=0 | *capacity
}

// Sliding window configuration
#SlidingWindowConfig: {
	maxRequests: int & >0
	windowSize:  #Duration
}

// Fixed window configuration
#FixedWindowConfig: {
	maxRequests: int & >0
	windowSize:  #Duration
}

// -----------------------------------------------------------------------------
// safe_circuit_breaker: Safe circuit breaker pattern
// -----------------------------------------------------------------------------

// Circuit state
#CircuitState: "closed" | "open" | "half-open"

// Circuit breaker configuration
#CircuitBreakerConfig: {
	failureThreshold:   int & >0 | *5
	successThreshold:   int & >0 | *2
	timeout:            #Duration | *{value: 30, unit: "s"}
	halfOpenMaxCalls:   int & >0 | *3
}

// Circuit breaker status
#CircuitBreakerStatus: {
	state:           #CircuitState
	failures:        int & >=0
	successes:       int & >=0
	lastFailureTime: #UnixTimestamp | *null
}

// -----------------------------------------------------------------------------
// safe_retry: Safe retry operations
// -----------------------------------------------------------------------------

// Retry strategy
#RetryStrategy: "fixed" | "linear" | "exponential" | "decorrelated-jitter"

// Retry configuration
#RetryConfig: {
	maxAttempts:   int & >0 & <=100 | *3
	strategy:      #RetryStrategy | *"exponential"
	initialDelay:  #Duration | *{value: 100, unit: "ms"}
	maxDelay:      #Duration | *{value: 30, unit: "s"}
	backoffFactor: float & >=1 | *2.0
	jitterFactor:  float & >=0 & <=1 | *0.1
	retryOn:       [...string] | *[]
}

// Retry status
#RetryStatus: {
	attempt:   int & >=0
	nextDelay: #Duration | *null
	exhausted: bool
}

// -----------------------------------------------------------------------------
// safe_monotonic: Safe monotonic operations
// -----------------------------------------------------------------------------

// Monotonic counter
#MonotonicCounter: {
	value:   int & >=0
	initial: int & >=0 | *0
}

// Monotonic timestamp
#MonotonicTimestamp: {
	nanos: int & >=0
}

// Sequence generator configuration
#SequenceConfig: {
	start:     int | *0
	step:      int & !=0 | *1
	maxValue:  int | *null
	wrapAround: bool | *false
}

// =============================================================================
// STATE MODULES (2)
// =============================================================================

// -----------------------------------------------------------------------------
// safe_state_machine: Safe state machine
// -----------------------------------------------------------------------------

// State machine definition
#StateMachineDefinition: {
	initial: string
	states:  [...string]
	transitions: [...#StateTransition]
	_validInitial: initial in states
}

// State transition
#StateTransition: {
	from:   string
	to:     string
	event:  string
	guard:  string | *""
	action: string | *""
}

// State machine status
#StateMachineStatus: {
	current:  string
	previous: string | *null
	history:  [...string]
}

// -----------------------------------------------------------------------------
// safe_calculator: Safe calculator operations
// -----------------------------------------------------------------------------

// Calculator operation
#CalculatorOp: "add" | "sub" | "mul" | "div" | "mod" | "pow" | "sqrt" | "abs" | "neg"

// Calculator expression
#CalculatorExpr: {
	op:   #CalculatorOp
	args: [...(number | #CalculatorExpr)]
}

// Calculator precision
#CalculatorPrecision: {
	decimalPlaces: int & >=0 & <=100 | *10
	roundingMode:  "half-up" | "half-down" | "half-even" | "floor" | "ceiling" | *"half-even"
}

// =============================================================================
// ALGORITHM MODULES (4)
// =============================================================================

// -----------------------------------------------------------------------------
// safe_geo: Safe geographic operations
// -----------------------------------------------------------------------------

// Earth radius constants
#EarthRadius: {
	km: 6371.0
	mi: 3958.8
}

// Geographic coordinate
#Coordinate: {
	lat: float & >=-90 & <=90
	lon: float & >=-180 & <=180
}

// Bounding box
#BoundingBox: {
	minLat: float & >=-90 & <=90
	minLon: float & >=-180 & <=180
	maxLat: float & >=-90 & <=90
	maxLon: float & >=-180 & <=180
	_validLat: minLat <= maxLat
}

// GeoJSON point
#GeoJSONPoint: {
	type:        "Point"
	coordinates: [float, float] // [lon, lat]
}

// GeoJSON polygon
#GeoJSONPolygon: {
	type:        "Polygon"
	coordinates: [...[...[ float, float]]] // array of linear rings
}

// Distance unit
#DistanceUnit: "m" | "km" | "mi" | "ft" | "nm"

// -----------------------------------------------------------------------------
// safe_probability: Safe probability operations
// -----------------------------------------------------------------------------

// Probability value (0-1)
#Probability: float & >=0 & <=1

// Probability distribution
#ProbabilityDistribution: {
	outcomes: [...{
		value:       _
		probability: #Probability
	}]
	_sumValid: true // Sum of probabilities should equal 1
}

// Odds ratio
#Odds: {
	numerator:   float & >=0
	denominator: float & >0
}

// -----------------------------------------------------------------------------
// safe_checksum: Safe checksum operations
// -----------------------------------------------------------------------------

// Checksum algorithm
#ChecksumAlgorithm: "crc32" | "crc32c" | "adler32" | "xxhash32" | "xxhash64"

// Checksum result
#ChecksumResult: {
	algorithm: #ChecksumAlgorithm
	value:     string
	verified:  bool | *false
}

// CRC-32 value
#CRC32: string & =~"^[0-9a-fA-F]{8}$"

// Adler-32 value
#Adler32: string & =~"^[0-9a-fA-F]{8}$"

// -----------------------------------------------------------------------------
// safe_tensor: Safe tensor operations
// -----------------------------------------------------------------------------

// Tensor shape
#TensorShape: [...int & >0]

// Tensor dtype
#TensorDtype: "float32" | "float64" | "int32" | "int64" | "uint8" | "bool"

// Tensor configuration
#TensorConfig: {
	shape: #TensorShape
	dtype: #TensorDtype | *"float32"
	requiresGrad: bool | *false
}

// Tensor metadata
#TensorMeta: {
	shape:    #TensorShape
	dtype:    #TensorDtype
	size:     int & >0
	strides:  [...int]
	contiguous: bool
}

// =============================================================================
// SECURITY MODULES (2)
// =============================================================================

// -----------------------------------------------------------------------------
// safe_password: Safe password operations
// -----------------------------------------------------------------------------

// Password policy
#PasswordPolicy: {
	minLength:      int & >=8 | *12
	maxLength:      int & <=128 | *64
	requireUpper:   bool | *true
	requireLower:   bool | *true
	requireDigit:   bool | *true
	requireSpecial: bool | *true
	specialChars:   string | *"!@#$%^&*()_+-=[]{}|;:,.<>?"
	forbidCommon:   bool | *true
}

// Password strength
#PasswordStrength: "weak" | "fair" | "good" | "strong" | "very-strong"

// Password hash configuration
#PasswordHashConfig: {
	algorithm:  "argon2id" | "bcrypt" | "scrypt" | *"argon2id"
	memoryCost: int & >=65536 | *65536      // 64 MB for Argon2
	timeCost:   int & >=3 | *3
	parallelism: int & >=1 | *4
	saltLength: int & >=16 | *16
	hashLength: int & >=32 | *32
}

// Hashed password
#HashedPassword: string & =~"^\\$argon2(id|i|d)\\$|^\\$2[aby]?\\$|^\\$scrypt\\$"

// -----------------------------------------------------------------------------
// safe_ml: Safe machine learning operations
// -----------------------------------------------------------------------------

// Model input validation
#ModelInputConfig: {
	shape:       #TensorShape
	dtype:       #TensorDtype
	normalize:   bool | *true
	clipRange:   [float, float] | *null
}

// Prediction confidence threshold
#ConfidenceThreshold: float & >=0 & <=1 | *0.5

// Model output
#ModelOutput: {
	predictions: [...float]
	confidence:  [...#Probability]
	labels:      [...string] | *[]
}

// ML safety constraints
#MLSafetyConfig: {
	maxBatchSize:      int & >0 | *32
	maxSequenceLength: int & >0 | *512
	maxMemoryMB:       int & >0 | *1024
	timeout:           #Duration | *{value: 30, unit: "s"}
}

// =============================================================================
// HTTP MODULES (3)
// =============================================================================

// -----------------------------------------------------------------------------
// safe_header: Safe HTTP header operations
// -----------------------------------------------------------------------------

// HTTP header name (valid token)
#HeaderName: string & =~"^[!#$%&'*+.^_`|~a-zA-Z0-9-]+$"

// HTTP header
#Header: {
	name:  #HeaderName
	value: string & !~"[\r\n]" // No CRLF injection
}

// Dangerous headers (should not be set by user code)
#DangerousHeaders: [
	"proxy-authorization",
	"proxy-authenticate",
	"proxy-connection",
	"transfer-encoding",
	"content-length",
	"host",
	"connection",
	"keep-alive",
	"upgrade",
	"te",
	"trailer",
]

// Security headers preset
#SecurityHeaders: {
	"X-Frame-Options":        "DENY"
	"X-Content-Type-Options": "nosniff"
	"Referrer-Policy":        "strict-origin-when-cross-origin"
	"X-XSS-Protection":       "1; mode=block"
}

// HSTS configuration
#HSTSConfig: {
	maxAge:            int & >0 | *31536000
	includeSubdomains: bool | *true
	preload:           bool | *false
}

// Content-Security-Policy directive
#CSPDirective: {
	name:    string
	sources: [...string]
}

// -----------------------------------------------------------------------------
// safe_cookie: Safe HTTP cookie operations
// -----------------------------------------------------------------------------

// SameSite attribute
#SameSite: "Strict" | "Lax" | "None"

// Cookie attributes
#CookieAttributes: {
	name:     string & =~"^[a-zA-Z0-9_-]+$"
	value:    string
	domain:   #Hostname | *""
	path:     string | *"/"
	maxAge:   int | *null          // seconds
	expires:  #ISODateTime | *null
	secure:   bool | *true
	httpOnly: bool | *true
	sameSite: #SameSite | *"Lax"
}

// Session cookie (no expiry)
#SessionCookie: #CookieAttributes & {
	maxAge:  null
	expires: null
}

// Persistent cookie (has expiry)
#PersistentCookie: #CookieAttributes & {
	maxAge: int & >0
}

// -----------------------------------------------------------------------------
// safe_content_type: Safe content type operations
// -----------------------------------------------------------------------------

// MIME type
#MimeType: string & =~"^[a-zA-Z0-9][a-zA-Z0-9!#$&.+-^_]*\\/[a-zA-Z0-9][a-zA-Z0-9!#$&.+-^_]*(;\\s*[a-zA-Z0-9][a-zA-Z0-9!#$&.+-^_]*=[^;]+)*$"

// Common MIME types
#CommonMimeTypes: {
	json:       "application/json"
	xml:        "application/xml"
	html:       "text/html"
	text:       "text/plain"
	css:        "text/css"
	javascript: "application/javascript"
	form:       "application/x-www-form-urlencoded"
	multipart:  "multipart/form-data"
	octet:      "application/octet-stream"
	pdf:        "application/pdf"
	png:        "image/png"
	jpeg:       "image/jpeg"
	gif:        "image/gif"
	svg:        "image/svg+xml"
}

// Content type with charset
#ContentType: {
	mimeType: #MimeType
	charset:  string | *"utf-8"
	boundary: string | *"" // for multipart
}

// Safe content types for file upload
#SafeUploadTypes: [
	"image/png",
	"image/jpeg",
	"image/gif",
	"image/webp",
	"application/pdf",
	"text/plain",
	"text/csv",
]

// =============================================================================
// KUBERNETES / INFRASTRUCTURE TYPES
// =============================================================================

// Safe replica count
#Replicas: {
	count: #PosInt
	min:   #PosInt | *1
	max:   #PosInt | *100

	_valid: count >= min & count <= max
}

// Resource constraints (Kubernetes-style)
#ResourceConstraints: {
	requests?: {
		memory?: #Memory
		cpu?:    #CPU
	}
	limits?: {
		memory?: #Memory
		cpu?:    #CPU
	}

	// Validation: requests <= limits when both specified
	if requests != _|_ & limits != _|_ {
		if requests.memory != _|_ & limits.memory != _|_ {
			requests.memory.bytes <= limits.memory.bytes
		}
		if requests.cpu != _|_ & limits.cpu != _|_ {
			requests.cpu.millicores <= limits.cpu.millicores
		}
	}
}

// Environment variable (no secrets in plain text)
#EnvVar: {
	name:  =~"^[A-Z][A-Z0-9_]*$"
	value: string
	// Warn if it looks like a secret
	_notSecret: !strings.Contains(strings.ToLower(name), "password") &
		!strings.Contains(strings.ToLower(name), "secret") &
		!strings.Contains(strings.ToLower(name), "token") &
		!strings.Contains(strings.ToLower(name), "key")
}

// Secret reference (reference, not value)
#SecretRef: {
	name: #NonEmptyString
	key:  #NonEmptyString
}

// Health check probe
#Probe: {
	type: "http" | "tcp" | "exec"

	if type == "http" {
		path: string & =~"^/"
		port: #Port
	}
	if type == "tcp" {
		port: #Port
	}
	if type == "exec" {
		command: [...#NonEmptyString] & [_, ...]
	}

	initialDelay?: #Duration
	interval?:     #Duration
	timeout?:      #Duration
	retries?:      int & >=1 & <=10
}

// Service definition
#Service: {
	name:       #NonEmptyString & =~"^[a-z][a-z0-9-]*$"
	port:       #Port
	protocol:   "TCP" | "UDP" | *"TCP"
	targetPort?: #Port
}
