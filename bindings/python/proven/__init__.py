# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
Proven - Code that cannot crash.

A verified safety library providing:

Core Safety Modules:
- SafeMath: Arithmetic without overflow/underflow/division-by-zero
- SafeString: UTF-8 and escaping without exceptions
- SafeJson: Parsing without JSONDecodeError
- SafeUrl: URL parsing without malformed URL crashes
- SafeEmail: Validation without regex catastrophic backtracking
- SafePath: Filesystem ops without traversal attacks
- SafeCrypto: Cryptographic primitives done right
- SafePassword: Authentication without security holes
- SafeUUID: UUID generation and parsing without exceptions
- SafeCurrency: Money arithmetic without floating-point errors
- SafePhone: Phone number parsing and E.164 formatting
- SafeHex: Hex encoding/decoding without exceptions
- SafeNetwork: IP address and CIDR validation

HTTP Safety Modules (v0.8.0):
- SafeHeader: HTTP header validation, CRLF injection prevention
- SafeCookie: HTTP cookie validation, injection prevention
- SafeContentType: MIME type validation, sniffing prevention

Numerical Computing (v0.9.0):
- SafeFloat: NaN/Infinity prevention, safe division for floats
- SafeTensor: Bounds-checked vector/matrix operations
- SafeML: Numerically stable softmax, loss functions, activations

Extended Types (v1.0.0):
- SafeCalculator: Expression evaluator with overflow/division protection
- SafeDateTime: ISO 8601 parsing, timezone handling
- SafeVersion: Semantic versioning parsing
- SafeGeo: Geographic coordinates, Haversine distance
- SafeAngle: Degree/radian conversions
- SafeChecksum: CRC-32, Adler-32, FNV, Luhn
- SafeProbability: Probability values clamped to [0,1]
- SafeUnit: Physical unit conversions
- SafeColor: RGB/RGBA with WCAG contrast calculations

Data Structures (v1.0.0):
- SafeBuffer: Bounded buffers and ring buffers
- SafeQueue: Bounded FIFO and priority queues
- SafeBloom: Probabilistic set membership
- SafeLru: Least-recently-used cache

Resilience Patterns (v1.0.0):
- SafeRateLimiter: Token bucket, sliding window rate limiting
- SafeCircuitBreaker: Fault tolerance pattern
- SafeRetry: Exponential backoff with jitter

State Management (v1.0.0):
- SafeMonotonic: Monotonically increasing sequences
- SafeStateMachine: Type-safe state transitions
- SafeGraph: Directed graph with cycle detection
"""

# Core imports
from .core import ProvenError, ProvenStatus

# Core safety modules
from .safe_math import SafeMath
from .safe_string import SafeString
from .safe_path import SafePath
from .safe_crypto import SafeCrypto
from .safe_url import SafeUrl
from .safe_email import SafeEmail
from .safe_network import SafeNetwork
from .safe_uuid import SafeUUID, UUIDVersion, UUIDVariant
from .safe_currency import CurrencyCode, Money
from .safe_phone import CountryCode, PhoneNumber, SafePhone
from .safe_hex import SafeHex, HexCase, HexFormat

# Parsing and validation
from .safe_json import SafeJson, JsonValue
from .safe_datetime import SafeDateTime
from .safe_password import SafePassword, PasswordPolicy, PasswordStrength, PasswordValidation

# HTTP modules
from .safe_header import SafeHeader, Header
from .safe_cookie import SafeCookie, Cookie, CookieAttributes, SameSite, CookiePrefix
from .safe_content_type import SafeContentType, ContentType, MediaCategory

# Numerical computing
from .safe_float import SafeFloat
from .safe_tensor import SafeTensor, Vector, Matrix, ShapeError
from .safe_ml import SafeML
from .safe_calculator import SafeCalculator

# Extended types
from .safe_version import SafeVersion, Version
from .safe_geo import SafeGeo, Coordinate, BoundingBox, haversine_distance, bearing, destination
from .safe_angle import SafeAngle, Degrees, Radians, deg_to_rad, rad_to_deg
from .safe_checksum import SafeChecksum, crc32, adler32, fnv1a_64, fnv1a_32, luhn_check
from .safe_probability import SafeProbability, Probability
from .safe_unit import SafeUnit, LengthUnit, MassUnit, TemperatureUnit, TimeUnit, DataUnit
from .safe_unit import convert_length, convert_mass, convert_temperature, convert_time, convert_data
from .safe_color import SafeColor, Rgb, Rgba

# Data structures
from .safe_buffer import BoundedBuffer, RingBuffer
from .safe_queue import BoundedQueue, PriorityQueue
from .safe_bloom import BloomFilter
from .safe_lru import LruCache

# Resilience patterns
from .safe_rate_limiter import TokenBucket, SlidingWindow, FixedWindow, RateLimitResult
from .safe_circuit_breaker import CircuitBreaker, CircuitConfig, CircuitState, CircuitOpenError
from .safe_retry import Retry, RetryConfig, RetryState, exponential_backoff, full_jitter, equal_jitter, with_retry

# State management
from .safe_monotonic import MonotonicCounter, HighWaterMark, SequenceGenerator, EpochGenerator, MonotonicTimestamp
from .safe_state_machine import StateMachine, StateMachineBuilder, InvalidTransitionError
from .safe_graph import Graph, SafeGraph, CycleDetectedError

__version__ = "1.0.0"
__all__ = [
    # Core
    "ProvenError",
    "ProvenStatus",
    # Math & String
    "SafeMath",
    "SafeString",
    # Path & Crypto
    "SafePath",
    "SafeCrypto",
    # URL & Email
    "SafeUrl",
    "SafeEmail",
    # Network
    "SafeNetwork",
    # UUID
    "SafeUUID",
    "UUIDVersion",
    "UUIDVariant",
    # Currency
    "CurrencyCode",
    "Money",
    # Phone
    "CountryCode",
    "PhoneNumber",
    "SafePhone",
    # Hex
    "SafeHex",
    "HexCase",
    "HexFormat",
    # JSON
    "SafeJson",
    "JsonValue",
    # DateTime
    "SafeDateTime",
    # Password
    "SafePassword",
    "PasswordPolicy",
    "PasswordStrength",
    "PasswordValidation",
    # Header
    "SafeHeader",
    "Header",
    # Cookie
    "SafeCookie",
    "Cookie",
    "CookieAttributes",
    "SameSite",
    "CookiePrefix",
    # ContentType
    "SafeContentType",
    "ContentType",
    "MediaCategory",
    # Float
    "SafeFloat",
    # Tensor
    "SafeTensor",
    "Vector",
    "Matrix",
    "ShapeError",
    # ML
    "SafeML",
    # Calculator
    "SafeCalculator",
    # Version
    "SafeVersion",
    "Version",
    # Geo
    "SafeGeo",
    "Coordinate",
    "BoundingBox",
    "haversine_distance",
    "bearing",
    "destination",
    # Angle
    "SafeAngle",
    "Degrees",
    "Radians",
    "deg_to_rad",
    "rad_to_deg",
    # Checksum
    "SafeChecksum",
    "crc32",
    "adler32",
    "fnv1a_64",
    "fnv1a_32",
    "luhn_check",
    # Probability
    "SafeProbability",
    "Probability",
    # Unit
    "SafeUnit",
    "LengthUnit",
    "MassUnit",
    "TemperatureUnit",
    "TimeUnit",
    "DataUnit",
    "convert_length",
    "convert_mass",
    "convert_temperature",
    "convert_time",
    "convert_data",
    # Color
    "SafeColor",
    "Rgb",
    "Rgba",
    # Buffer
    "BoundedBuffer",
    "RingBuffer",
    # Queue
    "BoundedQueue",
    "PriorityQueue",
    # Bloom
    "BloomFilter",
    # LRU
    "LruCache",
    # Rate Limiter
    "TokenBucket",
    "SlidingWindow",
    "FixedWindow",
    "RateLimitResult",
    # Circuit Breaker
    "CircuitBreaker",
    "CircuitConfig",
    "CircuitState",
    "CircuitOpenError",
    # Retry
    "Retry",
    "RetryConfig",
    "RetryState",
    "exponential_backoff",
    "full_jitter",
    "equal_jitter",
    "with_retry",
    # Monotonic
    "MonotonicCounter",
    "HighWaterMark",
    "SequenceGenerator",
    "EpochGenerator",
    "MonotonicTimestamp",
    # State Machine
    "StateMachine",
    "StateMachineBuilder",
    "InvalidTransitionError",
    # Graph
    "Graph",
    "SafeGraph",
    "CycleDetectedError",
]
