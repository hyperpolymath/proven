---------------------------- MODULE Proven ----------------------------
(* SPDX-License-Identifier: PMPL-1.0-or-later                                *)
(* Proven - Formally verified safety primitives for TLA+              *)
(*                                                                     *)
(* TLA+ is a formal specification language used by Amazon Web Services *)
(* to verify distributed systems (S3, DynamoDB, EBS). This module     *)
(* provides proven safety primitives as TLA+ operators for use in     *)
(* system specifications and model checking.                           *)
(*                                                                     *)
(* Usage: EXTENDS Proven                                               *)
(**************************************************************************)

EXTENDS Integers, Sequences, FiniteSets, TLC

(**************************************************************************)
(* CONSTANTS - Configurable bounds for model checking                     *)
(**************************************************************************)

CONSTANTS
    INT_MIN,        \* Minimum integer value (e.g., -2147483648)
    INT_MAX,        \* Maximum integer value (e.g., 2147483647)
    PERCENTAGE_MAX, \* Maximum percentage (typically 100)
    BASIS_POINTS_MAX, \* Maximum basis points (typically 10000)
    PORT_MIN,       \* Minimum port number (1)
    PORT_MAX        \* Maximum port number (65535)

(**************************************************************************)
(* RESULT TYPE - Explicit error handling                                  *)
(**************************************************************************)

\* A Result is either Ok(value) or Err(code, message)
\* Represented as a record with discriminator field

Ok(value) == [tag |-> "Ok", value |-> value, error_code |-> 0, error_msg |-> ""]

Err(code, msg) == [tag |-> "Err", value |-> 0, error_code |-> code, error_msg |-> msg]

IsOk(result) == result.tag = "Ok"

IsErr(result) == result.tag = "Err"

UnwrapOr(result, default) == IF IsOk(result) THEN result.value ELSE default

\* Map a function over a Result (functor)
ResultMap(result, f(_)) ==
    IF IsOk(result)
    THEN Ok(f(result.value))
    ELSE result

\* Chain Results (monad bind)
ResultFlatMap(result, f(_)) ==
    IF IsOk(result)
    THEN f(result.value)
    ELSE result

(**************************************************************************)
(* OPTION TYPE - Safe nullable values                                     *)
(**************************************************************************)

Some(value) == [tag |-> "Some", value |-> value]

None == [tag |-> "None", value |-> 0]

IsSome(opt) == opt.tag = "Some"

IsNone(opt) == opt.tag = "None"

OptionUnwrapOr(opt, default) == IF IsSome(opt) THEN opt.value ELSE default

OptionMap(opt, f(_)) ==
    IF IsSome(opt)
    THEN Some(f(opt.value))
    ELSE None

(**************************************************************************)
(* BOUNDED INTEGER OPERATIONS                                             *)
(**************************************************************************)

\* Check if value is within bounds
InBounds(value, min, max) == value >= min /\ value <= max

\* Clamp value to bounds
Clamp(value, min, max) ==
    IF value < min THEN min
    ELSE IF value > max THEN max
    ELSE value

\* Safe addition with overflow detection
SafeAdd(a, b) ==
    LET sum == a + b IN
    IF sum > INT_MAX THEN Err(1, "overflow")
    ELSE IF sum < INT_MIN THEN Err(2, "underflow")
    ELSE Ok(sum)

\* Saturating addition (clamps to bounds)
SaturatingAdd(a, b) == Clamp(a + b, INT_MIN, INT_MAX)

\* Safe subtraction with overflow detection
SafeSub(a, b) ==
    LET diff == a - b IN
    IF diff > INT_MAX THEN Err(1, "overflow")
    ELSE IF diff < INT_MIN THEN Err(2, "underflow")
    ELSE Ok(diff)

\* Saturating subtraction
SaturatingSub(a, b) == Clamp(a - b, INT_MIN, INT_MAX)

\* Safe multiplication with overflow detection
SafeMul(a, b) ==
    LET product == a * b IN
    IF product > INT_MAX THEN Err(1, "overflow")
    ELSE IF product < INT_MIN THEN Err(2, "underflow")
    ELSE Ok(product)

\* Safe division with divide-by-zero protection
SafeDiv(a, b) ==
    IF b = 0 THEN Err(3, "division by zero")
    ELSE Ok(a \div b)

\* Safe modulo with divide-by-zero protection
SafeMod(a, b) ==
    IF b = 0 THEN Err(3, "division by zero")
    ELSE Ok(a % b)

(**************************************************************************)
(* BOUNDED VALUE TYPE                                                     *)
(**************************************************************************)

\* Create a bounded value record
BoundedInt(value, min, max) ==
    [value |-> Clamp(value, min, max), min |-> min, max |-> max]

\* Get value from bounded int
BoundedGet(b) == b.value

\* Set value with automatic clamping
BoundedSet(b, new_value) ==
    [value |-> Clamp(new_value, b.min, b.max), min |-> b.min, max |-> b.max]

\* Add to bounded value
BoundedAdd(b, delta) == BoundedSet(b, b.value + delta)

\* Check if at minimum
BoundedAtMin(b) == b.value = b.min

\* Check if at maximum
BoundedAtMax(b) == b.value = b.max

(**************************************************************************)
(* PERCENTAGE TYPE (0-100)                                                *)
(**************************************************************************)

Percentage(value) == BoundedInt(value, 0, PERCENTAGE_MAX)

PercentageToFraction(p) == p.value / PERCENTAGE_MAX

PercentageFromFraction(f) == Percentage(f * PERCENTAGE_MAX)

\* Invariant: percentage is always in valid range
PercentageInvariant(p) == p.value >= 0 /\ p.value <= PERCENTAGE_MAX

(**************************************************************************)
(* BASIS POINTS TYPE (0-10000)                                            *)
(**************************************************************************)

BasisPoints(value) == BoundedInt(value, 0, BASIS_POINTS_MAX)

BasisPointsToFraction(bp) == bp.value / BASIS_POINTS_MAX

BasisPointsToPercentage(bp) == bp.value / 100

\* Invariant
BasisPointsInvariant(bp) == bp.value >= 0 /\ bp.value <= BASIS_POINTS_MAX

(**************************************************************************)
(* PORT NUMBER TYPE (1-65535)                                             *)
(**************************************************************************)

Port(value) ==
    IF value < PORT_MIN \/ value > PORT_MAX
    THEN None
    ELSE Some([value |-> Clamp(value, PORT_MIN, PORT_MAX)])

IsWellKnownPort(p) == IsSome(p) /\ p.value.value <= 1023

IsRegisteredPort(p) == IsSome(p) /\ p.value.value >= 1024 /\ p.value.value <= 49151

IsDynamicPort(p) == IsSome(p) /\ p.value.value >= 49152

PortInvariant(p) ==
    IsSome(p) => (p.value.value >= PORT_MIN /\ p.value.value <= PORT_MAX)

(**************************************************************************)
(* RESOURCE BAR (for game development / resource management)              *)
(**************************************************************************)

ResourceBar(current, maximum) ==
    LET safe_max == IF maximum < 1 THEN 1 ELSE maximum IN
    [current |-> Clamp(current, 0, safe_max), maximum |-> safe_max]

ResourceGet(rb) == rb.current

ResourceGetMax(rb) == rb.maximum

ResourceAdd(rb, amount) ==
    [current |-> Clamp(rb.current + amount, 0, rb.maximum), maximum |-> rb.maximum]

ResourceIsEmpty(rb) == rb.current = 0

ResourceIsFull(rb) == rb.current = rb.maximum

ResourcePercentage(rb) == (rb.current * 100) \div rb.maximum

\* Invariant
ResourceBarInvariant(rb) ==
    /\ rb.current >= 0
    /\ rb.current <= rb.maximum
    /\ rb.maximum >= 1

(**************************************************************************)
(* VECTOR2 TYPE                                                           *)
(**************************************************************************)

Vec2(x, y) == [x |-> x, y |-> y]

Vec2Add(a, b) == Vec2(a.x + b.x, a.y + b.y)

Vec2Sub(a, b) == Vec2(a.x - b.x, a.y - b.y)

Vec2Neg(v) == Vec2(-v.x, -v.y)

Vec2Scale(v, factor) == Vec2(v.x * factor, v.y * factor)

Vec2Dot(a, b) == a.x * b.x + a.y * b.y

Vec2ManhattanDistance(a, b) ==
    LET dx == IF a.x > b.x THEN a.x - b.x ELSE b.x - a.x IN
    LET dy == IF a.y > b.y THEN a.y - b.y ELSE b.y - a.y IN
    dx + dy

Vec2MagnitudeSquared(v) == v.x * v.x + v.y * v.y

(**************************************************************************)
(* VECTOR3 TYPE                                                           *)
(**************************************************************************)

Vec3(x, y, z) == [x |-> x, y |-> y, z |-> z]

Vec3Add(a, b) == Vec3(a.x + b.x, a.y + b.y, a.z + b.z)

Vec3Sub(a, b) == Vec3(a.x - b.x, a.y - b.y, a.z - b.z)

Vec3Neg(v) == Vec3(-v.x, -v.y, -v.z)

Vec3Scale(v, factor) == Vec3(v.x * factor, v.y * factor, v.z * factor)

Vec3Dot(a, b) == a.x * b.x + a.y * b.y + a.z * b.z

Vec3Cross(a, b) == Vec3(
    a.y * b.z - a.z * b.y,
    a.z * b.x - a.x * b.z,
    a.x * b.y - a.y * b.x
)

Vec3MagnitudeSquared(v) == v.x * v.x + v.y * v.y + v.z * v.z

(**************************************************************************)
(* COLOR TYPE (RGBA, integer 0-255)                                       *)
(**************************************************************************)

Color(r, g, b, a) == [
    r |-> Clamp(r, 0, 255),
    g |-> Clamp(g, 0, 255),
    b |-> Clamp(b, 0, 255),
    a |-> Clamp(a, 0, 255)
]

ColorRGB(r, g, b) == Color(r, g, b, 255)

\* Luminance (integer approximation: 0.299R + 0.587G + 0.114B)
ColorLuminance(c) == (299 * c.r + 587 * c.g + 114 * c.b) \div 1000

ColorGrayscale(c) ==
    LET lum == ColorLuminance(c) IN
    Color(lum, lum, lum, c.a)

ColorInvariant(c) ==
    /\ c.r >= 0 /\ c.r <= 255
    /\ c.g >= 0 /\ c.g <= 255
    /\ c.b >= 0 /\ c.b <= 255
    /\ c.a >= 0 /\ c.a <= 255

(**************************************************************************)
(* STATE MACHINE SAFETY PATTERNS                                          *)
(**************************************************************************)

\* Safe state transition - only allows defined transitions
SafeTransition(current_state, new_state, allowed_transitions) ==
    IF <<current_state, new_state>> \in allowed_transitions
    THEN Ok(new_state)
    ELSE Err(10, "invalid state transition")

\* Bounded counter with automatic wrap
BoundedCounter(value, max) ==
    IF value >= max THEN 0 ELSE value

\* Monotonic counter (never decreases)
MonotonicIncrement(current, increment) ==
    IF increment < 0 THEN current
    ELSE current + increment

(**************************************************************************)
(* SEQUENCE SAFETY OPERATIONS                                             *)
(**************************************************************************)

\* Safe sequence access with bounds checking
SafeSeqGet(seq, index) ==
    IF index < 1 \/ index > Len(seq)
    THEN Err(20, "index out of bounds")
    ELSE Ok(seq[index])

\* Safe sequence head
SafeHead(seq) ==
    IF seq = <<>>
    THEN Err(21, "empty sequence")
    ELSE Ok(Head(seq))

\* Safe sequence tail
SafeTail(seq) ==
    IF seq = <<>>
    THEN Err(21, "empty sequence")
    ELSE Ok(Tail(seq))

\* Bounded sequence append (enforces max length)
BoundedAppend(seq, elem, max_len) ==
    IF Len(seq) >= max_len
    THEN seq  \* Don't grow beyond max
    ELSE Append(seq, elem)

(**************************************************************************)
(* SET SAFETY OPERATIONS                                                  *)
(**************************************************************************)

\* Bounded set insert (enforces max cardinality)
BoundedSetInsert(set, elem, max_size) ==
    IF Cardinality(set) >= max_size /\ elem \notin set
    THEN set  \* Don't grow beyond max
    ELSE set \union {elem}

(**************************************************************************)
(* TEMPORAL SAFETY INVARIANTS                                             *)
(**************************************************************************)

\* These are temporal formulas for use in specifications

\* Value stays bounded forever
AlwaysBounded(value, min, max) == [](value >= min /\ value <= max)

\* Resource never goes negative
NeverNegative(value) == [](value >= 0)

\* Monotonically increasing
MonotonicallyIncreasing(value) == [][value' >= value]_value

\* Eventually reaches target
EventuallyReaches(value, target) == <>(value = target)

\* Bounded liveness: reaches target within n steps (requires fairness)
BoundedLiveness(value, target, steps) ==
    <>(value = target)  \* Simplified; actual bounded liveness needs step counting

(**************************************************************************)
(* COMMON INVARIANT PATTERNS                                              *)
(**************************************************************************)

\* Type invariant for bounded integers
BoundedIntTypeInvariant(b) ==
    /\ b.value \in Int
    /\ b.min \in Int
    /\ b.max \in Int
    /\ b.min <= b.max
    /\ b.value >= b.min
    /\ b.value <= b.max

\* Result type invariant
ResultTypeInvariant(r) ==
    /\ r.tag \in {"Ok", "Err"}
    /\ r.tag = "Ok" => r.error_code = 0
    /\ r.tag = "Err" => r.error_code # 0

\* Option type invariant
OptionTypeInvariant(o) ==
    o.tag \in {"Some", "None"}

=============================================================================
