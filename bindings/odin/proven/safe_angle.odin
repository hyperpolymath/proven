// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:math"

// Constants for angle conversions.
PI :: math.PI
TAU :: 2.0 * PI
DEGREES_PER_RADIAN :: 180.0 / PI
RADIANS_PER_DEGREE :: PI / 180.0

// Angle representation.
Angle :: struct {
    radians: f64,
}

// Create angle from degrees.
angle_from_degrees :: proc(degrees: f64) -> Angle {
    return Angle{radians = degrees * RADIANS_PER_DEGREE}
}

// Create angle from radians.
angle_from_radians :: proc(radians: f64) -> Angle {
    return Angle{radians = radians}
}

// Create angle from turns (1 turn = 360 degrees).
angle_from_turns :: proc(turns: f64) -> Angle {
    return Angle{radians = turns * TAU}
}

// Create angle from gradians (1 grad = 0.9 degrees).
angle_from_gradians :: proc(gradians: f64) -> Angle {
    return Angle{radians = gradians * PI / 200.0}
}

// Get angle in degrees.
to_degrees :: proc(angle: Angle) -> f64 {
    return angle.radians * DEGREES_PER_RADIAN
}

// Get angle in radians.
to_radians :: proc(angle: Angle) -> f64 {
    return angle.radians
}

// Get angle in turns.
to_turns :: proc(angle: Angle) -> f64 {
    return angle.radians / TAU
}

// Get angle in gradians.
to_gradians :: proc(angle: Angle) -> f64 {
    return angle.radians * 200.0 / PI
}

// Normalize angle to [0, 2*PI).
normalize_positive :: proc(angle: Angle) -> Angle {
    r := angle.radians
    for r < 0 {
        r += TAU
    }
    for r >= TAU {
        r -= TAU
    }
    return Angle{radians = r}
}

// Normalize angle to [-PI, PI).
normalize_symmetric :: proc(angle: Angle) -> Angle {
    r := angle.radians
    for r < -PI {
        r += TAU
    }
    for r >= PI {
        r -= TAU
    }
    return Angle{radians = r}
}

// Normalize angle to [0, 360) degrees.
normalize_degrees :: proc(degrees: f64) -> f64 {
    d := degrees
    for d < 0 {
        d += 360.0
    }
    for d >= 360.0 {
        d -= 360.0
    }
    return d
}

// Add two angles.
angle_add :: proc(a, b: Angle) -> Angle {
    return Angle{radians = a.radians + b.radians}
}

// Subtract two angles.
angle_sub :: proc(a, b: Angle) -> Angle {
    return Angle{radians = a.radians - b.radians}
}

// Multiply angle by scalar.
angle_mul :: proc(angle: Angle, scalar: f64) -> Angle {
    return Angle{radians = angle.radians * scalar}
}

// Divide angle by scalar.
angle_div :: proc(angle: Angle, divisor: f64) -> (result: Angle, ok: bool) {
    if float_is_zero(divisor) {
        return {}, false
    }
    return Angle{radians = angle.radians / divisor}, true
}

// Negate an angle.
angle_neg :: proc(angle: Angle) -> Angle {
    return Angle{radians = -angle.radians}
}

// Absolute value of angle.
angle_abs :: proc(angle: Angle) -> Angle {
    return Angle{radians = abs(angle.radians)}
}

// Sine of angle.
angle_sin :: proc(angle: Angle) -> f64 {
    return math.sin(angle.radians)
}

// Cosine of angle.
angle_cos :: proc(angle: Angle) -> f64 {
    return math.cos(angle.radians)
}

// Tangent of angle.
angle_tan :: proc(angle: Angle) -> (result: f64, ok: bool) {
    // Check for values near PI/2 or 3*PI/2 where tan is undefined
    normalized := normalize_positive(angle)
    half_pi := PI / 2.0
    epsilon := 1e-10

    if abs(normalized.radians - half_pi) < epsilon ||
       abs(normalized.radians - 3.0 * half_pi) < epsilon {
        return 0, false
    }

    return math.tan(angle.radians), true
}

// Arc sine (returns angle).
angle_asin :: proc(value: f64) -> (result: Angle, ok: bool) {
    if value < -1.0 || value > 1.0 {
        return {}, false
    }
    return Angle{radians = math.asin(value)}, true
}

// Arc cosine (returns angle).
angle_acos :: proc(value: f64) -> (result: Angle, ok: bool) {
    if value < -1.0 || value > 1.0 {
        return {}, false
    }
    return Angle{radians = math.acos(value)}, true
}

// Arc tangent (returns angle).
angle_atan :: proc(value: f64) -> Angle {
    return Angle{radians = math.atan(value)}
}

// Arc tangent of y/x (returns angle in correct quadrant).
angle_atan2 :: proc(y, x: f64) -> Angle {
    return Angle{radians = math.atan2(y, x)}
}

// Shortest angular distance between two angles.
angular_distance :: proc(a, b: Angle) -> Angle {
    diff := normalize_symmetric(angle_sub(b, a))
    return angle_abs(diff)
}

// Linear interpolation between two angles (taking shortest path).
angle_lerp :: proc(a, b: Angle, t: f64) -> Angle {
    diff := normalize_symmetric(angle_sub(b, a))
    return normalize_positive(angle_add(a, angle_mul(diff, t)))
}

// Check if angle is in range [start, end] (going counterclockwise from start).
angle_in_range :: proc(angle, start, end: Angle) -> bool {
    a := normalize_positive(angle)
    s := normalize_positive(start)
    e := normalize_positive(end)

    if s.radians <= e.radians {
        return a.radians >= s.radians && a.radians <= e.radians
    }
    // Wraps around 0
    return a.radians >= s.radians || a.radians <= e.radians
}

// Compare two angles.
angle_compare :: proc(a, b: Angle) -> int {
    diff := a.radians - b.radians
    if diff < -1e-10 { return -1 }
    if diff > 1e-10 { return 1 }
    return 0
}

// Check if two angles are approximately equal.
angle_approx_equal :: proc(a, b: Angle, epsilon: f64 = 1e-9) -> bool {
    return abs(normalize_symmetric(angle_sub(a, b)).radians) < epsilon
}

// Common angle constants.
ANGLE_ZERO :: Angle{radians = 0}
ANGLE_90 :: Angle{radians = PI / 2}
ANGLE_180 :: Angle{radians = PI}
ANGLE_270 :: Angle{radians = 3 * PI / 2}
ANGLE_360 :: Angle{radians = TAU}

// Cardinal direction from angle.
CardinalDirection :: enum {
    North,
    Northeast,
    East,
    Southeast,
    South,
    Southwest,
    West,
    Northwest,
}

// Get cardinal direction from angle (0 = East, going counterclockwise).
angle_to_cardinal :: proc(angle: Angle) -> CardinalDirection {
    degrees := normalize_degrees(to_degrees(angle))

    if degrees < 22.5 || degrees >= 337.5 { return .East }
    if degrees < 67.5 { return .Northeast }
    if degrees < 112.5 { return .North }
    if degrees < 157.5 { return .Northwest }
    if degrees < 202.5 { return .West }
    if degrees < 247.5 { return .Southwest }
    if degrees < 292.5 { return .South }
    return .Southeast
}
