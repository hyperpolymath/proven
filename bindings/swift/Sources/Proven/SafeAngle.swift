// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Angle represented in degrees with validation.
public struct Degrees: Equatable, Comparable, Hashable {
    public let value: Double

    public init(_ value: Double) {
        self.value = value
    }

    /// Normalize to [0, 360).
    public var normalized: Degrees {
        var v = value.truncatingRemainder(dividingBy: 360)
        if v < 0 { v += 360 }
        return Degrees(v)
    }

    /// Normalize to [-180, 180).
    public var normalizedSigned: Degrees {
        var v = value.truncatingRemainder(dividingBy: 360)
        if v < 0 { v += 360 }
        if v >= 180 { v -= 360 }
        return Degrees(v)
    }

    /// Convert to radians.
    public var toRadians: Radians {
        Radians(value * .pi / 180)
    }

    /// Add degrees.
    public func adding(_ other: Degrees) -> Degrees {
        Degrees(value + other.value)
    }

    /// Subtract degrees.
    public func subtracting(_ other: Degrees) -> Degrees {
        Degrees(value - other.value)
    }

    /// Multiply by scalar.
    public func multiplied(by scalar: Double) -> Degrees {
        Degrees(value * scalar)
    }

    public static func < (lhs: Degrees, rhs: Degrees) -> Bool {
        lhs.value < rhs.value
    }

    public static func + (lhs: Degrees, rhs: Degrees) -> Degrees {
        lhs.adding(rhs)
    }

    public static func - (lhs: Degrees, rhs: Degrees) -> Degrees {
        lhs.subtracting(rhs)
    }

    public static func * (lhs: Degrees, rhs: Double) -> Degrees {
        lhs.multiplied(by: rhs)
    }
}

/// Angle represented in radians with validation.
public struct Radians: Equatable, Comparable, Hashable {
    public let value: Double

    public init(_ value: Double) {
        self.value = value
    }

    /// Normalize to [0, 2π).
    public var normalized: Radians {
        var v = value.truncatingRemainder(dividingBy: 2 * .pi)
        if v < 0 { v += 2 * .pi }
        return Radians(v)
    }

    /// Normalize to [-π, π).
    public var normalizedSigned: Radians {
        var v = value.truncatingRemainder(dividingBy: 2 * .pi)
        if v < 0 { v += 2 * .pi }
        if v >= .pi { v -= 2 * .pi }
        return Radians(v)
    }

    /// Convert to degrees.
    public var toDegrees: Degrees {
        Degrees(value * 180 / .pi)
    }

    /// Add radians.
    public func adding(_ other: Radians) -> Radians {
        Radians(value + other.value)
    }

    /// Subtract radians.
    public func subtracting(_ other: Radians) -> Radians {
        Radians(value - other.value)
    }

    /// Multiply by scalar.
    public func multiplied(by scalar: Double) -> Radians {
        Radians(value * scalar)
    }

    public static func < (lhs: Radians, rhs: Radians) -> Bool {
        lhs.value < rhs.value
    }

    public static func + (lhs: Radians, rhs: Radians) -> Radians {
        lhs.adding(rhs)
    }

    public static func - (lhs: Radians, rhs: Radians) -> Radians {
        lhs.subtracting(rhs)
    }

    public static func * (lhs: Radians, rhs: Double) -> Radians {
        lhs.multiplied(by: rhs)
    }
}

/// Angle utilities.
public enum SafeAngle {
    /// Common angle constants in degrees.
    public static let zero = Degrees(0)
    public static let right = Degrees(90)
    public static let straight = Degrees(180)
    public static let full = Degrees(360)

    /// Common angle constants in radians.
    public static let zeroRad = Radians(0)
    public static let piOver6 = Radians(.pi / 6)
    public static let piOver4 = Radians(.pi / 4)
    public static let piOver3 = Radians(.pi / 3)
    public static let piOver2 = Radians(.pi / 2)
    public static let pi = Radians(.pi)
    public static let twoPi = Radians(2 * .pi)

    /// Calculate the smallest angle between two angles.
    public static func angleBetween(_ a: Degrees, _ b: Degrees) -> Degrees {
        let diff = abs((a.value - b.value).truncatingRemainder(dividingBy: 360))
        return Degrees(diff > 180 ? 360 - diff : diff)
    }

    /// Linear interpolation between angles (taking shortest path).
    public static func lerp(from: Degrees, to: Degrees, t: Double) -> Degrees {
        let t = max(0, min(1, t))
        var diff = (to.value - from.value).truncatingRemainder(dividingBy: 360)
        if diff > 180 { diff -= 360 }
        if diff < -180 { diff += 360 }
        return Degrees(from.value + diff * t).normalized
    }

    /// Check if angle is in range [start, end] (going clockwise).
    public static func isInRange(_ angle: Degrees, start: Degrees, end: Degrees) -> Bool {
        let a = angle.normalized.value
        let s = start.normalized.value
        let e = end.normalized.value

        if s <= e {
            return a >= s && a <= e
        } else {
            return a >= s || a <= e
        }
    }

    /// Convert compass bearing to standard mathematical angle.
    public static func bearingToAngle(_ bearing: Degrees) -> Degrees {
        Degrees(90 - bearing.value).normalized
    }

    /// Convert standard mathematical angle to compass bearing.
    public static func angleToBearing(_ angle: Degrees) -> Degrees {
        Degrees(90 - angle.value).normalized
    }

    /// Trigonometric functions with degrees.
    public static func sin(_ angle: Degrees) -> Double {
        Foundation.sin(angle.toRadians.value)
    }

    public static func cos(_ angle: Degrees) -> Double {
        Foundation.cos(angle.toRadians.value)
    }

    public static func tan(_ angle: Degrees) -> Double {
        Foundation.tan(angle.toRadians.value)
    }

    /// Inverse trigonometric functions returning degrees.
    public static func asin(_ value: Double) -> Degrees? {
        guard value >= -1 && value <= 1 else { return nil }
        return Radians(Foundation.asin(value)).toDegrees
    }

    public static func acos(_ value: Double) -> Degrees? {
        guard value >= -1 && value <= 1 else { return nil }
        return Radians(Foundation.acos(value)).toDegrees
    }

    public static func atan(_ value: Double) -> Degrees {
        Radians(Foundation.atan(value)).toDegrees
    }

    public static func atan2(y: Double, x: Double) -> Degrees {
        Radians(Foundation.atan2(y, x)).toDegrees
    }
}
