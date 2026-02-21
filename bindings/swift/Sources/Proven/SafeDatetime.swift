// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Safe datetime handling.
public struct SafeDateTime: Equatable, Comparable, Hashable {
    public let timestamp: Double

    public init(_ timestamp: Double) {
        self.timestamp = timestamp
    }

    public init(_ date: Date) {
        self.timestamp = date.timeIntervalSince1970 * 1000
    }

    public static func now() -> SafeDateTime {
        SafeDateTime(Date())
    }

    /// Parse an ISO 8601 date string.
    public static func parse(_ dateString: String) -> SafeDateTime? {
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]

        if let date = formatter.date(from: dateString) {
            return SafeDateTime(date)
        }

        // Try without fractional seconds
        formatter.formatOptions = [.withInternetDateTime]
        if let date = formatter.date(from: dateString) {
            return SafeDateTime(date)
        }

        return nil
    }

    public func toDate() -> Date {
        Date(timeIntervalSince1970: timestamp / 1000)
    }

    public func toISOString() -> String {
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return formatter.string(from: toDate())
    }

    public func add(_ duration: SafeDuration) -> SafeDateTime {
        SafeDateTime(timestamp + duration.milliseconds)
    }

    public func subtract(_ duration: SafeDuration) -> SafeDateTime {
        SafeDateTime(timestamp - duration.milliseconds)
    }

    public func diff(_ other: SafeDateTime) -> SafeDuration {
        SafeDuration(milliseconds: abs(timestamp - other.timestamp))
    }

    public func isBefore(_ other: SafeDateTime) -> Bool {
        timestamp < other.timestamp
    }

    public func isAfter(_ other: SafeDateTime) -> Bool {
        timestamp > other.timestamp
    }

    public static func < (lhs: SafeDateTime, rhs: SafeDateTime) -> Bool {
        lhs.timestamp < rhs.timestamp
    }
}

/// Duration representation.
public struct SafeDuration: Equatable, Comparable, Hashable {
    public let milliseconds: Double

    public init(milliseconds: Double) {
        self.milliseconds = milliseconds
    }

    public static func seconds(_ s: Double) -> SafeDuration {
        SafeDuration(milliseconds: s * 1000)
    }

    public static func minutes(_ m: Double) -> SafeDuration {
        SafeDuration(milliseconds: m * 60 * 1000)
    }

    public static func hours(_ h: Double) -> SafeDuration {
        SafeDuration(milliseconds: h * 60 * 60 * 1000)
    }

    public static func days(_ d: Double) -> SafeDuration {
        SafeDuration(milliseconds: d * 24 * 60 * 60 * 1000)
    }

    public var seconds: Double { milliseconds / 1000 }
    public var minutes: Double { milliseconds / 60000 }
    public var hours: Double { milliseconds / 3600000 }
    public var days: Double { milliseconds / 86400000 }

    public static func < (lhs: SafeDuration, rhs: SafeDuration) -> Bool {
        lhs.milliseconds < rhs.milliseconds
    }

    public static func + (lhs: SafeDuration, rhs: SafeDuration) -> SafeDuration {
        SafeDuration(milliseconds: lhs.milliseconds + rhs.milliseconds)
    }

    public static func - (lhs: SafeDuration, rhs: SafeDuration) -> SafeDuration {
        SafeDuration(milliseconds: lhs.milliseconds - rhs.milliseconds)
    }
}
