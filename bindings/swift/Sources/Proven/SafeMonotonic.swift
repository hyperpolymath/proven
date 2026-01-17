// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Monotonically increasing counter.
public class MonotonicCounter {
    private var value: Int
    public let max: Int
    private let lock = NSLock()

    public init(initial: Int = 0, max: Int = Int.max) {
        self.value = Swift.max(0, initial)
        self.max = max
    }

    /// Increment by 1.
    public func increment() -> Int? {
        incrementBy(1)
    }

    /// Increment by a specific amount.
    public func incrementBy(_ amount: Int) -> Int? {
        lock.lock()
        defer { lock.unlock() }

        guard amount >= 0 else { return nil }

        let newValue = value + amount
        guard newValue <= max else { return nil }

        value = newValue
        return value
    }

    /// Get current value.
    public func get() -> Int {
        lock.lock()
        defer { lock.unlock() }
        return value
    }

    /// Check if at maximum.
    public func isAtMax() -> Bool {
        lock.lock()
        defer { lock.unlock() }
        return value >= max
    }
}

/// Monotonically increasing timestamp.
public class MonotonicTimestamp {
    private var timestamp: Double
    private let getNow: () -> Double
    private let lock = NSLock()

    public init(initial: Double? = nil, getNow: @escaping () -> Double = { Date().timeIntervalSince1970 * 1000 }) {
        self.getNow = getNow
        self.timestamp = initial ?? getNow()
    }

    /// Update to current time if later.
    public func update() -> Double {
        lock.lock()
        defer { lock.unlock() }

        let now = getNow()
        if now > timestamp {
            timestamp = now
        }
        return timestamp
    }

    /// Try to set a specific timestamp (must be >= current).
    public func trySet(_ newTimestamp: Double) -> Double? {
        lock.lock()
        defer { lock.unlock() }

        guard newTimestamp >= timestamp else { return nil }
        timestamp = newTimestamp
        return timestamp
    }

    /// Get current timestamp.
    public func get() -> Double {
        lock.lock()
        defer { lock.unlock() }
        return timestamp
    }

    /// Get as Date object.
    public func toDate() -> Date {
        Date(timeIntervalSince1970: get() / 1000)
    }

    /// Get as ISO string.
    public func toISOString() -> String {
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return formatter.string(from: toDate())
    }

    /// Get milliseconds since a reference time.
    public func since(_ reference: Double) -> Double {
        get() - reference
    }

    /// Check if this timestamp is after another.
    public func isAfter(_ other: Double) -> Bool {
        get() > other
    }
}

/// Monotonically increasing ID generator (Snowflake-like).
public class MonotonicID {
    private var lastTimestamp: Int64 = 0
    private var sequence: Int = 0
    private let machineId: Int
    private let sequenceBits: Int = 12
    private let machineBits: Int = 10
    private let maxSequence: Int
    private let getNow: () -> Int64
    private let lock = NSLock()

    public init(machineId: Int = 0, getNow: @escaping () -> Int64 = { Int64(Date().timeIntervalSince1970 * 1000) }) {
        self.machineId = machineId & ((1 << machineBits) - 1)
        self.maxSequence = (1 << sequenceBits) - 1
        self.getNow = getNow
    }

    /// Generate the next ID.
    public func next() -> Int64? {
        lock.lock()
        defer { lock.unlock() }

        var timestamp = getNow()

        if timestamp < lastTimestamp {
            return nil // Clock moved backwards
        }

        if timestamp == lastTimestamp {
            sequence = (sequence + 1) & maxSequence
            if sequence == 0 {
                // Wait for next millisecond
                while timestamp <= lastTimestamp {
                    timestamp = getNow()
                }
            }
        } else {
            sequence = 0
        }

        lastTimestamp = timestamp

        // Format: timestamp (42 bits) | machine (10 bits) | sequence (12 bits)
        let id = (timestamp << (machineBits + sequenceBits)) |
                 (Int64(machineId) << sequenceBits) |
                 Int64(sequence)

        return id
    }

    /// Generate next ID as string.
    public func nextString() -> String? {
        guard let id = next() else { return nil }
        return String(id)
    }

    /// Extract timestamp from an ID.
    public func extractTimestamp(_ id: Int64) -> Int64 {
        id >> (machineBits + sequenceBits)
    }

    /// Extract machine ID from an ID.
    public func extractMachineId(_ id: Int64) -> Int {
        Int((id >> sequenceBits) & Int64((1 << machineBits) - 1))
    }

    /// Extract sequence from an ID.
    public func extractSequence(_ id: Int64) -> Int {
        Int(id & Int64(maxSequence))
    }
}

/// Monotonically increasing sequence generator.
public class MonotonicSequence {
    private var current: Int
    public let step: Int
    public let max: Int
    private let lock = NSLock()

    public init(start: Int = 0, step: Int = 1, max: Int = Int.max) {
        self.current = start
        self.step = Swift.max(1, step)
        self.max = max
    }

    /// Get the next value.
    public func next() -> Int? {
        lock.lock()
        defer { lock.unlock() }

        let value = current
        let nextValue = current + step

        guard nextValue <= max else { return nil }

        current = nextValue
        return value
    }

    /// Peek at the next value without consuming.
    public func peek() -> Int {
        lock.lock()
        defer { lock.unlock() }
        return current
    }

    /// Skip n values.
    public func skip(_ n: Int) -> Int? {
        lock.lock()
        defer { lock.unlock() }

        let skipAmount = n * step
        let newValue = current + skipAmount

        guard newValue <= max else { return nil }

        current = newValue
        return current
    }
}

/// Monotonically increasing version.
public struct MonotonicVersion: Equatable, Comparable, Hashable, CustomStringConvertible {
    public let major: Int
    public let minor: Int
    public let patch: Int

    public init(major: Int = 0, minor: Int = 0, patch: Int = 0) {
        self.major = Swift.max(0, major)
        self.minor = Swift.max(0, minor)
        self.patch = Swift.max(0, patch)
    }

    /// Parse a version string.
    public static func parse(_ version: String) -> MonotonicVersion? {
        let components = version.split(separator: ".").map { Int($0) }
        guard components.count == 3,
              let major = components[0],
              let minor = components[1],
              let patch = components[2] else {
            return nil
        }
        return MonotonicVersion(major: major, minor: minor, patch: patch)
    }

    public func bumpMajor() -> MonotonicVersion {
        MonotonicVersion(major: major + 1, minor: 0, patch: 0)
    }

    public func bumpMinor() -> MonotonicVersion {
        MonotonicVersion(major: major, minor: minor + 1, patch: 0)
    }

    public func bumpPatch() -> MonotonicVersion {
        MonotonicVersion(major: major, minor: minor, patch: patch + 1)
    }

    public static func < (lhs: MonotonicVersion, rhs: MonotonicVersion) -> Bool {
        if lhs.major != rhs.major { return lhs.major < rhs.major }
        if lhs.minor != rhs.minor { return lhs.minor < rhs.minor }
        return lhs.patch < rhs.patch
    }

    public var description: String {
        "\(major).\(minor).\(patch)"
    }
}
