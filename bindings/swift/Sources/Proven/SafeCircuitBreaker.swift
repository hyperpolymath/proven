// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Circuit breaker states.
public enum CircuitState: String, Equatable {
    case closed
    case open
    case halfOpen
}

/// Circuit breaker configuration.
public struct CircuitBreakerConfig {
    public let failureThreshold: Int
    public let successThreshold: Int
    public let timeout: Double // milliseconds
    public let halfOpenMaxCalls: Int

    public init(failureThreshold: Int = 5, successThreshold: Int = 2, timeout: Double = 30000, halfOpenMaxCalls: Int = 1) {
        self.failureThreshold = max(1, failureThreshold)
        self.successThreshold = max(1, successThreshold)
        self.timeout = max(0, timeout)
        self.halfOpenMaxCalls = max(1, halfOpenMaxCalls)
    }

    public static let `default` = CircuitBreakerConfig()
}

/// Circuit breaker statistics.
public struct CircuitStats {
    public let state: CircuitState
    public let failures: Int
    public let successes: Int
    public let consecutiveFailures: Int
    public let consecutiveSuccesses: Int
    public let totalCalls: Int
    public let lastFailure: Double?
    public let lastSuccess: Double?
}

/// Circuit breaker pattern implementation.
public class CircuitBreaker {
    private var state: CircuitState = .closed
    private var failures: Int = 0
    private var successes: Int = 0
    private var consecutiveFailures: Int = 0
    private var consecutiveSuccesses: Int = 0
    private var totalCalls: Int = 0
    private var lastFailure: Double?
    private var lastSuccess: Double?
    private var openedAt: Double?
    private var halfOpenCalls: Int = 0

    private let config: CircuitBreakerConfig
    private let getNow: () -> Double
    private let lock = NSLock()

    public init(config: CircuitBreakerConfig = .default, getNow: @escaping () -> Double = { Date().timeIntervalSince1970 * 1000 }) {
        self.config = config
        self.getNow = getNow
    }

    /// Check if the circuit allows calls.
    public func canCall() -> Bool {
        lock.lock()
        defer { lock.unlock() }

        maybeTransitionFromOpen()

        switch state {
        case .closed:
            return true
        case .halfOpen:
            return halfOpenCalls < config.halfOpenMaxCalls
        case .open:
            return false
        }
    }

    /// Record a successful call.
    public func recordSuccess() {
        lock.lock()
        defer { lock.unlock() }

        maybeTransitionFromOpen()
        totalCalls += 1
        successes += 1
        consecutiveSuccesses += 1
        consecutiveFailures = 0
        lastSuccess = getNow()

        if state == .halfOpen && consecutiveSuccesses >= config.successThreshold {
            transitionToClosed()
        }
    }

    /// Record a failed call.
    public func recordFailure() {
        lock.lock()
        defer { lock.unlock() }

        maybeTransitionFromOpen()
        totalCalls += 1
        failures += 1
        consecutiveFailures += 1
        consecutiveSuccesses = 0
        lastFailure = getNow()

        switch state {
        case .halfOpen:
            transitionToOpen()
        case .closed:
            if consecutiveFailures >= config.failureThreshold {
                transitionToOpen()
            }
        case .open:
            break
        }
    }

    /// Execute a function through the circuit breaker.
    public func call<T>(_ fn: () throws -> T) -> Result<T, Error> {
        guard canCall() else {
            return .failure(CircuitBreakerError.circuitOpen)
        }

        lock.lock()
        if state == .halfOpen {
            halfOpenCalls += 1
        }
        lock.unlock()

        do {
            let result = try fn()
            recordSuccess()
            return .success(result)
        } catch {
            recordFailure()
            return .failure(error)
        }
    }

    /// Execute an async function through the circuit breaker.
    public func callAsync<T>(_ fn: () async throws -> T) async -> Result<T, Error> {
        guard canCall() else {
            return .failure(CircuitBreakerError.circuitOpen)
        }

        lock.lock()
        if state == .halfOpen {
            halfOpenCalls += 1
        }
        lock.unlock()

        do {
            let result = try await fn()
            recordSuccess()
            return .success(result)
        } catch {
            recordFailure()
            return .failure(error)
        }
    }

    /// Get current state.
    public func getState() -> CircuitState {
        lock.lock()
        defer { lock.unlock() }
        maybeTransitionFromOpen()
        return state
    }

    /// Get statistics.
    public func getStats() -> CircuitStats {
        lock.lock()
        defer { lock.unlock() }
        maybeTransitionFromOpen()
        return CircuitStats(
            state: state,
            failures: failures,
            successes: successes,
            consecutiveFailures: consecutiveFailures,
            consecutiveSuccesses: consecutiveSuccesses,
            totalCalls: totalCalls,
            lastFailure: lastFailure,
            lastSuccess: lastSuccess
        )
    }

    /// Reset the circuit breaker.
    public func reset() {
        lock.lock()
        defer { lock.unlock() }

        state = .closed
        failures = 0
        successes = 0
        consecutiveFailures = 0
        consecutiveSuccesses = 0
        totalCalls = 0
        lastFailure = nil
        lastSuccess = nil
        openedAt = nil
        halfOpenCalls = 0
    }

    /// Force the circuit open.
    public func forceOpen() {
        lock.lock()
        defer { lock.unlock() }
        transitionToOpen()
    }

    /// Force the circuit closed.
    public func forceClosed() {
        lock.lock()
        defer { lock.unlock() }
        transitionToClosed()
    }

    private func maybeTransitionFromOpen() {
        if state == .open, let openedAt = openedAt {
            let elapsed = getNow() - openedAt
            if elapsed >= config.timeout {
                transitionToHalfOpen()
            }
        }
    }

    private func transitionToOpen() {
        state = .open
        openedAt = getNow()
        halfOpenCalls = 0
    }

    private func transitionToHalfOpen() {
        state = .halfOpen
        consecutiveSuccesses = 0
        halfOpenCalls = 0
    }

    private func transitionToClosed() {
        state = .closed
        consecutiveFailures = 0
        openedAt = nil
        halfOpenCalls = 0
    }
}

/// Circuit breaker error.
public enum CircuitBreakerError: Error {
    case circuitOpen
}

/// Circuit breaker group for managing multiple breakers.
public class CircuitBreakerGroup {
    private var breakers: [String: CircuitBreaker] = [:]
    private let config: CircuitBreakerConfig
    private let getNow: () -> Double
    private let lock = NSLock()

    public init(config: CircuitBreakerConfig = .default, getNow: @escaping () -> Double = { Date().timeIntervalSince1970 * 1000 }) {
        self.config = config
        self.getNow = getNow
    }

    /// Get or create a circuit breaker for a key.
    public func get(_ key: String) -> CircuitBreaker {
        lock.lock()
        defer { lock.unlock() }

        if let breaker = breakers[key] {
            return breaker
        }

        let breaker = CircuitBreaker(config: config, getNow: getNow)
        breakers[key] = breaker
        return breaker
    }

    /// Check if a key allows calls.
    public func canCall(_ key: String) -> Bool {
        get(key).canCall()
    }

    /// Record success for a key.
    public func recordSuccess(_ key: String) {
        get(key).recordSuccess()
    }

    /// Record failure for a key.
    public func recordFailure(_ key: String) {
        get(key).recordFailure()
    }

    /// Reset all breakers.
    public func resetAll() {
        lock.lock()
        defer { lock.unlock() }

        for breaker in breakers.values {
            breaker.reset()
        }
    }

    /// Remove a breaker.
    @discardableResult
    public func remove(_ key: String) -> Bool {
        lock.lock()
        defer { lock.unlock() }
        return breakers.removeValue(forKey: key) != nil
    }

    public var count: Int {
        lock.lock()
        defer { lock.unlock() }
        return breakers.count
    }
}
