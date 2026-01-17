// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Retry strategies.
public enum RetryStrategy {
    case fixed
    case linear
    case exponential
}

/// Retry configuration.
public struct RetryConfig {
    public let maxAttempts: Int
    public let strategy: RetryStrategy
    public let baseDelay: Double // milliseconds
    public let maxDelay: Double // milliseconds
    public let jitterFactor: Double // 0 to 1
    public let multiplier: Double

    public init(
        maxAttempts: Int = 3,
        strategy: RetryStrategy = .exponential,
        baseDelay: Double = 1000,
        maxDelay: Double = 30000,
        jitterFactor: Double = 0.1,
        multiplier: Double = 2
    ) {
        self.maxAttempts = max(1, maxAttempts)
        self.strategy = strategy
        self.baseDelay = max(0, baseDelay)
        self.maxDelay = max(0, maxDelay)
        self.jitterFactor = max(0, min(1, jitterFactor))
        self.multiplier = max(1, multiplier)
    }
}

/// Retry result.
public struct RetryResult<T> {
    public let success: Bool
    public let value: T?
    public let error: Error?
    public let attempts: Int
    public let totalDelay: Double
}

/// Retry utilities.
public enum SafeRetry {
    /// Calculate delay for a given attempt.
    public static func calculateDelay(config: RetryConfig, attempt: Int) -> Double {
        var delay: Double

        switch config.strategy {
        case .fixed:
            delay = config.baseDelay
        case .linear:
            delay = config.baseDelay * (1 + Double(attempt) * config.multiplier)
        case .exponential:
            delay = config.baseDelay * pow(config.multiplier, Double(attempt))
        }

        // Apply max delay cap
        delay = min(delay, config.maxDelay)

        // Apply jitter
        if config.jitterFactor > 0 {
            let jitter = delay * config.jitterFactor * (Double.random(in: 0...1) * 2 - 1)
            delay = max(0, delay + jitter)
        }

        return delay
    }

    /// Retry an async operation with backoff.
    public static func retryAsync<T>(
        config: RetryConfig,
        operation: () async throws -> T
    ) async -> RetryResult<T> {
        var lastError: Error?
        var totalDelay: Double = 0

        for attempt in 0..<config.maxAttempts {
            do {
                let value = try await operation()
                return RetryResult(
                    success: true,
                    value: value,
                    error: nil,
                    attempts: attempt + 1,
                    totalDelay: totalDelay
                )
            } catch {
                lastError = error

                if attempt < config.maxAttempts - 1 {
                    let delay = calculateDelay(config: config, attempt: attempt)
                    totalDelay += delay
                    try? await Task.sleep(nanoseconds: UInt64(delay * 1_000_000))
                }
            }
        }

        return RetryResult(
            success: false,
            value: nil,
            error: lastError,
            attempts: config.maxAttempts,
            totalDelay: totalDelay
        )
    }

    /// Retry an async operation with selective error handling.
    public static func retryAsyncSelective<T>(
        config: RetryConfig,
        shouldRetry: @escaping (Error) -> Bool,
        operation: () async throws -> T
    ) async -> RetryResult<T> {
        var lastError: Error?
        var totalDelay: Double = 0

        for attempt in 0..<config.maxAttempts {
            do {
                let value = try await operation()
                return RetryResult(
                    success: true,
                    value: value,
                    error: nil,
                    attempts: attempt + 1,
                    totalDelay: totalDelay
                )
            } catch {
                lastError = error

                if !shouldRetry(error) {
                    return RetryResult(
                        success: false,
                        value: nil,
                        error: error,
                        attempts: attempt + 1,
                        totalDelay: totalDelay
                    )
                }

                if attempt < config.maxAttempts - 1 {
                    let delay = calculateDelay(config: config, attempt: attempt)
                    totalDelay += delay
                    try? await Task.sleep(nanoseconds: UInt64(delay * 1_000_000))
                }
            }
        }

        return RetryResult(
            success: false,
            value: nil,
            error: lastError,
            attempts: config.maxAttempts,
            totalDelay: totalDelay
        )
    }

    /// Retry a synchronous operation.
    public static func retry<T>(
        config: RetryConfig,
        operation: () throws -> T
    ) -> RetryResult<T> {
        var lastError: Error?

        for attempt in 0..<config.maxAttempts {
            do {
                let value = try operation()
                return RetryResult(
                    success: true,
                    value: value,
                    error: nil,
                    attempts: attempt + 1,
                    totalDelay: 0
                )
            } catch {
                lastError = error
            }
        }

        return RetryResult(
            success: false,
            value: nil,
            error: lastError,
            attempts: config.maxAttempts,
            totalDelay: 0
        )
    }
}

/// Retry presets.
public extension RetryConfig {
    /// Quick retries for local operations.
    static let quick = RetryConfig(
        maxAttempts: 3,
        strategy: .fixed,
        baseDelay: 100,
        jitterFactor: 0
    )

    /// Standard exponential backoff for network operations.
    static let standard = RetryConfig(
        maxAttempts: 5,
        strategy: .exponential,
        baseDelay: 1000,
        maxDelay: 30000,
        jitterFactor: 0.1,
        multiplier: 2
    )

    /// Aggressive retries for critical operations.
    static let aggressive = RetryConfig(
        maxAttempts: 10,
        strategy: .exponential,
        baseDelay: 500,
        maxDelay: 60000,
        jitterFactor: 0.2,
        multiplier: 2
    )

    /// Gentle retries for rate-limited APIs.
    static let gentle = RetryConfig(
        maxAttempts: 5,
        strategy: .linear,
        baseDelay: 5000,
        maxDelay: 60000,
        jitterFactor: 0.1,
        multiplier: 1
    )
}

/// Retry builder for fluent configuration.
public class RetryBuilder {
    private var config: RetryConfig

    public init() {
        self.config = RetryConfig()
    }

    public func maxAttempts(_ n: Int) -> RetryBuilder {
        config = RetryConfig(
            maxAttempts: n,
            strategy: config.strategy,
            baseDelay: config.baseDelay,
            maxDelay: config.maxDelay,
            jitterFactor: config.jitterFactor,
            multiplier: config.multiplier
        )
        return self
    }

    public func fixed(delay: Double) -> RetryBuilder {
        config = RetryConfig(
            maxAttempts: config.maxAttempts,
            strategy: .fixed,
            baseDelay: delay,
            maxDelay: config.maxDelay,
            jitterFactor: config.jitterFactor,
            multiplier: config.multiplier
        )
        return self
    }

    public func linear(baseDelay: Double, multiplier: Double = 1) -> RetryBuilder {
        config = RetryConfig(
            maxAttempts: config.maxAttempts,
            strategy: .linear,
            baseDelay: baseDelay,
            maxDelay: config.maxDelay,
            jitterFactor: config.jitterFactor,
            multiplier: multiplier
        )
        return self
    }

    public func exponential(baseDelay: Double, multiplier: Double = 2) -> RetryBuilder {
        config = RetryConfig(
            maxAttempts: config.maxAttempts,
            strategy: .exponential,
            baseDelay: baseDelay,
            maxDelay: config.maxDelay,
            jitterFactor: config.jitterFactor,
            multiplier: multiplier
        )
        return self
    }

    public func maxDelay(_ ms: Double) -> RetryBuilder {
        config = RetryConfig(
            maxAttempts: config.maxAttempts,
            strategy: config.strategy,
            baseDelay: config.baseDelay,
            maxDelay: ms,
            jitterFactor: config.jitterFactor,
            multiplier: config.multiplier
        )
        return self
    }

    public func jitter(_ factor: Double) -> RetryBuilder {
        config = RetryConfig(
            maxAttempts: config.maxAttempts,
            strategy: config.strategy,
            baseDelay: config.baseDelay,
            maxDelay: config.maxDelay,
            jitterFactor: factor,
            multiplier: config.multiplier
        )
        return self
    }

    public func build() -> RetryConfig {
        config
    }
}
