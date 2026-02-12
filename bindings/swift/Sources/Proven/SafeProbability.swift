// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Probability value clamped to [0, 1].
public struct Probability: Equatable, Comparable, Hashable {
    public let value: Double

    public init(_ value: Double) {
        self.value = max(0, min(1, value))
    }

    public static let zero = Probability(0)
    public static let one = Probability(1)
    public static let half = Probability(0.5)

    public var complement: Probability {
        Probability(1 - value)
    }

    public func and(_ other: Probability) -> Probability {
        Probability(value * other.value)
    }

    public func or(_ other: Probability) -> Probability {
        Probability(value + other.value - value * other.value)
    }

    public static func < (lhs: Probability, rhs: Probability) -> Bool {
        lhs.value < rhs.value
    }

    /// Convert to percentage.
    public var percentage: Double { value * 100 }

    /// Convert to odds.
    public var odds: Double? {
        guard value < 1 else { return nil }
        return value / (1 - value)
    }

    /// Create from odds.
    public static func fromOdds(_ odds: Double) -> Probability? {
        guard odds >= 0 else { return nil }
        return Probability(odds / (1 + odds))
    }
}

/// Probability utilities.
public enum SafeProbability {
    /// Bayes' theorem: P(A|B) = P(B|A) * P(A) / P(B)
    public static func bayes(priorA: Probability, likelihoodBGivenA: Probability, priorB: Probability) -> Probability? {
        guard priorB.value > 0 else { return nil }
        return Probability(likelihoodBGivenA.value * priorA.value / priorB.value)
    }

    /// Total probability: P(B) = Σ P(B|Ai) * P(Ai)
    public static func total(conditionals: [(probability: Probability, givenProbability: Probability)]) -> Probability {
        var sum = 0.0
        for (prob, given) in conditionals {
            sum += prob.value * given.value
        }
        return Probability(sum)
    }

    /// Expected value.
    public static func expectedValue(_ values: [(value: Double, probability: Probability)]) -> Double {
        var sum = 0.0
        for (value, prob) in values {
            sum += value * prob.value
        }
        return sum
    }

    /// Variance.
    public static func variance(_ values: [(value: Double, probability: Probability)]) -> Double {
        let mean = expectedValue(values)
        var sum = 0.0
        for (value, prob) in values {
            let diff = value - mean
            sum += diff * diff * prob.value
        }
        return sum
    }

    /// Standard deviation.
    public static func standardDeviation(_ values: [(value: Double, probability: Probability)]) -> Double {
        sqrt(variance(values))
    }

    /// Binomial probability P(X = k) = C(n,k) * p^k * (1-p)^(n-k)
    public static func binomial(n: Int, k: Int, p: Probability) -> Probability? {
        guard n >= 0 && k >= 0 && k <= n else { return nil }

        switch SafeCalculator.combination(n: n, r: k) {
        case .success(let c):
            let prob = c * pow(p.value, Double(k)) * pow(1 - p.value, Double(n - k))
            return Probability(prob)
        case .failure:
            return nil
        }
    }

    /// Poisson probability P(X = k) = (λ^k * e^-λ) / k!
    public static func poisson(lambda: Double, k: Int) -> Probability? {
        guard lambda >= 0 && k >= 0 else { return nil }

        switch SafeCalculator.factorial(k) {
        case .success(let factorial):
            let prob = pow(lambda, Double(k)) * exp(-lambda) / factorial
            return Probability(prob)
        case .failure:
            return nil
        }
    }

    /// Geometric probability P(X = k) = (1-p)^(k-1) * p
    public static func geometric(p: Probability, k: Int) -> Probability? {
        guard k >= 1 else { return nil }
        return Probability(pow(1 - p.value, Double(k - 1)) * p.value)
    }

    /// Independent events: P(all) = product of probabilities
    public static func allIndependent(_ probabilities: [Probability]) -> Probability {
        var result = 1.0
        for p in probabilities {
            result *= p.value
        }
        return Probability(result)
    }

    /// At least one event: P(at least one) = 1 - P(none)
    public static func atLeastOne(_ probabilities: [Probability]) -> Probability {
        var probNone = 1.0
        for p in probabilities {
            probNone *= (1 - p.value)
        }
        return Probability(1 - probNone)
    }

    /// Exactly n events.
    public static func exactly(_ n: Int, of probabilities: [Probability]) -> Probability {
        let count = probabilities.count
        guard n >= 0 && n <= count else { return .zero }

        // Simple implementation for small arrays
        // For larger arrays, consider using dynamic programming
        func combinations<T>(_ array: [T], choose k: Int) -> [[T]] {
            if k == 0 { return [[]] }
            if array.isEmpty { return [] }

            var result: [[T]] = []
            let rest = Array(array.dropFirst())

            // Include first element
            for combo in combinations(rest, choose: k - 1) {
                result.append([array[0]] + combo)
            }

            // Exclude first element
            result += combinations(rest, choose: k)

            return result
        }

        var totalProb = 0.0
        for indices in combinations(Array(0..<count), choose: n) {
            var prob = 1.0
            for i in 0..<count {
                if indices.contains(i) {
                    prob *= probabilities[i].value
                } else {
                    prob *= (1 - probabilities[i].value)
                }
            }
            totalProb += prob
        }

        return Probability(totalProb)
    }
}
