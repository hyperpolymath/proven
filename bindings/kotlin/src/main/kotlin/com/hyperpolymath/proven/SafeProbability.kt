// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import kotlin.math.pow
import kotlin.math.sqrt
import kotlin.math.exp

/**
 * Probability value clamped to [0, 1].
 */
@JvmInline
value class Probability private constructor(val value: Double) : Comparable<Probability> {
    init {
        require(value in 0.0..1.0) { "Probability must be between 0 and 1" }
    }

    override fun compareTo(other: Probability): Int = value.compareTo(other.value)

    val complement: Probability get() = Probability(1 - value)

    fun and(other: Probability): Probability = Probability(value * other.value)
    fun or(other: Probability): Probability = Probability(value + other.value - value * other.value)

    val percentage: Double get() = value * 100

    val odds: Double?
        get() = if (value < 1) value / (1 - value) else null

    companion object {
        val ZERO = Probability(0.0)
        val ONE = Probability(1.0)
        val HALF = Probability(0.5)

        fun of(value: Double): Probability = Probability(value.coerceIn(0.0, 1.0))

        fun fromOdds(odds: Double): Probability? {
            if (odds < 0) return null
            return Probability(odds / (1 + odds))
        }
    }
}

/**
 * Probability utilities.
 */
object SafeProbability {
    /**
     * Bayes' theorem: P(A|B) = P(B|A) * P(A) / P(B)
     */
    fun bayes(priorA: Probability, likelihoodBGivenA: Probability, priorB: Probability): Probability? {
        if (priorB.value == 0.0) return null
        val result = likelihoodBGivenA.value * priorA.value / priorB.value
        return Probability.of(result)
    }

    /**
     * Total probability: P(B) = Σ P(B|Ai) * P(Ai)
     */
    fun total(conditionals: List<Pair<Probability, Probability>>): Probability {
        var sum = 0.0
        for ((prob, given) in conditionals) {
            sum += prob.value * given.value
        }
        return Probability.of(sum)
    }

    /**
     * Expected value.
     */
    fun expectedValue(values: List<Pair<Double, Probability>>): Double {
        return values.sumOf { (value, prob) -> value * prob.value }
    }

    /**
     * Variance.
     */
    fun variance(values: List<Pair<Double, Probability>>): Double {
        val mean = expectedValue(values)
        return values.sumOf { (value, prob) ->
            val diff = value - mean
            diff * diff * prob.value
        }
    }

    /**
     * Standard deviation.
     */
    fun standardDeviation(values: List<Pair<Double, Probability>>): Double {
        return sqrt(variance(values))
    }

    /**
     * Binomial probability P(X = k) = C(n,k) * p^k * (1-p)^(n-k)
     */
    fun binomial(n: Int, k: Int, p: Probability): Probability? {
        if (n < 0 || k < 0 || k > n) return null

        val combination = SafeCalculator.combination(n, k).getOrNull() ?: return null
        val prob = combination * p.value.pow(k) * (1 - p.value).pow(n - k)
        return Probability.of(prob)
    }

    /**
     * Poisson probability P(X = k) = (λ^k * e^-λ) / k!
     */
    fun poisson(lambda: Double, k: Int): Probability? {
        if (lambda < 0 || k < 0) return null

        val factorial = SafeCalculator.factorial(k).getOrNull() ?: return null
        val prob = lambda.pow(k) * exp(-lambda) / factorial
        return Probability.of(prob)
    }

    /**
     * Geometric probability P(X = k) = (1-p)^(k-1) * p
     */
    fun geometric(p: Probability, k: Int): Probability? {
        if (k < 1) return null
        return Probability.of((1 - p.value).pow(k - 1) * p.value)
    }

    /**
     * Independent events: P(all) = product of probabilities
     */
    fun allIndependent(probabilities: List<Probability>): Probability {
        var result = 1.0
        for (p in probabilities) {
            result *= p.value
        }
        return Probability.of(result)
    }

    /**
     * At least one event: P(at least one) = 1 - P(none)
     */
    fun atLeastOne(probabilities: List<Probability>): Probability {
        var probNone = 1.0
        for (p in probabilities) {
            probNone *= (1 - p.value)
        }
        return Probability.of(1 - probNone)
    }

    /**
     * Exactly n events.
     */
    fun exactly(n: Int, probabilities: List<Probability>): Probability {
        val count = probabilities.size
        if (n < 0 || n > count) return Probability.ZERO

        fun combinations(indices: List<Int>, k: Int): List<List<Int>> {
            if (k == 0) return listOf(emptyList())
            if (indices.isEmpty()) return emptyList()

            val result = mutableListOf<List<Int>>()
            val rest = indices.drop(1)

            for (combo in combinations(rest, k - 1)) {
                result.add(listOf(indices[0]) + combo)
            }
            result.addAll(combinations(rest, k))

            return result
        }

        var totalProb = 0.0
        for (indices in combinations((0 until count).toList(), n)) {
            var prob = 1.0
            for (i in 0 until count) {
                prob *= if (i in indices) probabilities[i].value else (1 - probabilities[i].value)
            }
            totalProb += prob
        }

        return Probability.of(totalProb)
    }
}
