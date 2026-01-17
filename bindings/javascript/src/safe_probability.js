// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeProbability - Probability values clamped to [0, 1].
 *
 * Provides safe probability operations with validation.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Probability value clamped to [0, 1].
 */
export class Probability {
  /** @type {number} */
  #value;

  /**
   * Create a probability value.
   *
   * @param {number} value - Value (will be clamped to [0, 1])
   */
  constructor(value) {
    if (!Number.isFinite(value)) {
      this.#value = 0;
    } else {
      this.#value = Math.max(0, Math.min(1, value));
    }
  }

  /**
   * Get the raw value.
   *
   * @returns {number}
   */
  get value() {
    return this.#value;
  }

  /**
   * Create from validated value.
   *
   * @param {number} value - Value
   * @returns {{ ok: true, value: Probability } | { ok: false, error: string }}
   */
  static fromValue(value) {
    if (!Number.isFinite(value)) {
      return err('Value must be a finite number');
    }
    if (value < 0 || value > 1) {
      return err('Value must be between 0 and 1');
    }
    return ok(new Probability(value));
  }

  /**
   * Create from percentage.
   *
   * @param {number} percent - Percentage (0-100)
   * @returns {Probability}
   */
  static fromPercent(percent) {
    return new Probability(percent / 100);
  }

  /**
   * Create from odds.
   *
   * @param {number} odds - Odds (e.g., 3 means 3:1 odds)
   * @returns {Probability}
   */
  static fromOdds(odds) {
    if (odds < 0) {
      return new Probability(0);
    }
    return new Probability(odds / (1 + odds));
  }

  /**
   * Convert to percentage.
   *
   * @returns {number}
   */
  toPercent() {
    return this.#value * 100;
  }

  /**
   * Convert to odds.
   *
   * @returns {number | null} Returns null if probability is 1 (infinite odds)
   */
  toOdds() {
    if (this.#value === 1) {
      return null;
    }
    return this.#value / (1 - this.#value);
  }

  /**
   * Get complement (1 - p).
   *
   * @returns {Probability}
   */
  complement() {
    return new Probability(1 - this.#value);
  }

  /**
   * AND operation (independent events).
   *
   * @param {Probability} other - Other probability
   * @returns {Probability}
   */
  and(other) {
    return new Probability(this.#value * other.value);
  }

  /**
   * OR operation (independent events).
   *
   * @param {Probability} other - Other probability
   * @returns {Probability}
   */
  or(other) {
    return new Probability(this.#value + other.value - this.#value * other.value);
  }

  /**
   * Given (conditional probability).
   * P(A|B) = P(A AND B) / P(B)
   *
   * @param {Probability} prior - P(A AND B)
   * @param {Probability} condition - P(B)
   * @returns {{ ok: true, value: Probability } | { ok: false, error: string }}
   */
  static given(prior, condition) {
    if (condition.value === 0) {
      return err('Condition probability cannot be zero');
    }
    return ok(new Probability(prior.value / condition.value));
  }

  /**
   * Bayes' theorem.
   * P(A|B) = P(B|A) * P(A) / P(B)
   *
   * @param {Probability} likelihoodBGivenA - P(B|A)
   * @param {Probability} priorA - P(A)
   * @param {Probability} evidenceB - P(B)
   * @returns {{ ok: true, value: Probability } | { ok: false, error: string }}
   */
  static bayes(likelihoodBGivenA, priorA, evidenceB) {
    if (evidenceB.value === 0) {
      return err('Evidence probability cannot be zero');
    }
    return ok(new Probability((likelihoodBGivenA.value * priorA.value) / evidenceB.value));
  }

  /**
   * Check if event occurs based on probability.
   *
   * @returns {boolean}
   */
  occurs() {
    return Math.random() < this.#value;
  }

  /**
   * Compare probabilities.
   *
   * @param {Probability} other - Other probability
   * @returns {number} -1 if less, 0 if equal, 1 if greater
   */
  compare(other) {
    if (this.#value < other.value) return -1;
    if (this.#value > other.value) return 1;
    return 0;
  }

  /**
   * Check equality with tolerance.
   *
   * @param {Probability} other - Other probability
   * @param {number} [tolerance=1e-10] - Tolerance
   * @returns {boolean}
   */
  equals(other, tolerance = 1e-10) {
    return Math.abs(this.#value - other.value) <= tolerance;
  }

  /**
   * String representation.
   *
   * @param {number} [decimals=4] - Decimal places
   * @returns {string}
   */
  toString(decimals = 4) {
    return this.#value.toFixed(decimals);
  }

  /**
   * Percentage string representation.
   *
   * @param {number} [decimals=2] - Decimal places
   * @returns {string}
   */
  toPercentString(decimals = 2) {
    return `${this.toPercent().toFixed(decimals)}%`;
  }
}

/**
 * Safe probability operations.
 */
export class SafeProbability {
  /**
   * Create a probability.
   *
   * @param {number} value - Value (clamped to [0, 1])
   * @returns {Probability}
   */
  static create(value) {
    return new Probability(value);
  }

  /**
   * Validate a probability value.
   *
   * @param {number} value - Value to validate
   * @returns {{ ok: true, value: Probability } | { ok: false, error: string }}
   */
  static validate(value) {
    return Probability.fromValue(value);
  }

  /**
   * Calculate combined probability of independent events.
   *
   * @param {Probability[]} probabilities - List of probabilities
   * @returns {Probability}
   */
  static allOccur(probabilities) {
    let product = 1;
    for (const prob of probabilities) {
      product *= prob.value;
    }
    return new Probability(product);
  }

  /**
   * Calculate probability that at least one event occurs.
   *
   * @param {Probability[]} probabilities - List of probabilities
   * @returns {Probability}
   */
  static anyOccurs(probabilities) {
    let complementProduct = 1;
    for (const prob of probabilities) {
      complementProduct *= 1 - prob.value;
    }
    return new Probability(1 - complementProduct);
  }

  /**
   * Calculate expected value.
   *
   * @param {Array<{probability: Probability, value: number}>} outcomes - Outcomes with probabilities
   * @returns {number}
   */
  static expectedValue(outcomes) {
    let sum = 0;
    for (const outcome of outcomes) {
      sum += outcome.probability.value * outcome.value;
    }
    return sum;
  }

  /**
   * Normalize a list of weights to probabilities.
   *
   * @param {number[]} weights - Weights
   * @returns {{ ok: true, value: Probability[] } | { ok: false, error: string }}
   */
  static normalize(weights) {
    if (weights.length === 0) {
      return err('Weights list cannot be empty');
    }

    const sum = weights.reduce((total, weight) => total + weight, 0);
    if (sum === 0) {
      return err('Weights sum cannot be zero');
    }

    return ok(weights.map((weight) => new Probability(weight / sum)));
  }

  /** @type {Probability} Zero probability */
  static ZERO = new Probability(0);

  /** @type {Probability} Certain probability */
  static ONE = new Probability(1);

  /** @type {Probability} 50% probability */
  static HALF = new Probability(0.5);
}
