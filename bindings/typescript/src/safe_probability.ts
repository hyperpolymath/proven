// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * Probability represents a probability value clamped to [0, 1].
 */
export class Probability {
  private readonly value: number;

  private constructor(value: number) {
    this.value = value;
  }

  /**
   * Create a probability, clamping to [0, 1].
   */
  static create(value: number): Probability {
    if (value < 0) return new Probability(0);
    if (value > 1) return new Probability(1);
    return new Probability(value);
  }

  /**
   * Create a probability, returning error if out of range.
   */
  static tryCreate(value: number): Result<Probability> {
    if (value < 0 || value > 1) {
      return { ok: false, error: 'Probability must be between 0 and 1' };
    }
    return { ok: true, value: new Probability(value) };
  }

  /**
   * Probability of 0 (impossible).
   */
  static impossible(): Probability {
    return new Probability(0);
  }

  /**
   * Probability of 1 (certain).
   */
  static certain(): Probability {
    return new Probability(1);
  }

  /**
   * Create from percentage (0-100).
   */
  static fromPercentage(percent: number): Probability {
    return Probability.create(percent / 100);
  }

  /**
   * Create from odds ratio.
   */
  static fromOdds(odds: number): Probability {
    if (odds < 0) return new Probability(0);
    return new Probability(odds / (1 + odds));
  }

  /**
   * Get the underlying value.
   */
  unwrap(): number {
    return this.value;
  }

  /**
   * Convert to percentage.
   */
  toPercentage(): number {
    return this.value * 100;
  }

  /**
   * Convert to odds ratio.
   */
  toOdds(): number | undefined {
    if (this.value === 1) return undefined; // Infinite odds
    return this.value / (1 - this.value);
  }

  /**
   * Complement (1 - p).
   */
  complement(): Probability {
    return new Probability(1 - this.value);
  }

  /**
   * P(A and B) assuming independence.
   */
  and(other: Probability): Probability {
    return new Probability(this.value * other.value);
  }

  /**
   * P(A or B) assuming independence.
   */
  or(other: Probability): Probability {
    return Probability.create(this.value + other.value - this.value * other.value);
  }

  /**
   * P(A | B) = P(A and B) / P(B).
   */
  given(pBoth: Probability, pCondition: Probability): Result<Probability> {
    if (pCondition.value === 0) {
      return { ok: false, error: 'Cannot condition on probability 0' };
    }
    return Probability.tryCreate(pBoth.value / pCondition.value);
  }

  /**
   * Check if impossible (0).
   */
  isImpossible(): boolean {
    return this.value === 0;
  }

  /**
   * Check if certain (1).
   */
  isCertain(): boolean {
    return this.value === 1;
  }

  /**
   * Check if approximately equal.
   */
  approxEquals(other: Probability, epsilon: number = 1e-10): boolean {
    return Math.abs(this.value - other.value) < epsilon;
  }

  toString(): string {
    return `${(this.value * 100).toFixed(2)}%`;
  }
}

/**
 * Bayes' theorem: P(A|B) = P(B|A) * P(A) / P(B).
 */
export function bayes(
  pBGivenA: Probability,
  pA: Probability,
  pB: Probability
): Result<Probability> {
  if (pB.unwrap() === 0) {
    return { ok: false, error: 'P(B) cannot be 0' };
  }
  return Probability.tryCreate((pBGivenA.unwrap() * pA.unwrap()) / pB.unwrap());
}

/**
 * Bayes' theorem using total probability.
 * P(A|B) = P(B|A) * P(A) / [P(B|A) * P(A) + P(B|~A) * P(~A)]
 */
export function bayesTotal(
  pBGivenA: Probability,
  pBGivenNotA: Probability,
  pA: Probability
): Result<Probability> {
  const pNotA = pA.complement();
  const pB = pBGivenA.unwrap() * pA.unwrap() + pBGivenNotA.unwrap() * pNotA.unwrap();

  if (pB === 0) {
    return { ok: false, error: 'Total probability is 0' };
  }

  return Probability.tryCreate((pBGivenA.unwrap() * pA.unwrap()) / pB);
}

/**
 * Calculate expected value.
 */
export function expectedValue(
  values: number[],
  probabilities: Probability[]
): Result<number> {
  if (values.length !== probabilities.length) {
    return { ok: false, error: 'Values and probabilities must have same length' };
  }
  if (values.length === 0) {
    return { ok: false, error: 'Cannot calculate expected value of empty set' };
  }

  // Check probabilities sum to ~1
  const probSum = probabilities.reduce((sum, p) => sum + p.unwrap(), 0);
  if (Math.abs(probSum - 1) > 0.001) {
    return { ok: false, error: 'Probabilities must sum to 1' };
  }

  const ev = values.reduce((sum, v, i) => sum + v * probabilities[i].unwrap(), 0);
  return { ok: true, value: ev };
}

/**
 * Calculate variance.
 */
export function variance(
  values: number[],
  probabilities: Probability[]
): Result<number> {
  const evResult = expectedValue(values, probabilities);
  if (!evResult.ok) return evResult;
  const ev = evResult.value!;

  const varValue = values.reduce(
    (sum, v, i) => sum + Math.pow(v - ev, 2) * probabilities[i].unwrap(),
    0
  );
  return { ok: true, value: varValue };
}

/**
 * Calculate standard deviation.
 */
export function standardDeviation(
  values: number[],
  probabilities: Probability[]
): Result<number> {
  const varianceResult = variance(values, probabilities);
  if (!varianceResult.ok) return varianceResult;
  return { ok: true, value: Math.sqrt(varianceResult.value!) };
}

/**
 * Binomial probability P(X = k) for n trials with probability p.
 */
export function binomialProbability(
  n: number,
  k: number,
  p: Probability
): Result<Probability> {
  if (k < 0 || k > n || n < 0) {
    return { ok: false, error: 'Invalid parameters' };
  }

  const coefficient = binomialCoefficient(n, k);
  const prob =
    coefficient *
    Math.pow(p.unwrap(), k) *
    Math.pow(1 - p.unwrap(), n - k);

  return Probability.tryCreate(prob);
}

/**
 * Calculate binomial coefficient C(n, k).
 */
function binomialCoefficient(n: number, k: number): number {
  if (k > n - k) {
    k = n - k;
  }
  let result = 1;
  for (let i = 0; i < k; i++) {
    result = (result * (n - i)) / (i + 1);
  }
  return result;
}

/**
 * Normal distribution CDF (cumulative distribution function).
 */
export function normalCDF(z: number): Probability {
  // Approximation using error function
  return Probability.create(0.5 * (1 + erf(z / Math.SQRT2)));
}

/**
 * Error function approximation.
 */
function erf(x: number): number {
  const sign = x < 0 ? -1 : 1;
  x = Math.abs(x);

  const a1 = 0.254829592;
  const a2 = -0.284496736;
  const a3 = 1.421413741;
  const a4 = -1.453152027;
  const a5 = 1.061405429;
  const p = 0.3275911;

  const t = 1.0 / (1.0 + p * x);
  const y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * Math.exp(-x * x);

  return sign * y;
}

/**
 * Poisson probability P(X = k) for rate λ.
 */
export function poissonProbability(lambda: number, k: number): Result<Probability> {
  if (lambda <= 0 || k < 0 || !Number.isInteger(k)) {
    return { ok: false, error: 'Invalid parameters' };
  }

  // P(X = k) = (λ^k * e^-λ) / k!
  const prob = (Math.pow(lambda, k) * Math.exp(-lambda)) / factorial(k);
  return Probability.tryCreate(prob);
}

/**
 * Calculate factorial (with reasonable limit).
 */
function factorial(n: number): number {
  if (n <= 1) return 1;
  if (n > 170) return Infinity; // Overflow
  let result = 1;
  for (let i = 2; i <= n; i++) {
    result *= i;
  }
  return result;
}

export const SafeProbability = {
  Probability,
  bayes,
  bayesTotal,
  expectedValue,
  variance,
  standardDeviation,
  binomialProbability,
  normalCDF,
  poissonProbability,
};
