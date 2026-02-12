// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"math"
)

// Probability represents a probability value clamped to [0, 1].
type Probability struct {
	value float64
}

// NewProbability creates a probability, clamping to [0, 1].
func NewProbability(value float64) Probability {
	if value < 0 {
		value = 0
	} else if value > 1 {
		value = 1
	}
	return Probability{value: value}
}

// TryProbability creates a probability, returning false if out of range.
func TryProbability(value float64) (Probability, bool) {
	if value < 0 || value > 1 {
		return Probability{}, false
	}
	return Probability{value: value}, true
}

// Value returns the probability value.
func (p Probability) Value() float64 {
	return p.value
}

// Complement returns 1 - p.
func (p Probability) Complement() Probability {
	return Probability{value: 1 - p.value}
}

// And computes P(A and B) assuming independence.
func (p Probability) And(other Probability) Probability {
	return Probability{value: p.value * other.value}
}

// Or computes P(A or B) assuming independence.
func (p Probability) Or(other Probability) Probability {
	return NewProbability(p.value + other.value - p.value*other.value)
}

// Given computes P(A|B) = P(A and B) / P(B).
func (p Probability) Given(pBoth, pCondition Probability) (Probability, bool) {
	if pCondition.value == 0 {
		return Probability{}, false
	}
	return TryProbability(pBoth.value / pCondition.value)
}

// IsZero checks if probability is zero.
func (p Probability) IsZero() bool {
	return p.value == 0
}

// IsOne checks if probability is one (certain).
func (p Probability) IsOne() bool {
	return p.value == 1
}

// ToPercentage returns probability as percentage.
func (p Probability) ToPercentage() float64 {
	return p.value * 100
}

// ToOdds returns probability as odds ratio.
func (p Probability) ToOdds() (float64, bool) {
	if p.value == 1 {
		return 0, false // infinite odds
	}
	return p.value / (1 - p.value), true
}

// FromOdds creates probability from odds ratio.
func FromOdds(odds float64) Probability {
	if odds < 0 {
		return Probability{value: 0}
	}
	return Probability{value: odds / (1 + odds)}
}

// Certain is probability 1.
var Certain = Probability{value: 1}

// Impossible is probability 0.
var Impossible = Probability{value: 0}

// Bayes computes Bayes' theorem: P(A|B) = P(B|A) * P(A) / P(B).
func Bayes(pBGivenA, pA, pB Probability) (Probability, bool) {
	if pB.value == 0 {
		return Probability{}, false
	}
	return TryProbability(pBGivenA.value * pA.value / pB.value)
}

// BayesTotal computes posterior using total probability.
// P(A|B) = P(B|A) * P(A) / [P(B|A) * P(A) + P(B|~A) * P(~A)]
func BayesTotal(pBGivenA, pBGivenNotA, pA Probability) (Probability, bool) {
	pNotA := pA.Complement()
	pB := pBGivenA.value*pA.value + pBGivenNotA.value*pNotA.value
	if pB == 0 {
		return Probability{}, false
	}
	return TryProbability(pBGivenA.value * pA.value / pB)
}

// ExpectedValue computes expected value given outcomes and probabilities.
func ExpectedValue(values []float64, probabilities []Probability) (float64, bool) {
	if len(values) != len(probabilities) || len(values) == 0 {
		return 0, false
	}

	var sum float64
	var probSum float64
	for i, v := range values {
		sum += v * probabilities[i].value
		probSum += probabilities[i].value
	}

	// Check probabilities sum to ~1
	if math.Abs(probSum-1) > 0.001 {
		return 0, false
	}

	return sum, true
}

// Variance computes variance given outcomes and probabilities.
func Variance(values []float64, probabilities []Probability) (float64, bool) {
	ev, ok := ExpectedValue(values, probabilities)
	if !ok {
		return 0, false
	}

	var variance float64
	for i, v := range values {
		diff := v - ev
		variance += diff * diff * probabilities[i].value
	}

	return variance, true
}

// StandardDeviation computes standard deviation.
func StandardDeviation(values []float64, probabilities []Probability) (float64, bool) {
	variance, ok := Variance(values, probabilities)
	if !ok {
		return 0, false
	}
	return math.Sqrt(variance), true
}

// BinomialProbability computes P(X = k) for binomial distribution.
func BinomialProbability(n, k int, p Probability) (Probability, bool) {
	if k < 0 || k > n || n < 0 {
		return Probability{}, false
	}

	// C(n,k) * p^k * (1-p)^(n-k)
	coefficient := binomialCoefficient(n, k)
	prob := float64(coefficient) * math.Pow(p.value, float64(k)) * math.Pow(1-p.value, float64(n-k))

	return TryProbability(prob)
}

func binomialCoefficient(n, k int) int64 {
	if k > n-k {
		k = n - k
	}
	var result int64 = 1
	for i := 0; i < k; i++ {
		result = result * int64(n-i) / int64(i+1)
	}
	return result
}

// NormalCDF computes cumulative distribution function for standard normal.
func NormalCDF(z float64) Probability {
	// Approximation using error function
	return NewProbability(0.5 * (1 + erf(z/math.Sqrt2)))
}

func erf(x float64) float64 {
	// Approximation
	sign := 1.0
	if x < 0 {
		sign = -1
		x = -x
	}

	a1 := 0.254829592
	a2 := -0.284496736
	a3 := 1.421413741
	a4 := -1.453152027
	a5 := 1.061405429
	p := 0.3275911

	t := 1.0 / (1.0 + p*x)
	y := 1.0 - (((((a5*t+a4)*t)+a3)*t+a2)*t+a1)*t*math.Exp(-x*x)

	return sign * y
}
