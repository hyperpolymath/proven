// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"math"
)

// Softmax computes numerically stable softmax.
func Softmax(values []float64) []float64 {
	if len(values) == 0 {
		return []float64{}
	}

	// Find max for numerical stability
	maxVal := values[0]
	for _, v := range values[1:] {
		if v > maxVal {
			maxVal = v
		}
	}

	// Compute exp(x - max) and sum
	exps := make([]float64, len(values))
	var sum float64
	for i, v := range values {
		exps[i] = math.Exp(v - maxVal)
		sum += exps[i]
	}

	// Normalize
	result := make([]float64, len(values))
	for i, e := range exps {
		result[i] = e / sum
	}

	return result
}

// LogSoftmax computes numerically stable log softmax.
func LogSoftmax(values []float64) []float64 {
	if len(values) == 0 {
		return []float64{}
	}

	// Find max for numerical stability
	maxVal := values[0]
	for _, v := range values[1:] {
		if v > maxVal {
			maxVal = v
		}
	}

	// Compute log-sum-exp
	var sumExp float64
	for _, v := range values {
		sumExp += math.Exp(v - maxVal)
	}
	logSumExp := maxVal + math.Log(sumExp)

	// Compute log softmax
	result := make([]float64, len(values))
	for i, v := range values {
		result[i] = v - logSumExp
	}

	return result
}

// Sigmoid computes sigmoid activation.
func Sigmoid(x float64) float64 {
	if x >= 0 {
		return 1.0 / (1.0 + math.Exp(-x))
	}
	expX := math.Exp(x)
	return expX / (1.0 + expX)
}

// SigmoidVec applies sigmoid to vector.
func SigmoidVec(values []float64) []float64 {
	result := make([]float64, len(values))
	for i, v := range values {
		result[i] = Sigmoid(v)
	}
	return result
}

// Tanh computes tanh activation.
func Tanh(x float64) float64 {
	return math.Tanh(x)
}

// TanhVec applies tanh to vector.
func TanhVec(values []float64) []float64 {
	result := make([]float64, len(values))
	for i, v := range values {
		result[i] = math.Tanh(v)
	}
	return result
}

// ReLU computes ReLU activation.
func ReLU(x float64) float64 {
	if x > 0 {
		return x
	}
	return 0
}

// ReLUVec applies ReLU to vector.
func ReLUVec(values []float64) []float64 {
	result := make([]float64, len(values))
	for i, v := range values {
		result[i] = ReLU(v)
	}
	return result
}

// LeakyReLU computes leaky ReLU activation.
func LeakyReLU(x, alpha float64) float64 {
	if x > 0 {
		return x
	}
	return alpha * x
}

// LeakyReLUVec applies leaky ReLU to vector.
func LeakyReLUVec(values []float64, alpha float64) []float64 {
	result := make([]float64, len(values))
	for i, v := range values {
		result[i] = LeakyReLU(v, alpha)
	}
	return result
}

// GELU computes GELU activation (approximation).
func GELU(x float64) float64 {
	return 0.5 * x * (1.0 + math.Tanh(math.Sqrt(2.0/math.Pi)*(x+0.044715*x*x*x)))
}

// GELUVec applies GELU to vector.
func GELUVec(values []float64) []float64 {
	result := make([]float64, len(values))
	for i, v := range values {
		result[i] = GELU(v)
	}
	return result
}

// CrossEntropyLoss computes cross-entropy loss.
func CrossEntropyLoss(predicted, target []float64) (float64, bool) {
	if len(predicted) != len(target) || len(predicted) == 0 {
		return 0, false
	}

	var loss float64
	for i := range predicted {
		if target[i] > 0 {
			// Clamp predicted to avoid log(0)
			p := math.Max(predicted[i], 1e-15)
			loss -= target[i] * math.Log(p)
		}
	}

	return loss, true
}

// MSELoss computes mean squared error loss.
func MSELoss(predicted, target []float64) (float64, bool) {
	if len(predicted) != len(target) || len(predicted) == 0 {
		return 0, false
	}

	var sum float64
	for i := range predicted {
		diff := predicted[i] - target[i]
		sum += diff * diff
	}

	return sum / float64(len(predicted)), true
}

// MAELoss computes mean absolute error loss.
func MAELoss(predicted, target []float64) (float64, bool) {
	if len(predicted) != len(target) || len(predicted) == 0 {
		return 0, false
	}

	var sum float64
	for i := range predicted {
		sum += math.Abs(predicted[i] - target[i])
	}

	return sum / float64(len(predicted)), true
}

// BinaryCrossEntropyLoss computes binary cross-entropy.
func BinaryCrossEntropyLoss(predicted, target float64) float64 {
	// Clamp to avoid log(0)
	p := math.Max(math.Min(predicted, 1-1e-15), 1e-15)
	return -(target*math.Log(p) + (1-target)*math.Log(1-p))
}

// L2Regularization computes L2 penalty.
func L2Regularization(weights []float64, lambda float64) float64 {
	var sum float64
	for _, w := range weights {
		sum += w * w
	}
	return lambda * sum / 2.0
}

// L1Regularization computes L1 penalty.
func L1Regularization(weights []float64, lambda float64) float64 {
	var sum float64
	for _, w := range weights {
		sum += math.Abs(w)
	}
	return lambda * sum
}

// Normalize normalizes values to [0, 1] range.
func Normalize(values []float64) []float64 {
	if len(values) == 0 {
		return []float64{}
	}

	minVal, maxVal := values[0], values[0]
	for _, v := range values[1:] {
		if v < minVal {
			minVal = v
		}
		if v > maxVal {
			maxVal = v
		}
	}

	rangeVal := maxVal - minVal
	if rangeVal == 0 {
		result := make([]float64, len(values))
		for i := range result {
			result[i] = 0.5
		}
		return result
	}

	result := make([]float64, len(values))
	for i, v := range values {
		result[i] = (v - minVal) / rangeVal
	}
	return result
}

// Standardize standardizes values (zero mean, unit variance).
func Standardize(values []float64) []float64 {
	if len(values) == 0 {
		return []float64{}
	}

	// Calculate mean
	var sum float64
	for _, v := range values {
		sum += v
	}
	mean := sum / float64(len(values))

	// Calculate variance
	var variance float64
	for _, v := range values {
		diff := v - mean
		variance += diff * diff
	}
	variance /= float64(len(values))
	stdDev := math.Sqrt(variance)

	if stdDev == 0 {
		result := make([]float64, len(values))
		return result
	}

	result := make([]float64, len(values))
	for i, v := range values {
		result[i] = (v - mean) / stdDev
	}
	return result
}
