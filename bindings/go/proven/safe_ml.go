// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeML provides machine learning utility functions via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
import "C"
import "unsafe"

// MLSoftmax applies the softmax function to an input slice, writing results
// to the output slice. Both slices must have the same length.
func MLSoftmax(input []float64) ([]float64, error) {
	if len(input) == 0 {
		return nil, nil
	}
	output := make([]float64, len(input))
	inPtr := (*C.double)(unsafe.Pointer(&input[0]))
	outPtr := (*C.double)(unsafe.Pointer(&output[0]))
	status := int(C.proven_ml_softmax(inPtr, outPtr, C.size_t(len(input))))
	if status != StatusOK {
		return nil, newError(status)
	}
	return output, nil
}

// MLSigmoid computes the sigmoid function: 1 / (1 + exp(-x)).
func MLSigmoid(x float64) float64 {
	return float64(C.proven_ml_sigmoid(C.double(x)))
}

// MLRelu computes the ReLU (Rectified Linear Unit) function: max(0, x).
func MLRelu(x float64) float64 {
	return float64(C.proven_ml_relu(C.double(x)))
}

// MLLeakyRelu computes the Leaky ReLU function: x if x > 0, else alpha * x.
func MLLeakyRelu(x, alpha float64) float64 {
	return float64(C.proven_ml_leaky_relu(C.double(x), C.double(alpha)))
}

// MLClamp clamps a value to the range [minVal, maxVal].
func MLClamp(x, minVal, maxVal float64) float64 {
	return float64(C.proven_ml_clamp(C.double(x), C.double(minVal), C.double(maxVal)))
}
