// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeMl - Machine learning utilities that cannot crash.
 *
 * Provides safe functions for common ML operations including activation functions,
 * normalization, loss functions, and vector operations. All operations handle
 * edge cases like empty inputs and dimension mismatches without throwing exceptions.
 */

/** Error types for ML operations */
type mlError =
  | DimensionMismatch
  | InvalidInput
  | EmptyInput
  | NumericalInstability

/** Activation function types */
type activation =
  | Relu
  | Sigmoid
  | Tanh
  | LeakyRelu
  | Softmax

// =============================================================================
// Activation Functions
// =============================================================================

/** Apply ReLU activation: max(0, x) */
let relu = (x: float): float => {
  Js.Math.max_float(0.0, x)
}

/** Apply Sigmoid activation: 1 / (1 + exp(-x)) */
let sigmoid = (x: float): float => {
  1.0 /. (1.0 +. Js.Math.exp(-.x))
}

/** Apply Tanh activation */
let tanh = (x: float): float => {
  Js.Math.tanh(x)
}

/** Apply Leaky ReLU activation: x if x > 0 else 0.01 * x */
let leakyRelu = (x: float, ~alpha: float=0.01, ()): float => {
  if x > 0.0 {
    x
  } else {
    alpha *. x
  }
}

/** Apply an activation function to a value */
let applyActivation = (activation: activation, x: float): float => {
  switch activation {
  | Relu => relu(x)
  | Sigmoid => sigmoid(x)
  | Tanh => tanh(x)
  | LeakyRelu => leakyRelu(x, ())
  | Softmax => x // Softmax requires full vector, returns identity for single value
  }
}

/** Calculate derivative of ReLU */
let reluDerivative = (x: float): float => {
  if x > 0.0 {
    1.0
  } else {
    0.0
  }
}

/** Calculate derivative of Sigmoid */
let sigmoidDerivative = (x: float): float => {
  let s = sigmoid(x)
  s *. (1.0 -. s)
}

/** Calculate derivative of Tanh */
let tanhDerivative = (x: float): float => {
  let t = tanh(x)
  1.0 -. t *. t
}

/** Calculate derivative of Leaky ReLU */
let leakyReluDerivative = (x: float, ~alpha: float=0.01, ()): float => {
  if x > 0.0 {
    1.0
  } else {
    alpha
  }
}

/** Calculate derivative of an activation function */
let activationDerivative = (activation: activation, x: float): float => {
  switch activation {
  | Relu => reluDerivative(x)
  | Sigmoid => sigmoidDerivative(x)
  | Tanh => tanhDerivative(x)
  | LeakyRelu => leakyReluDerivative(x, ())
  | Softmax => 1.0 // Special case, actual softmax derivative is a Jacobian
  }
}

// =============================================================================
// Vector Operations
// =============================================================================

/** Apply softmax to an array
 *
 * Softmax: exp(x_i) / sum(exp(x_j))
 * Uses numerical stability trick: subtract max before exp
 */
let softmax = (input: array<float>): result<array<float>, mlError> => {
  let len = Belt.Array.length(input)
  if len == 0 {
    Error(EmptyInput)
  } else {
    // Find max for numerical stability
    let maxVal = ref(Belt.Array.getUnsafe(input, 0))
    Belt.Array.forEach(input, v => {
      if v > maxVal.contents {
        maxVal := v
      }
    })

    // Calculate exp and sum
    let expValues = Belt.Array.map(input, v => Js.Math.exp(v -. maxVal.contents))
    let sum = ref(0.0)
    Belt.Array.forEach(expValues, v => {
      sum := sum.contents +. v
    })

    // Check for numerical instability
    if sum.contents == 0.0 {
      Error(NumericalInstability)
    } else {
      // Normalize
      let result = Belt.Array.map(expValues, v => v /. sum.contents)
      Ok(result)
    }
  }
}

/** Normalize data to [0, 1] range (min-max normalization) */
let normalize = (data: array<float>): result<array<float>, mlError> => {
  let len = Belt.Array.length(data)
  if len == 0 {
    Error(EmptyInput)
  } else {
    // Find min and max
    let minVal = ref(Belt.Array.getUnsafe(data, 0))
    let maxVal = ref(Belt.Array.getUnsafe(data, 0))

    Belt.Array.forEach(data, v => {
      if v < minVal.contents {
        minVal := v
      }
      if v > maxVal.contents {
        maxVal := v
      }
    })

    let range = maxVal.contents -. minVal.contents
    if range == 0.0 {
      // All values are the same, return array of 0.5s (midpoint)
      Ok(Belt.Array.make(len, 0.5))
    } else {
      let result = Belt.Array.map(data, v => (v -. minVal.contents) /. range)
      Ok(result)
    }
  }
}

/** Standardize data to zero mean and unit variance (z-score normalization) */
let standardize = (data: array<float>): result<array<float>, mlError> => {
  let len = Belt.Array.length(data)
  if len == 0 {
    Error(EmptyInput)
  } else {
    // Calculate mean
    let sum = ref(0.0)
    Belt.Array.forEach(data, v => {
      sum := sum.contents +. v
    })
    let mean = sum.contents /. Belt.Int.toFloat(len)

    // Calculate variance
    let variance = ref(0.0)
    Belt.Array.forEach(data, v => {
      let diff = v -. mean
      variance := variance.contents +. diff *. diff
    })
    variance := variance.contents /. Belt.Int.toFloat(len)

    let stdDev = Js.Math.sqrt(variance.contents)
    if stdDev == 0.0 {
      // All values are the same, return zeros
      Ok(Belt.Array.make(len, 0.0))
    } else {
      let result = Belt.Array.map(data, v => (v -. mean) /. stdDev)
      Ok(result)
    }
  }
}

/** Clip values to a range */
let clip = (data: array<float>, minVal: float, maxVal: float): array<float> => {
  Belt.Array.map(data, v => Js.Math.max_float(minVal, Js.Math.min_float(maxVal, v)))
}

// =============================================================================
// Loss Functions
// =============================================================================

/** Calculate cross-entropy loss */
let crossEntropyLoss = (predictions: array<float>, targets: array<float>): result<float, mlError> => {
  let predLen = Belt.Array.length(predictions)
  let targetLen = Belt.Array.length(targets)

  if predLen != targetLen {
    Error(DimensionMismatch)
  } else if predLen == 0 {
    Error(EmptyInput)
  } else {
    let epsilon = 1e-15 // Prevent log(0)
    let loss = ref(0.0)

    for i in 0 to predLen - 1 {
      let p = Belt.Array.getUnsafe(predictions, i)
      let t = Belt.Array.getUnsafe(targets, i)
      let clipped = Js.Math.max_float(epsilon, Js.Math.min_float(1.0 -. epsilon, p))
      loss := loss.contents -. t *. Js.Math.log(clipped)
    }

    Ok(loss.contents /. Belt.Int.toFloat(predLen))
  }
}

/** Calculate mean squared error loss */
let mseLoss = (predictions: array<float>, targets: array<float>): result<float, mlError> => {
  let predLen = Belt.Array.length(predictions)
  let targetLen = Belt.Array.length(targets)

  if predLen != targetLen {
    Error(DimensionMismatch)
  } else if predLen == 0 {
    Error(EmptyInput)
  } else {
    let sum = ref(0.0)

    for i in 0 to predLen - 1 {
      let p = Belt.Array.getUnsafe(predictions, i)
      let t = Belt.Array.getUnsafe(targets, i)
      let diff = p -. t
      sum := sum.contents +. diff *. diff
    }

    Ok(sum.contents /. Belt.Int.toFloat(predLen))
  }
}

/** Calculate mean absolute error loss */
let maeLoss = (predictions: array<float>, targets: array<float>): result<float, mlError> => {
  let predLen = Belt.Array.length(predictions)
  let targetLen = Belt.Array.length(targets)

  if predLen != targetLen {
    Error(DimensionMismatch)
  } else if predLen == 0 {
    Error(EmptyInput)
  } else {
    let sum = ref(0.0)

    for i in 0 to predLen - 1 {
      let p = Belt.Array.getUnsafe(predictions, i)
      let t = Belt.Array.getUnsafe(targets, i)
      sum := sum.contents +. Js.Math.abs_float(p -. t)
    }

    Ok(sum.contents /. Belt.Int.toFloat(predLen))
  }
}

// =============================================================================
// Metrics
// =============================================================================

/** Calculate binary accuracy */
let binaryAccuracy = (
  predictions: array<float>,
  targets: array<float>,
  ~threshold: float=0.5,
  (),
): result<float, mlError> => {
  let predLen = Belt.Array.length(predictions)
  let targetLen = Belt.Array.length(targets)

  if predLen != targetLen {
    Error(DimensionMismatch)
  } else if predLen == 0 {
    Error(EmptyInput)
  } else {
    let correct = ref(0)

    for i in 0 to predLen - 1 {
      let p = Belt.Array.getUnsafe(predictions, i)
      let t = Belt.Array.getUnsafe(targets, i)
      let predictedClass = if p >= threshold {
        1.0
      } else {
        0.0
      }
      if predictedClass == t {
        correct := correct.contents + 1
      }
    }

    Ok(Belt.Int.toFloat(correct.contents) /. Belt.Int.toFloat(predLen))
  }
}

// =============================================================================
// Vector Math Operations
// =============================================================================

/** Dot product of two vectors */
let dot = (a: array<float>, b: array<float>): result<float, mlError> => {
  if Belt.Array.length(a) != Belt.Array.length(b) {
    Error(DimensionMismatch)
  } else {
    let sum = ref(0.0)
    for i in 0 to Belt.Array.length(a) - 1 {
      let va = Belt.Array.getUnsafe(a, i)
      let vb = Belt.Array.getUnsafe(b, i)
      sum := sum.contents +. va *. vb
    }
    Ok(sum.contents)
  }
}

/** L2 norm (Euclidean) */
let l2Norm = (data: array<float>): float => {
  let sum = ref(0.0)
  Belt.Array.forEach(data, v => {
    sum := sum.contents +. v *. v
  })
  Js.Math.sqrt(sum.contents)
}

/** L1 norm (Manhattan) */
let l1Norm = (data: array<float>): float => {
  let sum = ref(0.0)
  Belt.Array.forEach(data, v => {
    sum := sum.contents +. Js.Math.abs_float(v)
  })
  sum.contents
}

/** Cosine similarity between two vectors */
let cosineSimilarity = (a: array<float>, b: array<float>): result<float, mlError> => {
  if Belt.Array.length(a) != Belt.Array.length(b) {
    Error(DimensionMismatch)
  } else {
    switch dot(a, b) {
    | Error(e) => Error(e)
    | Ok(dotProduct) =>
      let normA = l2Norm(a)
      let normB = l2Norm(b)

      if normA == 0.0 || normB == 0.0 {
        Error(NumericalInstability)
      } else {
        Ok(dotProduct /. (normA *. normB))
      }
    }
  }
}

/** One-hot encode an index */
let oneHot = (index: int, numClasses: int): result<array<float>, mlError> => {
  if index < 0 || index >= numClasses {
    Error(InvalidInput)
  } else {
    let result = Belt.Array.make(numClasses, 0.0)
    Belt.Array.setUnsafe(result, index, 1.0)
    Ok(result)
  }
}

/** Argmax - find index of maximum value */
let argmax = (data: array<float>): result<int, mlError> => {
  let len = Belt.Array.length(data)
  if len == 0 {
    Error(EmptyInput)
  } else {
    let maxIdx = ref(0)
    let maxVal = ref(Belt.Array.getUnsafe(data, 0))

    for i in 1 to len - 1 {
      let v = Belt.Array.getUnsafe(data, i)
      if v > maxVal.contents {
        maxVal := v
        maxIdx := i
      }
    }

    Ok(maxIdx.contents)
  }
}

/** Argmin - find index of minimum value */
let argmin = (data: array<float>): result<int, mlError> => {
  let len = Belt.Array.length(data)
  if len == 0 {
    Error(EmptyInput)
  } else {
    let minIdx = ref(0)
    let minVal = ref(Belt.Array.getUnsafe(data, 0))

    for i in 1 to len - 1 {
      let v = Belt.Array.getUnsafe(data, i)
      if v < minVal.contents {
        minVal := v
        minIdx := i
      }
    }

    Ok(minIdx.contents)
  }
}

/** Calculate mean of an array */
let mean = (data: array<float>): result<float, mlError> => {
  let len = Belt.Array.length(data)
  if len == 0 {
    Error(EmptyInput)
  } else {
    let sum = ref(0.0)
    Belt.Array.forEach(data, v => {
      sum := sum.contents +. v
    })
    Ok(sum.contents /. Belt.Int.toFloat(len))
  }
}

/** Calculate variance of an array */
let variance = (data: array<float>): result<float, mlError> => {
  let len = Belt.Array.length(data)
  if len == 0 {
    Error(EmptyInput)
  } else {
    switch mean(data) {
    | Error(e) => Error(e)
    | Ok(meanVal) =>
      let sum = ref(0.0)
      Belt.Array.forEach(data, v => {
        let diff = v -. meanVal
        sum := sum.contents +. diff *. diff
      })
      Ok(sum.contents /. Belt.Int.toFloat(len))
    }
  }
}

/** Calculate standard deviation of an array */
let stdDev = (data: array<float>): result<float, mlError> => {
  switch variance(data) {
  | Error(e) => Error(e)
  | Ok(v) => Ok(Js.Math.sqrt(v))
  }
}

/** Convert ML error to string */
let errorToString = (err: mlError): string => {
  switch err {
  | DimensionMismatch => "Dimension mismatch"
  | InvalidInput => "Invalid input"
  | EmptyInput => "Empty input"
  | NumericalInstability => "Numerical instability"
  }
}
