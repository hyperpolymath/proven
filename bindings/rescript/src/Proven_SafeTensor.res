// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeTensor - Tensor operations that cannot crash.
 *
 * Provides safe vector and matrix operations with proper bounds checking
 * and dimension validation. All operations return Result types on failure.
 */

/** Error types for tensor operations */
type tensorError =
  | DimensionMismatch
  | InvalidShape
  | IndexOutOfBounds
  | InvalidOperation

/** A 1D tensor (vector) */
type vector = {data: array<float>}

/** A 2D tensor (matrix) */
type matrix = {
  data: array<float>,
  rows: int,
  cols: int,
}

// =============================================================================
// Vector Operations
// =============================================================================

/** Create a zero-filled vector of given size */
let vectorZeros = (size: int): result<vector, tensorError> => {
  if size < 0 {
    Error(InvalidShape)
  } else {
    Ok({data: Belt.Array.make(size, 0.0)})
  }
}

/** Create a ones-filled vector of given size */
let vectorOnes = (size: int): result<vector, tensorError> => {
  if size < 0 {
    Error(InvalidShape)
  } else {
    Ok({data: Belt.Array.make(size, 1.0)})
  }
}

/** Create a vector from an array */
let vectorFromArray = (arr: array<float>): vector => {
  {data: Belt.Array.copy(arr)}
}

/** Get the length of a vector */
let vectorLen = (v: vector): int => {
  Belt.Array.length(v.data)
}

/** Get element at index */
let vectorGet = (v: vector, index: int): result<float, tensorError> => {
  if index < 0 || index >= Belt.Array.length(v.data) {
    Error(IndexOutOfBounds)
  } else {
    switch Belt.Array.get(v.data, index) {
    | Some(value) => Ok(value)
    | None => Error(IndexOutOfBounds)
    }
  }
}

/** Set element at index (returns new vector) */
let vectorSet = (v: vector, index: int, value: float): result<vector, tensorError> => {
  if index < 0 || index >= Belt.Array.length(v.data) {
    Error(IndexOutOfBounds)
  } else {
    let newData = Belt.Array.copy(v.data)
    Belt.Array.setUnsafe(newData, index, value)
    Ok({data: newData})
  }
}

/** Dot product of two vectors */
let vectorDot = (a: vector, b: vector): result<float, tensorError> => {
  if Belt.Array.length(a.data) != Belt.Array.length(b.data) {
    Error(DimensionMismatch)
  } else {
    let sum = ref(0.0)
    for i in 0 to Belt.Array.length(a.data) - 1 {
      let va = Belt.Array.getUnsafe(a.data, i)
      let vb = Belt.Array.getUnsafe(b.data, i)
      sum := sum.contents +. va *. vb
    }
    Ok(sum.contents)
  }
}

/** Add two vectors element-wise */
let vectorAdd = (a: vector, b: vector): result<vector, tensorError> => {
  if Belt.Array.length(a.data) != Belt.Array.length(b.data) {
    Error(DimensionMismatch)
  } else {
    let result = Belt.Array.make(Belt.Array.length(a.data), 0.0)
    for i in 0 to Belt.Array.length(a.data) - 1 {
      let va = Belt.Array.getUnsafe(a.data, i)
      let vb = Belt.Array.getUnsafe(b.data, i)
      Belt.Array.setUnsafe(result, i, va +. vb)
    }
    Ok({data: result})
  }
}

/** Subtract two vectors element-wise */
let vectorSub = (a: vector, b: vector): result<vector, tensorError> => {
  if Belt.Array.length(a.data) != Belt.Array.length(b.data) {
    Error(DimensionMismatch)
  } else {
    let result = Belt.Array.make(Belt.Array.length(a.data), 0.0)
    for i in 0 to Belt.Array.length(a.data) - 1 {
      let va = Belt.Array.getUnsafe(a.data, i)
      let vb = Belt.Array.getUnsafe(b.data, i)
      Belt.Array.setUnsafe(result, i, va -. vb)
    }
    Ok({data: result})
  }
}

/** Scale a vector by a scalar */
let vectorScale = (v: vector, scalar: float): vector => {
  let result = Belt.Array.map(v.data, x => x *. scalar)
  {data: result}
}

/** Calculate L2 norm (Euclidean length) of a vector */
let vectorNorm = (v: vector): float => {
  let sumSq = ref(0.0)
  Belt.Array.forEach(v.data, x => {
    sumSq := sumSq.contents +. x *. x
  })
  Js.Math.sqrt(sumSq.contents)
}

/** Calculate L1 norm (Manhattan distance) of a vector */
let vectorNormL1 = (v: vector): float => {
  let sum = ref(0.0)
  Belt.Array.forEach(v.data, x => {
    sum := sum.contents +. Js.Math.abs_float(x)
  })
  sum.contents
}

/** Normalize a vector to unit length */
let vectorNormalize = (v: vector): result<vector, tensorError> => {
  let norm = vectorNorm(v)
  if norm == 0.0 {
    Error(InvalidOperation)
  } else {
    Ok(vectorScale(v, 1.0 /. norm))
  }
}

/** Create a range vector [start, start+1, ..., start+size-1] */
let vectorRange = (start: float, size: int): result<vector, tensorError> => {
  if size < 0 {
    Error(InvalidShape)
  } else {
    let data = Belt.Array.makeByU(size, (. i) => start +. Belt.Int.toFloat(i))
    Ok({data: data})
  }
}

// =============================================================================
// Matrix Operations
// =============================================================================

/** Create a zero-filled matrix */
let matrixZeros = (rows: int, cols: int): result<matrix, tensorError> => {
  if rows < 0 || cols < 0 {
    Error(InvalidShape)
  } else {
    Ok({
      data: Belt.Array.make(rows * cols, 0.0),
      rows: rows,
      cols: cols,
    })
  }
}

/** Create an identity matrix */
let matrixIdentity = (size: int): result<matrix, tensorError> => {
  if size < 0 {
    Error(InvalidShape)
  } else {
    let data = Belt.Array.make(size * size, 0.0)
    for i in 0 to size - 1 {
      Belt.Array.setUnsafe(data, i * size + i, 1.0)
    }
    Ok({data: data, rows: size, cols: size})
  }
}

/** Create a matrix from a 2D array (array of rows) */
let matrixFromArray2D = (arr: array<array<float>>): result<matrix, tensorError> => {
  let rows = Belt.Array.length(arr)
  if rows == 0 {
    Ok({data: [], rows: 0, cols: 0})
  } else {
    let cols = switch Belt.Array.get(arr, 0) {
    | Some(row) => Belt.Array.length(row)
    | None => 0
    }

    // Validate all rows have same length
    let valid = ref(true)
    Belt.Array.forEach(arr, row => {
      if Belt.Array.length(row) != cols {
        valid := false
      }
    })

    if !valid.contents {
      Error(InvalidShape)
    } else {
      let data = Belt.Array.make(rows * cols, 0.0)
      for i in 0 to rows - 1 {
        let row = Belt.Array.getUnsafe(arr, i)
        for j in 0 to cols - 1 {
          let value = Belt.Array.getUnsafe(row, j)
          Belt.Array.setUnsafe(data, i * cols + j, value)
        }
      }
      Ok({data: data, rows: rows, cols: cols})
    }
  }
}

/** Get the shape of a matrix as (rows, cols) */
let matrixShape = (m: matrix): (int, int) => {
  (m.rows, m.cols)
}

/** Get element at (row, col) */
let matrixGet = (m: matrix, row: int, col: int): result<float, tensorError> => {
  if row < 0 || row >= m.rows || col < 0 || col >= m.cols {
    Error(IndexOutOfBounds)
  } else {
    let index = row * m.cols + col
    switch Belt.Array.get(m.data, index) {
    | Some(value) => Ok(value)
    | None => Error(IndexOutOfBounds)
    }
  }
}

/** Set element at (row, col) (returns new matrix) */
let matrixSet = (m: matrix, row: int, col: int, value: float): result<matrix, tensorError> => {
  if row < 0 || row >= m.rows || col < 0 || col >= m.cols {
    Error(IndexOutOfBounds)
  } else {
    let newData = Belt.Array.copy(m.data)
    let index = row * m.cols + col
    Belt.Array.setUnsafe(newData, index, value)
    Ok({data: newData, rows: m.rows, cols: m.cols})
  }
}

/** Add two matrices element-wise */
let matrixAdd = (a: matrix, b: matrix): result<matrix, tensorError> => {
  if a.rows != b.rows || a.cols != b.cols {
    Error(DimensionMismatch)
  } else {
    let result = Belt.Array.make(a.rows * a.cols, 0.0)
    for i in 0 to Belt.Array.length(a.data) - 1 {
      let va = Belt.Array.getUnsafe(a.data, i)
      let vb = Belt.Array.getUnsafe(b.data, i)
      Belt.Array.setUnsafe(result, i, va +. vb)
    }
    Ok({data: result, rows: a.rows, cols: a.cols})
  }
}

/** Subtract two matrices element-wise */
let matrixSub = (a: matrix, b: matrix): result<matrix, tensorError> => {
  if a.rows != b.rows || a.cols != b.cols {
    Error(DimensionMismatch)
  } else {
    let result = Belt.Array.make(a.rows * a.cols, 0.0)
    for i in 0 to Belt.Array.length(a.data) - 1 {
      let va = Belt.Array.getUnsafe(a.data, i)
      let vb = Belt.Array.getUnsafe(b.data, i)
      Belt.Array.setUnsafe(result, i, va -. vb)
    }
    Ok({data: result, rows: a.rows, cols: a.cols})
  }
}

/** Multiply two matrices */
let matrixMul = (a: matrix, b: matrix): result<matrix, tensorError> => {
  if a.cols != b.rows {
    Error(DimensionMismatch)
  } else {
    let result = Belt.Array.make(a.rows * b.cols, 0.0)

    for i in 0 to a.rows - 1 {
      for j in 0 to b.cols - 1 {
        let sum = ref(0.0)
        for k in 0 to a.cols - 1 {
          let va = Belt.Array.getUnsafe(a.data, i * a.cols + k)
          let vb = Belt.Array.getUnsafe(b.data, k * b.cols + j)
          sum := sum.contents +. va *. vb
        }
        Belt.Array.setUnsafe(result, i * b.cols + j, sum.contents)
      }
    }

    Ok({data: result, rows: a.rows, cols: b.cols})
  }
}

/** Transpose a matrix */
let matrixTranspose = (m: matrix): matrix => {
  let result = Belt.Array.make(m.rows * m.cols, 0.0)

  for i in 0 to m.rows - 1 {
    for j in 0 to m.cols - 1 {
      let value = Belt.Array.getUnsafe(m.data, i * m.cols + j)
      Belt.Array.setUnsafe(result, j * m.rows + i, value)
    }
  }

  {data: result, rows: m.cols, cols: m.rows}
}

/** Scale a matrix by a scalar */
let matrixScale = (m: matrix, scalar: float): matrix => {
  let result = Belt.Array.map(m.data, x => x *. scalar)
  {data: result, rows: m.rows, cols: m.cols}
}

/** Calculate the trace of a square matrix (sum of diagonal) */
let matrixTrace = (m: matrix): result<float, tensorError> => {
  if m.rows != m.cols {
    Error(InvalidOperation)
  } else {
    let sum = ref(0.0)
    for i in 0 to m.rows - 1 {
      let value = Belt.Array.getUnsafe(m.data, i * m.cols + i)
      sum := sum.contents +. value
    }
    Ok(sum.contents)
  }
}

/** Calculate the Frobenius norm squared of a matrix */
let matrixFrobeniusNormSq = (m: matrix): float => {
  let sum = ref(0.0)
  Belt.Array.forEach(m.data, x => {
    sum := sum.contents +. x *. x
  })
  sum.contents
}

/** Calculate the Frobenius norm of a matrix */
let matrixFrobeniusNorm = (m: matrix): float => {
  Js.Math.sqrt(matrixFrobeniusNormSq(m))
}

/** Get a row from a matrix as a vector */
let matrixGetRow = (m: matrix, row: int): result<vector, tensorError> => {
  if row < 0 || row >= m.rows {
    Error(IndexOutOfBounds)
  } else {
    let data = Belt.Array.make(m.cols, 0.0)
    for j in 0 to m.cols - 1 {
      let value = Belt.Array.getUnsafe(m.data, row * m.cols + j)
      Belt.Array.setUnsafe(data, j, value)
    }
    Ok({data: data})
  }
}

/** Get a column from a matrix as a vector */
let matrixGetCol = (m: matrix, col: int): result<vector, tensorError> => {
  if col < 0 || col >= m.cols {
    Error(IndexOutOfBounds)
  } else {
    let data = Belt.Array.make(m.rows, 0.0)
    for i in 0 to m.rows - 1 {
      let value = Belt.Array.getUnsafe(m.data, i * m.cols + col)
      Belt.Array.setUnsafe(data, i, value)
    }
    Ok({data: data})
  }
}

/** Matrix-vector multiplication */
let matrixVectorMul = (m: matrix, v: vector): result<vector, tensorError> => {
  if m.cols != vectorLen(v) {
    Error(DimensionMismatch)
  } else {
    let result = Belt.Array.make(m.rows, 0.0)

    for i in 0 to m.rows - 1 {
      let sum = ref(0.0)
      for j in 0 to m.cols - 1 {
        let mVal = Belt.Array.getUnsafe(m.data, i * m.cols + j)
        let vVal = Belt.Array.getUnsafe(v.data, j)
        sum := sum.contents +. mVal *. vVal
      }
      Belt.Array.setUnsafe(result, i, sum.contents)
    }

    Ok({data: result})
  }
}

/** Convert tensor error to string */
let errorToString = (err: tensorError): string => {
  switch err {
  | DimensionMismatch => "Dimension mismatch"
  | InvalidShape => "Invalid shape"
  | IndexOutOfBounds => "Index out of bounds"
  | InvalidOperation => "Invalid operation"
  }
}
