// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeMatrix - Matrix operations with dimension checking that cannot crash.
 *
 * Provides dense matrix operations with guaranteed dimension validation,
 * bounds checking, and safe arithmetic. All operations return errors
 * instead of crashing on invalid inputs.
 */

/** Error types for matrix operations */
type matrixError =
  | DimensionMismatch
  | IndexOutOfBounds
  | InvalidShape
  | NotSquare
  | SingularMatrix
  | Overflow

/** Maximum supported matrix dimension to prevent excessive memory allocation */
let maxDimension = 4096

/** A dense matrix with safe operations */
type matrix = {
  data: array<float>,
  rows: int,
  cols: int,
}

/** Create a new zero-initialized matrix with given dimensions */
let make = (rows: int, cols: int): result<matrix, matrixError> => {
  if rows <= 0 || cols <= 0 {
    Error(InvalidShape)
  } else if rows > maxDimension || cols > maxDimension {
    Error(InvalidShape)
  } else {
    let totalSize = rows * cols
    // Check for overflow
    if totalSize / rows != cols {
      Error(Overflow)
    } else {
      Ok({
        data: Belt.Array.make(totalSize, 0.0),
        rows,
        cols,
      })
    }
  }
}

/** Create a matrix from a 2D array of values */
let fromArray = (values: array<array<float>>): result<matrix, matrixError> => {
  let numRows = Belt.Array.length(values)
  if numRows == 0 {
    Error(InvalidShape)
  } else {
    let firstRow = Belt.Array.getUnsafe(values, 0)
    let numCols = Belt.Array.length(firstRow)
    if numCols == 0 {
      Error(InvalidShape)
    } else {
      // Verify all rows have the same column count
      let validShape = Belt.Array.every(values, row => Belt.Array.length(row) == numCols)
      if !validShape {
        Error(DimensionMismatch)
      } else {
        switch make(numRows, numCols) {
        | Error(e) => Error(e)
        | Ok(matrix) =>
          Belt.Array.forEachWithIndex(values, (rowIdx, row) => {
            Belt.Array.forEachWithIndex(row, (colIdx, val_) => {
              Belt.Array.setUnsafe(matrix.data, rowIdx * numCols + colIdx, val_)
            })
          })
          Ok(matrix)
        }
      }
    }
  }
}

/** Create an identity matrix of given size */
let identity = (size: int): result<matrix, matrixError> => {
  switch make(size, size) {
  | Error(e) => Error(e)
  | Ok(matrix) =>
    for i in 0 to size - 1 {
      Belt.Array.setUnsafe(matrix.data, i * size + i, 1.0)
    }
    Ok(matrix)
  }
}

/** Create a diagonal matrix from a slice of values */
let diagonal = (values: array<float>): result<matrix, matrixError> => {
  let size = Belt.Array.length(values)
  if size == 0 {
    Error(InvalidShape)
  } else {
    switch make(size, size) {
    | Error(e) => Error(e)
    | Ok(matrix) =>
      Belt.Array.forEachWithIndex(values, (i, val_) => {
        Belt.Array.setUnsafe(matrix.data, i * size + i, val_)
      })
      Ok(matrix)
    }
  }
}

/** Clone the matrix */
let clone = (m: matrix): matrix => {
  {
    data: Belt.Array.copy(m.data),
    rows: m.rows,
    cols: m.cols,
  }
}

/** Get the element at (row, col) */
let get = (m: matrix, row: int, col: int): result<float, matrixError> => {
  if row < 0 || row >= m.rows || col < 0 || col >= m.cols {
    Error(IndexOutOfBounds)
  } else {
    Ok(Belt.Array.getUnsafe(m.data, row * m.cols + col))
  }
}

/** Set the element at (row, col) */
let set = (m: matrix, row: int, col: int, value: float): result<unit, matrixError> => {
  if row < 0 || row >= m.rows || col < 0 || col >= m.cols {
    Error(IndexOutOfBounds)
  } else {
    Belt.Array.setUnsafe(m.data, row * m.cols + col, value)
    Ok()
  }
}

/** Get the shape as (rows, cols) */
let shape = (m: matrix): (int, int) => {
  (m.rows, m.cols)
}

/** Check if the matrix is square */
let isSquare = (m: matrix): bool => {
  m.rows == m.cols
}

/** Check if dimensions match another matrix */
let hasSameShape = (a: matrix, b: matrix): bool => {
  a.rows == b.rows && a.cols == b.cols
}

/** Check if multiplication is compatible with another matrix */
let canMultiplyWith = (a: matrix, b: matrix): bool => {
  a.cols == b.rows
}

/** Add two matrices element-wise */
let add = (a: matrix, b: matrix): result<matrix, matrixError> => {
  if !hasSameShape(a, b) {
    Error(DimensionMismatch)
  } else {
    switch make(a.rows, a.cols) {
    | Error(e) => Error(e)
    | Ok(result) =>
      Belt.Array.forEachWithIndex(a.data, (i, val_) => {
        Belt.Array.setUnsafe(result.data, i, val_ +. Belt.Array.getUnsafe(b.data, i))
      })
      Ok(result)
    }
  }
}

/** Subtract two matrices element-wise */
let sub = (a: matrix, b: matrix): result<matrix, matrixError> => {
  if !hasSameShape(a, b) {
    Error(DimensionMismatch)
  } else {
    switch make(a.rows, a.cols) {
    | Error(e) => Error(e)
    | Ok(result) =>
      Belt.Array.forEachWithIndex(a.data, (i, val_) => {
        Belt.Array.setUnsafe(result.data, i, val_ -. Belt.Array.getUnsafe(b.data, i))
      })
      Ok(result)
    }
  }
}

/** Multiply two matrices */
let mul = (a: matrix, b: matrix): result<matrix, matrixError> => {
  if !canMultiplyWith(a, b) {
    Error(DimensionMismatch)
  } else {
    switch make(a.rows, b.cols) {
    | Error(e) => Error(e)
    | Ok(result) =>
      for i in 0 to a.rows - 1 {
        for j in 0 to b.cols - 1 {
          let sum = ref(0.0)
          for k in 0 to a.cols - 1 {
            sum :=
              sum.contents +.
              Belt.Array.getUnsafe(a.data, i * a.cols + k) *.
              Belt.Array.getUnsafe(b.data, k * b.cols + j)
          }
          Belt.Array.setUnsafe(result.data, i * b.cols + j, sum.contents)
        }
      }
      Ok(result)
    }
  }
}

/** Scale the matrix by a scalar value */
let scale = (m: matrix, scalar: float): matrix => {
  {
    data: Belt.Array.map(m.data, v => v *. scalar),
    rows: m.rows,
    cols: m.cols,
  }
}

/** Transpose the matrix */
let transpose = (m: matrix): result<matrix, matrixError> => {
  switch make(m.cols, m.rows) {
  | Error(e) => Error(e)
  | Ok(result) =>
    for i in 0 to m.rows - 1 {
      for j in 0 to m.cols - 1 {
        Belt.Array.setUnsafe(result.data, j * m.rows + i, Belt.Array.getUnsafe(m.data, i * m.cols + j))
      }
    }
    Ok(result)
  }
}

/** Compute the trace (sum of diagonal elements) */
let trace = (m: matrix): result<float, matrixError> => {
  if !isSquare(m) {
    Error(NotSquare)
  } else {
    let sum = ref(0.0)
    for i in 0 to m.rows - 1 {
      sum := sum.contents +. Belt.Array.getUnsafe(m.data, i * m.cols + i)
    }
    Ok(sum.contents)
  }
}

/** Compute the Frobenius norm squared (sum of squared elements) */
let frobeniusNormSquared = (m: matrix): float => {
  Belt.Array.reduce(m.data, 0.0, (acc, v) => acc +. v *. v)
}

/** Compute the Frobenius norm */
let frobeniusNorm = (m: matrix): float => {
  Js.Math.sqrt(frobeniusNormSquared(m))
}

/** Get a row as a slice */
let getRow = (m: matrix, row: int): result<array<float>, matrixError> => {
  if row < 0 || row >= m.rows {
    Error(IndexOutOfBounds)
  } else {
    let start = row * m.cols
    Ok(Belt.Array.slice(m.data, ~offset=start, ~len=m.cols))
  }
}

/** Get a column as a new array */
let getColumn = (m: matrix, col: int): result<array<float>, matrixError> => {
  if col < 0 || col >= m.cols {
    Error(IndexOutOfBounds)
  } else {
    let column = Belt.Array.make(m.rows, 0.0)
    for i in 0 to m.rows - 1 {
      Belt.Array.setUnsafe(column, i, Belt.Array.getUnsafe(m.data, i * m.cols + col))
    }
    Ok(column)
  }
}

/** Extract a submatrix */
let submatrix = (
  m: matrix,
  rowStart: int,
  colStart: int,
  numRows: int,
  numCols: int,
): result<matrix, matrixError> => {
  if rowStart + numRows > m.rows || colStart + numCols > m.cols {
    Error(IndexOutOfBounds)
  } else if numRows <= 0 || numCols <= 0 {
    Error(InvalidShape)
  } else {
    switch make(numRows, numCols) {
    | Error(e) => Error(e)
    | Ok(result) =>
      for i in 0 to numRows - 1 {
        for j in 0 to numCols - 1 {
          Belt.Array.setUnsafe(
            result.data,
            i * numCols + j,
            Belt.Array.getUnsafe(m.data, (rowStart + i) * m.cols + (colStart + j)),
          )
        }
      }
      Ok(result)
    }
  }
}

/** Element-wise multiplication (Hadamard product) */
let hadamard = (a: matrix, b: matrix): result<matrix, matrixError> => {
  if !hasSameShape(a, b) {
    Error(DimensionMismatch)
  } else {
    switch make(a.rows, a.cols) {
    | Error(e) => Error(e)
    | Ok(result) =>
      Belt.Array.forEachWithIndex(a.data, (i, val_) => {
        Belt.Array.setUnsafe(result.data, i, val_ *. Belt.Array.getUnsafe(b.data, i))
      })
      Ok(result)
    }
  }
}

/** Check if the matrix is symmetric (A == A^T) */
let isSymmetric = (m: matrix): bool => {
  if !isSquare(m) {
    false
  } else {
    let symmetric = ref(true)
    for i in 0 to m.rows - 1 {
      for j in i + 1 to m.cols - 1 {
        if Belt.Array.getUnsafe(m.data, i * m.cols + j) !=
          Belt.Array.getUnsafe(m.data, j * m.cols + i) {
          symmetric := false
        }
      }
    }
    symmetric.contents
  }
}

/** Check if the matrix is diagonal */
let isDiagonal = (m: matrix): bool => {
  if !isSquare(m) {
    false
  } else {
    let diagonal_ = ref(true)
    for i in 0 to m.rows - 1 {
      for j in 0 to m.cols - 1 {
        if i != j && Belt.Array.getUnsafe(m.data, i * m.cols + j) != 0.0 {
          diagonal_ := false
        }
      }
    }
    diagonal_.contents
  }
}

/** Check if the matrix is the identity matrix */
let isIdentity = (m: matrix): bool => {
  if !isSquare(m) {
    false
  } else {
    let isIdent = ref(true)
    for i in 0 to m.rows - 1 {
      for j in 0 to m.cols - 1 {
        let expected = if i == j {
          1.0
        } else {
          0.0
        }
        if Belt.Array.getUnsafe(m.data, i * m.cols + j) != expected {
          isIdent := false
        }
      }
    }
    isIdent.contents
  }
}

/** Sum of all elements */
let sum = (m: matrix): float => {
  Belt.Array.reduce(m.data, 0.0, (acc, v) => acc +. v)
}

/** Maximum element */
let max = (m: matrix): option<float> => {
  if Belt.Array.length(m.data) == 0 {
    None
  } else {
    let maxVal = ref(Belt.Array.getUnsafe(m.data, 0))
    Belt.Array.forEach(m.data, v => {
      if v > maxVal.contents {
        maxVal := v
      }
    })
    Some(maxVal.contents)
  }
}

/** Minimum element */
let min = (m: matrix): option<float> => {
  if Belt.Array.length(m.data) == 0 {
    None
  } else {
    let minVal = ref(Belt.Array.getUnsafe(m.data, 0))
    Belt.Array.forEach(m.data, v => {
      if v < minVal.contents {
        minVal := v
      }
    })
    Some(minVal.contents)
  }
}

/** Validate that matrix dimensions are compatible for addition */
let validateAddition = (
  rowsA: int,
  colsA: int,
  rowsB: int,
  colsB: int,
): result<unit, matrixError> => {
  if rowsA != rowsB || colsA != colsB {
    Error(DimensionMismatch)
  } else {
    Ok()
  }
}

/** Validate that matrix dimensions are compatible for multiplication */
let validateMultiplication = (
  _rowsA: int,
  colsA: int,
  rowsB: int,
  _colsB: int,
): result<unit, matrixError> => {
  if colsA != rowsB {
    Error(DimensionMismatch)
  } else {
    Ok()
  }
}

/** Validate that a matrix is square */
let validateSquare = (rows: int, cols: int): result<unit, matrixError> => {
  if rows != cols {
    Error(NotSquare)
  } else {
    Ok()
  }
}

/** Compute the result dimensions for matrix multiplication */
let multiplicationResultShape = (
  rowsA: int,
  colsA: int,
  rowsB: int,
  colsB: int,
): result<(int, int), matrixError> => {
  switch validateMultiplication(rowsA, colsA, rowsB, colsB) {
  | Error(e) => Error(e)
  | Ok() => Ok((rowsA, colsB))
  }
}

/** Convert matrix to 2D array */
let toArray = (m: matrix): array<array<float>> => {
  Belt.Array.makeBy(m.rows, i => {
    Belt.Array.makeBy(m.cols, j => {
      Belt.Array.getUnsafe(m.data, i * m.cols + j)
    })
  })
}

/** Fill the entire matrix with a single value */
let fill = (m: matrix, value: float): unit => {
  Belt.Array.fill(m.data, ~offset=0, ~len=Belt.Array.length(m.data), value)
}

/** Create a matrix filled with ones */
let ones = (rows: int, cols: int): result<matrix, matrixError> => {
  switch make(rows, cols) {
  | Error(e) => Error(e)
  | Ok(m) =>
    fill(m, 1.0)
    Ok(m)
  }
}

/** Apply a function to each element */
let map = (m: matrix, f: float => float): matrix => {
  {
    data: Belt.Array.map(m.data, f),
    rows: m.rows,
    cols: m.cols,
  }
}

/** Check approximate equality within epsilon */
let approxEqual = (a: matrix, b: matrix, epsilon: float): bool => {
  if !hasSameShape(a, b) {
    false
  } else {
    Belt.Array.everyWithIndex(a.data, (i, v) => {
      Js.Math.abs_float(v -. Belt.Array.getUnsafe(b.data, i)) <= epsilon
    })
  }
}

/** Dot product of two vectors (1D matrices or arrays) */
let dotProduct = (a: array<float>, b: array<float>): result<float, matrixError> => {
  if Belt.Array.length(a) != Belt.Array.length(b) {
    Error(DimensionMismatch)
  } else {
    let sum = ref(0.0)
    Belt.Array.forEachWithIndex(a, (i, v) => {
      sum := sum.contents +. v *. Belt.Array.getUnsafe(b, i)
    })
    Ok(sum.contents)
  }
}
