// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"math"
)

// Vector represents a fixed-size vector.
type Vector struct {
	data []float64
}

// NewVector creates a vector with given values.
func NewVector(values ...float64) Vector {
	data := make([]float64, len(values))
	copy(data, values)
	return Vector{data: data}
}

// ZeroVector creates a zero vector of given size.
func ZeroVector(size int) Vector {
	return Vector{data: make([]float64, size)}
}

// OnesVector creates a vector of ones.
func OnesVector(size int) Vector {
	data := make([]float64, size)
	for i := range data {
		data[i] = 1.0
	}
	return Vector{data: data}
}

// Len returns vector length.
func (v Vector) Len() int {
	return len(v.data)
}

// Get returns element at index.
func (v Vector) Get(i int) (float64, bool) {
	if i < 0 || i >= len(v.data) {
		return 0, false
	}
	return v.data[i], true
}

// Set sets element at index.
func (v Vector) Set(i int, value float64) bool {
	if i < 0 || i >= len(v.data) {
		return false
	}
	v.data[i] = value
	return true
}

// Add adds two vectors.
func (v Vector) Add(other Vector) (Vector, bool) {
	if v.Len() != other.Len() {
		return Vector{}, false
	}
	result := make([]float64, v.Len())
	for i := range result {
		result[i] = v.data[i] + other.data[i]
	}
	return Vector{data: result}, true
}

// Sub subtracts two vectors.
func (v Vector) Sub(other Vector) (Vector, bool) {
	if v.Len() != other.Len() {
		return Vector{}, false
	}
	result := make([]float64, v.Len())
	for i := range result {
		result[i] = v.data[i] - other.data[i]
	}
	return Vector{data: result}, true
}

// Scale multiplies vector by scalar.
func (v Vector) Scale(scalar float64) Vector {
	result := make([]float64, v.Len())
	for i := range result {
		result[i] = v.data[i] * scalar
	}
	return Vector{data: result}
}

// Dot computes dot product.
func (v Vector) Dot(other Vector) (float64, bool) {
	if v.Len() != other.Len() {
		return 0, false
	}
	var sum float64
	for i := range v.data {
		sum += v.data[i] * other.data[i]
	}
	return sum, true
}

// Magnitude computes vector magnitude.
func (v Vector) Magnitude() float64 {
	var sum float64
	for _, x := range v.data {
		sum += x * x
	}
	return math.Sqrt(sum)
}

// Normalize returns unit vector.
func (v Vector) Normalize() (Vector, bool) {
	mag := v.Magnitude()
	if mag == 0 {
		return Vector{}, false
	}
	return v.Scale(1 / mag), true
}

// ToSlice returns underlying slice.
func (v Vector) ToSlice() []float64 {
	result := make([]float64, len(v.data))
	copy(result, v.data)
	return result
}

// Matrix represents a 2D matrix.
type Matrix struct {
	data []float64
	rows int
	cols int
}

// NewMatrix creates a matrix from values.
func NewMatrix(rows, cols int, values []float64) (Matrix, bool) {
	if rows < 1 || cols < 1 || len(values) != rows*cols {
		return Matrix{}, false
	}
	data := make([]float64, len(values))
	copy(data, values)
	return Matrix{data: data, rows: rows, cols: cols}, true
}

// ZeroMatrix creates a zero matrix.
func ZeroMatrix(rows, cols int) Matrix {
	return Matrix{
		data: make([]float64, rows*cols),
		rows: rows,
		cols: cols,
	}
}

// IdentityMatrix creates an identity matrix.
func IdentityMatrix(size int) Matrix {
	m := ZeroMatrix(size, size)
	for i := 0; i < size; i++ {
		m.data[i*size+i] = 1.0
	}
	return m
}

// Rows returns number of rows.
func (m Matrix) Rows() int {
	return m.rows
}

// Cols returns number of columns.
func (m Matrix) Cols() int {
	return m.cols
}

// Get returns element at (row, col).
func (m Matrix) Get(row, col int) (float64, bool) {
	if row < 0 || row >= m.rows || col < 0 || col >= m.cols {
		return 0, false
	}
	return m.data[row*m.cols+col], true
}

// Set sets element at (row, col).
func (m Matrix) Set(row, col int, value float64) bool {
	if row < 0 || row >= m.rows || col < 0 || col >= m.cols {
		return false
	}
	m.data[row*m.cols+col] = value
	return true
}

// Add adds two matrices.
func (m Matrix) Add(other Matrix) (Matrix, bool) {
	if m.rows != other.rows || m.cols != other.cols {
		return Matrix{}, false
	}
	result := make([]float64, len(m.data))
	for i := range result {
		result[i] = m.data[i] + other.data[i]
	}
	return Matrix{data: result, rows: m.rows, cols: m.cols}, true
}

// Sub subtracts two matrices.
func (m Matrix) Sub(other Matrix) (Matrix, bool) {
	if m.rows != other.rows || m.cols != other.cols {
		return Matrix{}, false
	}
	result := make([]float64, len(m.data))
	for i := range result {
		result[i] = m.data[i] - other.data[i]
	}
	return Matrix{data: result, rows: m.rows, cols: m.cols}, true
}

// Scale multiplies matrix by scalar.
func (m Matrix) Scale(scalar float64) Matrix {
	result := make([]float64, len(m.data))
	for i := range result {
		result[i] = m.data[i] * scalar
	}
	return Matrix{data: result, rows: m.rows, cols: m.cols}
}

// Mul multiplies two matrices.
func (m Matrix) Mul(other Matrix) (Matrix, bool) {
	if m.cols != other.rows {
		return Matrix{}, false
	}

	result := make([]float64, m.rows*other.cols)
	for i := 0; i < m.rows; i++ {
		for j := 0; j < other.cols; j++ {
			var sum float64
			for k := 0; k < m.cols; k++ {
				sum += m.data[i*m.cols+k] * other.data[k*other.cols+j]
			}
			result[i*other.cols+j] = sum
		}
	}

	return Matrix{data: result, rows: m.rows, cols: other.cols}, true
}

// Transpose returns transposed matrix.
func (m Matrix) Transpose() Matrix {
	result := make([]float64, len(m.data))
	for i := 0; i < m.rows; i++ {
		for j := 0; j < m.cols; j++ {
			result[j*m.rows+i] = m.data[i*m.cols+j]
		}
	}
	return Matrix{data: result, rows: m.cols, cols: m.rows}
}

// MulVec multiplies matrix by vector.
func (m Matrix) MulVec(v Vector) (Vector, bool) {
	if m.cols != v.Len() {
		return Vector{}, false
	}

	result := make([]float64, m.rows)
	for i := 0; i < m.rows; i++ {
		var sum float64
		for j := 0; j < m.cols; j++ {
			sum += m.data[i*m.cols+j] * v.data[j]
		}
		result[i] = sum
	}

	return Vector{data: result}, true
}

// ToSlice returns underlying data as slice.
func (m Matrix) ToSlice() []float64 {
	result := make([]float64, len(m.data))
	copy(result, m.data)
	return result
}
