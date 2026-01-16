// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe tensor and matrix operations with bounds checking.
//!
//! This module provides safe operations for vectors and matrices commonly used
//! in machine learning and numerical computing. All operations include bounds
//! checking and prevent common errors like shape mismatches.
//!
//! # Design Philosophy
//!
//! Matrix operations in ML are prone to runtime panics from:
//! - Index out of bounds
//! - Shape mismatches in operations
//! - Empty tensor operations
//!
//! This module makes all these errors explicit through Rust's Result type.
//!
//! # Example
//!
//! ```rust
//! use proven::SafeTensor;
//!
//! // Safe dot product with shape checking
//! let a = vec![1.0, 2.0, 3.0];
//! let b = vec![4.0, 5.0, 6.0];
//! assert_eq!(SafeTensor::dot(&a, &b).unwrap(), 32.0);
//!
//! // Shape mismatch returns Error
//! let c = vec![1.0, 2.0];
//! assert!(SafeTensor::dot(&a, &c).is_err());
//! ```

use crate::core::{Error, Result};
use crate::safe_float::SafeFloat;

/// Safe tensor operations that never panic on bounds errors.
pub struct SafeTensor;

impl SafeTensor {
    /// Safe dot product with length check.
    ///
    /// Returns Error if vectors have different lengths.
    ///
    /// # Example
    /// ```rust
    /// use proven::SafeTensor;
    ///
    /// let a = vec![1.0, 2.0, 3.0];
    /// let b = vec![4.0, 5.0, 6.0];
    /// assert_eq!(SafeTensor::dot(&a, &b).unwrap(), 32.0); // 1*4 + 2*5 + 3*6 = 32
    /// ```
    pub fn dot(a: &[f64], b: &[f64]) -> Result<f64> {
        if a.len() != b.len() {
            return Err(Error::ValidationError(format!(
                "Dot product shape mismatch: {} vs {}",
                a.len(),
                b.len()
            )));
        }
        Ok(a.iter().zip(b.iter()).map(|(x, y)| x * y).sum())
    }

    /// Safe f32 dot product.
    pub fn dot_f32(a: &[f32], b: &[f32]) -> Result<f32> {
        if a.len() != b.len() {
            return Err(Error::ValidationError(format!(
                "Dot product shape mismatch: {} vs {}",
                a.len(),
                b.len()
            )));
        }
        Ok(a.iter().zip(b.iter()).map(|(x, y)| x * y).sum())
    }

    /// Safe element-wise addition.
    ///
    /// Returns Error if vectors have different lengths.
    pub fn add(a: &[f64], b: &[f64]) -> Result<Vec<f64>> {
        if a.len() != b.len() {
            return Err(Error::ValidationError(format!(
                "Add shape mismatch: {} vs {}",
                a.len(),
                b.len()
            )));
        }
        Ok(a.iter().zip(b.iter()).map(|(x, y)| x + y).collect())
    }

    /// Safe f32 element-wise addition.
    pub fn add_f32(a: &[f32], b: &[f32]) -> Result<Vec<f32>> {
        if a.len() != b.len() {
            return Err(Error::ValidationError(format!(
                "Add shape mismatch: {} vs {}",
                a.len(),
                b.len()
            )));
        }
        Ok(a.iter().zip(b.iter()).map(|(x, y)| x + y).collect())
    }

    /// Safe element-wise subtraction.
    pub fn sub(a: &[f64], b: &[f64]) -> Result<Vec<f64>> {
        if a.len() != b.len() {
            return Err(Error::ValidationError(format!(
                "Sub shape mismatch: {} vs {}",
                a.len(),
                b.len()
            )));
        }
        Ok(a.iter().zip(b.iter()).map(|(x, y)| x - y).collect())
    }

    /// Safe element-wise multiplication (Hadamard product).
    pub fn hadamard(a: &[f64], b: &[f64]) -> Result<Vec<f64>> {
        if a.len() != b.len() {
            return Err(Error::ValidationError(format!(
                "Hadamard product shape mismatch: {} vs {}",
                a.len(),
                b.len()
            )));
        }
        Ok(a.iter().zip(b.iter()).map(|(x, y)| x * y).collect())
    }

    /// Safe scalar multiplication.
    pub fn scale(v: &[f64], scalar: f64) -> Vec<f64> {
        v.iter().map(|x| x * scalar).collect()
    }

    /// Safe f32 scalar multiplication.
    pub fn scale_f32(v: &[f32], scalar: f32) -> Vec<f32> {
        v.iter().map(|x| x * scalar).collect()
    }

    /// Safe matrix-vector multiplication.
    ///
    /// Matrix is represented as rows × cols, where each inner vec is a row.
    /// Returns Error if dimensions don't match.
    ///
    /// # Example
    /// ```rust
    /// use proven::SafeTensor;
    ///
    /// let matrix = vec![
    ///     vec![1.0, 2.0],
    ///     vec![3.0, 4.0],
    /// ];
    /// let vector = vec![1.0, 2.0];
    /// let result = SafeTensor::mat_vec(&matrix, &vector).unwrap();
    /// assert_eq!(result, vec![5.0, 11.0]); // [1*1+2*2, 3*1+4*2]
    /// ```
    pub fn mat_vec(matrix: &[Vec<f64>], vector: &[f64]) -> Result<Vec<f64>> {
        if matrix.is_empty() {
            return Ok(Vec::new());
        }

        let cols = matrix[0].len();
        if vector.len() != cols {
            return Err(Error::ValidationError(format!(
                "Matrix-vector dimension mismatch: matrix cols {} vs vector len {}",
                cols,
                vector.len()
            )));
        }

        // Verify all rows have same length
        for (i, row) in matrix.iter().enumerate() {
            if row.len() != cols {
                return Err(Error::ValidationError(format!(
                    "Matrix row {} has {} cols, expected {}",
                    i,
                    row.len(),
                    cols
                )));
            }
        }

        let mut result = Vec::with_capacity(matrix.len());
        for row in matrix {
            let dot = Self::dot(row, vector)?;
            result.push(dot);
        }
        Ok(result)
    }

    /// Safe f32 matrix-vector multiplication.
    pub fn mat_vec_f32(matrix: &[Vec<f32>], vector: &[f32]) -> Result<Vec<f32>> {
        if matrix.is_empty() {
            return Ok(Vec::new());
        }

        let cols = matrix[0].len();
        if vector.len() != cols {
            return Err(Error::ValidationError(format!(
                "Matrix-vector dimension mismatch: matrix cols {} vs vector len {}",
                cols,
                vector.len()
            )));
        }

        for (i, row) in matrix.iter().enumerate() {
            if row.len() != cols {
                return Err(Error::ValidationError(format!(
                    "Matrix row {} has {} cols, expected {}",
                    i,
                    row.len(),
                    cols
                )));
            }
        }

        let mut result = Vec::with_capacity(matrix.len());
        for row in matrix {
            let dot = Self::dot_f32(row, vector)?;
            result.push(dot);
        }
        Ok(result)
    }

    /// Safe index access with bounds checking.
    ///
    /// Returns Error instead of panicking on out-of-bounds.
    pub fn get<T: Clone>(v: &[T], index: usize) -> Result<T> {
        v.get(index).cloned().ok_or_else(|| {
            Error::OutOfBounds {
                value: index as i64,
                min: 0,
                max: v.len() as i64 - 1,
            }
        })
    }

    /// Safe index access for 2D arrays.
    pub fn get_2d<T: Clone>(matrix: &[Vec<T>], row: usize, col: usize) -> Result<T> {
        let r = matrix.get(row).ok_or_else(|| Error::OutOfBounds {
            value: row as i64,
            min: 0,
            max: matrix.len() as i64 - 1,
        })?;
        r.get(col).cloned().ok_or_else(|| Error::OutOfBounds {
            value: col as i64,
            min: 0,
            max: r.len() as i64 - 1,
        })
    }

    /// Safe argmax - returns index of maximum value.
    ///
    /// Returns Error for empty vectors.
    pub fn argmax(v: &[f64]) -> Result<usize> {
        if v.is_empty() {
            return Err(Error::EmptyInput);
        }
        let (idx, _) = v
            .iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .unwrap();
        Ok(idx)
    }

    /// Safe f32 argmax.
    pub fn argmax_f32(v: &[f32]) -> Result<usize> {
        if v.is_empty() {
            return Err(Error::EmptyInput);
        }
        let (idx, _) = v
            .iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .unwrap();
        Ok(idx)
    }

    /// Safe argmin - returns index of minimum value.
    pub fn argmin(v: &[f64]) -> Result<usize> {
        if v.is_empty() {
            return Err(Error::EmptyInput);
        }
        let (idx, _) = v
            .iter()
            .enumerate()
            .min_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .unwrap();
        Ok(idx)
    }

    /// Safe sum with overflow check for large vectors.
    pub fn sum(v: &[f64]) -> Result<f64> {
        let result: f64 = v.iter().sum();
        if !result.is_finite() {
            return Err(Error::Overflow("Vector sum overflowed to infinity".to_string()));
        }
        Ok(result)
    }

    /// Safe f32 sum.
    pub fn sum_f32(v: &[f32]) -> Result<f32> {
        let result: f32 = v.iter().sum();
        if !result.is_finite() {
            return Err(Error::Overflow("Vector sum overflowed to infinity".to_string()));
        }
        Ok(result)
    }

    /// Safe outer product.
    ///
    /// Returns a matrix where result[i][j] = a[i] * b[j]
    pub fn outer(a: &[f64], b: &[f64]) -> Vec<Vec<f64>> {
        a.iter()
            .map(|&ai| b.iter().map(|&bj| ai * bj).collect())
            .collect()
    }

    /// Safe transpose of a matrix.
    ///
    /// Returns Error if matrix is jagged (inconsistent row lengths).
    pub fn transpose(matrix: &[Vec<f64>]) -> Result<Vec<Vec<f64>>> {
        if matrix.is_empty() {
            return Ok(Vec::new());
        }

        let rows = matrix.len();
        let cols = matrix[0].len();

        // Check all rows have same length
        for (i, row) in matrix.iter().enumerate() {
            if row.len() != cols {
                return Err(Error::ValidationError(format!(
                    "Jagged matrix: row {} has {} cols, expected {}",
                    i,
                    row.len(),
                    cols
                )));
            }
        }

        let mut result = vec![vec![0.0; rows]; cols];
        for (i, row) in matrix.iter().enumerate() {
            for (j, &val) in row.iter().enumerate() {
                result[j][i] = val;
            }
        }
        Ok(result)
    }

    /// Check if two vectors have the same shape.
    pub fn same_shape<T>(a: &[T], b: &[T]) -> bool {
        a.len() == b.len()
    }

    /// Validate matrix is rectangular (all rows same length).
    pub fn is_rectangular<T>(matrix: &[Vec<T>]) -> bool {
        if matrix.is_empty() {
            return true;
        }
        let cols = matrix[0].len();
        matrix.iter().all(|row| row.len() == cols)
    }

    /// Get matrix shape (rows, cols).
    ///
    /// Returns Error for jagged matrices.
    pub fn shape<T>(matrix: &[Vec<T>]) -> Result<(usize, usize)> {
        if matrix.is_empty() {
            return Ok((0, 0));
        }
        let rows = matrix.len();
        let cols = matrix[0].len();

        if !Self::is_rectangular(matrix) {
            return Err(Error::ValidationError("Jagged matrix has no consistent shape".to_string()));
        }
        Ok((rows, cols))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dot_product() {
        let a = vec![1.0, 2.0, 3.0];
        let b = vec![4.0, 5.0, 6.0];
        assert_eq!(SafeTensor::dot(&a, &b).unwrap(), 32.0);
    }

    #[test]
    fn test_dot_product_mismatch() {
        let a = vec![1.0, 2.0, 3.0];
        let b = vec![4.0, 5.0];
        assert!(SafeTensor::dot(&a, &b).is_err());
    }

    #[test]
    fn test_add() {
        let a = vec![1.0, 2.0, 3.0];
        let b = vec![4.0, 5.0, 6.0];
        assert_eq!(SafeTensor::add(&a, &b).unwrap(), vec![5.0, 7.0, 9.0]);
    }

    #[test]
    fn test_add_mismatch() {
        let a = vec![1.0, 2.0];
        let b = vec![1.0];
        assert!(SafeTensor::add(&a, &b).is_err());
    }

    #[test]
    fn test_mat_vec() {
        let matrix = vec![vec![1.0, 2.0], vec![3.0, 4.0]];
        let vector = vec![1.0, 2.0];
        let result = SafeTensor::mat_vec(&matrix, &vector).unwrap();
        assert_eq!(result, vec![5.0, 11.0]);
    }

    #[test]
    fn test_mat_vec_mismatch() {
        let matrix = vec![vec![1.0, 2.0, 3.0], vec![4.0, 5.0, 6.0]];
        let vector = vec![1.0, 2.0]; // Wrong size
        assert!(SafeTensor::mat_vec(&matrix, &vector).is_err());
    }

    #[test]
    fn test_get_bounds() {
        let v = vec![1, 2, 3];
        assert_eq!(SafeTensor::get(&v, 0).unwrap(), 1);
        assert_eq!(SafeTensor::get(&v, 2).unwrap(), 3);
        assert!(SafeTensor::get(&v, 3).is_err());
    }

    #[test]
    fn test_argmax() {
        let v = vec![1.0, 5.0, 3.0, 2.0];
        assert_eq!(SafeTensor::argmax(&v).unwrap(), 1);
    }

    #[test]
    fn test_argmax_empty() {
        let v: Vec<f64> = vec![];
        assert!(SafeTensor::argmax(&v).is_err());
    }

    #[test]
    fn test_transpose() {
        let matrix = vec![vec![1.0, 2.0, 3.0], vec![4.0, 5.0, 6.0]];
        let transposed = SafeTensor::transpose(&matrix).unwrap();
        assert_eq!(transposed.len(), 3);
        assert_eq!(transposed[0], vec![1.0, 4.0]);
        assert_eq!(transposed[1], vec![2.0, 5.0]);
        assert_eq!(transposed[2], vec![3.0, 6.0]);
    }

    #[test]
    fn test_transpose_jagged() {
        let matrix = vec![vec![1.0, 2.0], vec![3.0, 4.0, 5.0]];
        assert!(SafeTensor::transpose(&matrix).is_err());
    }

    #[test]
    fn test_shape() {
        let matrix = vec![vec![1.0, 2.0, 3.0], vec![4.0, 5.0, 6.0]];
        assert_eq!(SafeTensor::shape(&matrix).unwrap(), (2, 3));
    }

    #[test]
    fn test_outer_product() {
        let a = vec![1.0, 2.0];
        let b = vec![3.0, 4.0, 5.0];
        let result = SafeTensor::outer(&a, &b);
        assert_eq!(result, vec![vec![3.0, 4.0, 5.0], vec![6.0, 8.0, 10.0]]);
    }
}
