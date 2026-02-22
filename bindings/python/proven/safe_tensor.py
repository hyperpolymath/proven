# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeTensor - Bounds-checked vector/matrix operations.

Provides safe tensor operations with shape validation.
All computation is delegated to the Idris core via FFI.
"""

import ctypes
from typing import List, Optional, Tuple

from .core import ProvenStatus, get_lib, check_status


class ShapeError(Exception):
    """Raised when tensor shapes are incompatible."""
    pass


def _to_c_array(data: List[float]) -> ctypes.Array:
    """Convert Python list to ctypes double array."""
    arr = (ctypes.c_double * len(data))()
    for i, v in enumerate(data):
        arr[i] = v
    return arr


def _from_c_array(arr: ctypes.Array, size: int) -> List[float]:
    """Convert ctypes double array to Python list."""
    return [arr[i] for i in range(size)]


class Vector:
    """
    Bounds-checked vector operations via FFI.

    Example:
        >>> v = Vector([1.0, 2.0, 3.0])
        >>> v.dot(Vector([4.0, 5.0, 6.0]))
        32.0
    """

    def __init__(self, data: List[float]):
        """
        Create a vector.

        Args:
            data: Vector elements
        """
        self._data = list(data)

    @property
    def size(self) -> int:
        """Get vector length."""
        return len(self._data)

    def __len__(self) -> int:
        return len(self._data)

    def __getitem__(self, index: int) -> Optional[float]:
        """Safe element access."""
        if 0 <= index < len(self._data):
            return self._data[index]
        return None

    def get(self, index: int) -> Optional[float]:
        """Safe element access."""
        return self[index]

    def set(self, index: int, value: float) -> bool:
        """
        Safe element update.

        Args:
            index: Index
            value: New value

        Returns:
            True if successful
        """
        if 0 <= index < len(self._data):
            self._data[index] = value
            return True
        return False

    def to_list(self) -> List[float]:
        """Get copy of data as list."""
        return list(self._data)

    def dot(self, other: "Vector") -> Optional[float]:
        """
        Dot product via FFI.

        Args:
            other: Other vector

        Returns:
            Dot product, or None if sizes don't match
        """
        if len(self._data) != len(other._data):
            return None
        lib = get_lib()
        a = _to_c_array(self._data)
        b = _to_c_array(other._data)
        result = lib.proven_tensor_dot(a, len(self._data), b, len(other._data))
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    def magnitude(self) -> float:
        """Calculate vector magnitude (L2 norm) via FFI."""
        lib = get_lib()
        a = _to_c_array(self._data)
        result = lib.proven_tensor_magnitude(a, len(self._data))
        if result.status != ProvenStatus.OK:
            return 0.0
        return result.value

    def normalize(self) -> Optional["Vector"]:
        """
        Return normalized vector via FFI.

        Returns:
            Unit vector, or None if zero vector
        """
        lib = get_lib()
        a = _to_c_array(self._data)
        out = (ctypes.c_double * len(self._data))()
        status = lib.proven_tensor_normalize(a, len(self._data), out)
        if status != ProvenStatus.OK:
            return None
        return Vector(_from_c_array(out, len(self._data)))

    def add(self, other: "Vector") -> Optional["Vector"]:
        """Element-wise addition via FFI."""
        if len(self._data) != len(other._data):
            return None
        lib = get_lib()
        a = _to_c_array(self._data)
        b = _to_c_array(other._data)
        out = (ctypes.c_double * len(self._data))()
        status = lib.proven_tensor_add(a, len(self._data), b, len(other._data), out)
        if status != ProvenStatus.OK:
            return None
        return Vector(_from_c_array(out, len(self._data)))

    def sub(self, other: "Vector") -> Optional["Vector"]:
        """Element-wise subtraction via FFI."""
        if len(self._data) != len(other._data):
            return None
        lib = get_lib()
        a = _to_c_array(self._data)
        b = _to_c_array(other._data)
        out = (ctypes.c_double * len(self._data))()
        status = lib.proven_tensor_sub(a, len(self._data), b, len(other._data), out)
        if status != ProvenStatus.OK:
            return None
        return Vector(_from_c_array(out, len(self._data)))

    def mul(self, scalar: float) -> "Vector":
        """Scalar multiplication via FFI."""
        lib = get_lib()
        a = _to_c_array(self._data)
        out = (ctypes.c_double * len(self._data))()
        status = lib.proven_tensor_scale(a, len(self._data), scalar, out)
        if status != ProvenStatus.OK:
            return Vector(list(self._data))
        return Vector(_from_c_array(out, len(self._data)))

    def __add__(self, other: "Vector") -> "Vector":
        result = self.add(other)
        if result is None:
            raise ShapeError("Vector sizes don't match")
        return result

    def __sub__(self, other: "Vector") -> "Vector":
        result = self.sub(other)
        if result is None:
            raise ShapeError("Vector sizes don't match")
        return result

    def __mul__(self, scalar: float) -> "Vector":
        return self.mul(scalar)

    def __repr__(self) -> str:
        return f"Vector({self._data})"


class Matrix:
    """
    Bounds-checked matrix operations via FFI.

    Example:
        >>> m = Matrix([[1, 2], [3, 4]])
        >>> m.shape
        (2, 2)
    """

    def __init__(self, data: List[List[float]]):
        """
        Create a matrix.

        Args:
            data: 2D list of elements (row-major)

        Raises:
            ValueError: If rows have different lengths
        """
        if not data or not data[0]:
            self._data = [[]]
            self._rows = 0
            self._cols = 0
            return

        cols = len(data[0])
        for row in data:
            if len(row) != cols:
                raise ValueError("All rows must have same length")

        self._data = [list(row) for row in data]
        self._rows = len(data)
        self._cols = cols

    def _to_flat(self) -> List[float]:
        """Flatten matrix to row-major array."""
        flat = []
        for row in self._data:
            flat.extend(row)
        return flat

    @staticmethod
    def _from_flat(flat: List[float], rows: int, cols: int) -> "Matrix":
        """Create matrix from flat row-major array."""
        data = []
        for r in range(rows):
            data.append(flat[r * cols:(r + 1) * cols])
        return Matrix(data)

    @property
    def shape(self) -> Tuple[int, int]:
        """Get (rows, cols)."""
        return (self._rows, self._cols)

    @property
    def rows(self) -> int:
        """Get number of rows."""
        return self._rows

    @property
    def cols(self) -> int:
        """Get number of columns."""
        return self._cols

    def get(self, row: int, col: int) -> Optional[float]:
        """
        Safe element access.

        Args:
            row: Row index
            col: Column index

        Returns:
            Element, or None if out of bounds
        """
        if 0 <= row < self._rows and 0 <= col < self._cols:
            return self._data[row][col]
        return None

    def set(self, row: int, col: int, value: float) -> bool:
        """
        Safe element update.

        Args:
            row: Row index
            col: Column index
            value: New value

        Returns:
            True if successful
        """
        if 0 <= row < self._rows and 0 <= col < self._cols:
            self._data[row][col] = value
            return True
        return False

    def get_row(self, row: int) -> Optional[Vector]:
        """Get a row as vector."""
        if 0 <= row < self._rows:
            return Vector(self._data[row])
        return None

    def get_col(self, col: int) -> Optional[Vector]:
        """Get a column as vector."""
        if 0 <= col < self._cols:
            return Vector([self._data[r][col] for r in range(self._rows)])
        return None

    def to_list(self) -> List[List[float]]:
        """Get copy of data as 2D list."""
        return [list(row) for row in self._data]

    def transpose(self) -> "Matrix":
        """Return transposed matrix."""
        transposed = [[self._data[r][c] for r in range(self._rows)] for c in range(self._cols)]
        return Matrix(transposed)

    def add(self, other: "Matrix") -> Optional["Matrix"]:
        """Element-wise addition via FFI."""
        if self.shape != other.shape:
            return None
        lib = get_lib()
        a = _to_c_array(self._to_flat())
        b = _to_c_array(other._to_flat())
        total = self._rows * self._cols
        out = (ctypes.c_double * total)()
        status = lib.proven_tensor_add(a, total, b, total, out)
        if status != ProvenStatus.OK:
            return None
        return Matrix._from_flat(_from_c_array(out, total), self._rows, self._cols)

    def sub(self, other: "Matrix") -> Optional["Matrix"]:
        """Element-wise subtraction via FFI."""
        if self.shape != other.shape:
            return None
        lib = get_lib()
        a = _to_c_array(self._to_flat())
        b = _to_c_array(other._to_flat())
        total = self._rows * self._cols
        out = (ctypes.c_double * total)()
        status = lib.proven_tensor_sub(a, total, b, total, out)
        if status != ProvenStatus.OK:
            return None
        return Matrix._from_flat(_from_c_array(out, total), self._rows, self._cols)

    def mul_scalar(self, scalar: float) -> "Matrix":
        """Scalar multiplication via FFI."""
        lib = get_lib()
        a = _to_c_array(self._to_flat())
        total = self._rows * self._cols
        out = (ctypes.c_double * total)()
        status = lib.proven_tensor_scale(a, total, scalar, out)
        if status != ProvenStatus.OK:
            return Matrix([list(row) for row in self._data])
        return Matrix._from_flat(_from_c_array(out, total), self._rows, self._cols)

    def matmul(self, other: "Matrix") -> Optional["Matrix"]:
        """
        Matrix multiplication via FFI.

        Args:
            other: Right-hand matrix

        Returns:
            Result, or None if shapes incompatible
        """
        if self._cols != other._rows:
            return None
        lib = get_lib()
        a = _to_c_array(self._to_flat())
        b = _to_c_array(other._to_flat())
        out_total = self._rows * other._cols
        out = (ctypes.c_double * out_total)()
        status = lib.proven_tensor_matmul(
            a, self._rows, self._cols,
            b, other._rows, other._cols,
            out,
        )
        if status != ProvenStatus.OK:
            return None
        return Matrix._from_flat(_from_c_array(out, out_total), self._rows, other._cols)

    def mul_vector(self, v: Vector) -> Optional[Vector]:
        """
        Matrix-vector multiplication via FFI.

        Args:
            v: Vector to multiply

        Returns:
            Result vector, or None if sizes incompatible
        """
        if self._cols != v.size:
            return None
        # Treat vector as Nx1 matrix
        v_mat = Matrix([[x] for x in v.to_list()])
        result = self.matmul(v_mat)
        if result is None:
            return None
        return Vector([result.get(r, 0) for r in range(result.rows)])

    @staticmethod
    def identity(n: int) -> "Matrix":
        """Create n x n identity matrix."""
        data = [[1.0 if i == j else 0.0 for j in range(n)] for i in range(n)]
        return Matrix(data)

    @staticmethod
    def zeros(rows: int, cols: int) -> "Matrix":
        """Create matrix of zeros."""
        data = [[0.0 for _ in range(cols)] for _ in range(rows)]
        return Matrix(data)

    def __repr__(self) -> str:
        return f"Matrix({self._data})"


class SafeTensor:
    """Safe tensor utilities via FFI."""

    @staticmethod
    def vector(data: List[float]) -> Vector:
        """Create a vector."""
        return Vector(data)

    @staticmethod
    def matrix(data: List[List[float]]) -> Matrix:
        """Create a matrix."""
        return Matrix(data)

    @staticmethod
    def zeros_vector(size: int) -> Vector:
        """Create zero vector."""
        return Vector([0.0] * size)

    @staticmethod
    def ones_vector(size: int) -> Vector:
        """Create ones vector."""
        return Vector([1.0] * size)

    @staticmethod
    def dot(a: Vector, b: Vector) -> Optional[float]:
        """Dot product."""
        return a.dot(b)

    @staticmethod
    def matmul(a: Matrix, b: Matrix) -> Optional[Matrix]:
        """Matrix multiplication."""
        return a.matmul(b)
