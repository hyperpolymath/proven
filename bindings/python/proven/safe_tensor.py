# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeTensor - Bounds-checked vector/matrix operations.

Provides safe tensor operations with shape validation.
"""

from typing import List, Optional, Tuple, Union
import math


class ShapeError(Exception):
    """Raised when tensor shapes are incompatible."""
    pass


class Vector:
    """
    Bounds-checked vector operations.

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
        Dot product.

        Args:
            other: Other vector

        Returns:
            Dot product, or None if sizes don't match
        """
        if len(self._data) != len(other._data):
            return None
        return sum(a * b for a, b in zip(self._data, other._data))

    def magnitude(self) -> float:
        """Calculate vector magnitude (L2 norm)."""
        return math.sqrt(sum(x * x for x in self._data))

    def normalize(self) -> Optional["Vector"]:
        """
        Return normalized vector.

        Returns:
            Unit vector, or None if zero vector
        """
        mag = self.magnitude()
        if mag == 0:
            return None
        return Vector([x / mag for x in self._data])

    def add(self, other: "Vector") -> Optional["Vector"]:
        """Element-wise addition."""
        if len(self._data) != len(other._data):
            return None
        return Vector([a + b for a, b in zip(self._data, other._data)])

    def sub(self, other: "Vector") -> Optional["Vector"]:
        """Element-wise subtraction."""
        if len(self._data) != len(other._data):
            return None
        return Vector([a - b for a, b in zip(self._data, other._data)])

    def mul(self, scalar: float) -> "Vector":
        """Scalar multiplication."""
        return Vector([x * scalar for x in self._data])

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
    Bounds-checked matrix operations.

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
        """Element-wise addition."""
        if self.shape != other.shape:
            return None
        result = [[self._data[r][c] + other._data[r][c] for c in range(self._cols)] for r in range(self._rows)]
        return Matrix(result)

    def sub(self, other: "Matrix") -> Optional["Matrix"]:
        """Element-wise subtraction."""
        if self.shape != other.shape:
            return None
        result = [[self._data[r][c] - other._data[r][c] for c in range(self._cols)] for r in range(self._rows)]
        return Matrix(result)

    def mul_scalar(self, scalar: float) -> "Matrix":
        """Scalar multiplication."""
        result = [[self._data[r][c] * scalar for c in range(self._cols)] for r in range(self._rows)]
        return Matrix(result)

    def matmul(self, other: "Matrix") -> Optional["Matrix"]:
        """
        Matrix multiplication.

        Args:
            other: Right-hand matrix

        Returns:
            Result, or None if shapes incompatible
        """
        if self._cols != other._rows:
            return None

        result = []
        for i in range(self._rows):
            row = []
            for j in range(other._cols):
                val = sum(self._data[i][k] * other._data[k][j] for k in range(self._cols))
                row.append(val)
            result.append(row)

        return Matrix(result)

    def mul_vector(self, v: Vector) -> Optional[Vector]:
        """
        Matrix-vector multiplication.

        Args:
            v: Vector to multiply

        Returns:
            Result vector, or None if sizes incompatible
        """
        if self._cols != v.size:
            return None

        result = []
        for i in range(self._rows):
            val = sum(self._data[i][j] * v._data[j] for j in range(self._cols))
            result.append(val)

        return Vector(result)

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
    """Safe tensor utilities."""

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
