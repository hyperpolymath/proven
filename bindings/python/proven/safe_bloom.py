# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeBloom - Probabilistic set membership with bloom filters.

Provides space-efficient probabilistic set membership testing.
"""

from typing import Union
import math


class BloomFilter:
    """
    Bloom filter for probabilistic set membership.

    A bloom filter can tell you:
    - Definitely NOT in the set (no false negatives)
    - POSSIBLY in the set (may have false positives)

    Example:
        >>> bf = BloomFilter(1000, 0.01)  # 1000 items, 1% false positive rate
        >>> bf.insert("hello")
        >>> bf.contains("hello")
        True
        >>> bf.contains("world")  # Probably False
        False
    """

    def __init__(self, expected_items: int, false_positive_rate: float = 0.01):
        """
        Create a bloom filter.

        Args:
            expected_items: Expected number of items to insert
            false_positive_rate: Desired false positive rate (0-1)

        Raises:
            ValueError: If parameters are invalid
        """
        if expected_items <= 0:
            raise ValueError("Expected items must be positive")
        if not 0 < false_positive_rate < 1:
            raise ValueError("False positive rate must be between 0 and 1")

        # Calculate optimal size and hash count
        self._size = BloomFilter.optimal_size(expected_items, false_positive_rate)
        self._hash_count = BloomFilter.optimal_hashes(self._size, expected_items)
        self._bits = [False] * self._size
        self._count = 0

    @property
    def size(self) -> int:
        """Get bit array size."""
        return self._size

    @property
    def hash_count(self) -> int:
        """Get number of hash functions."""
        return self._hash_count

    @property
    def count(self) -> int:
        """Get number of items inserted."""
        return self._count

    @staticmethod
    def optimal_size(n: int, p: float) -> int:
        """
        Calculate optimal bit array size.

        Args:
            n: Expected number of items
            p: Desired false positive rate

        Returns:
            Optimal size in bits
        """
        return int(-(n * math.log(p)) / (math.log(2) ** 2))

    @staticmethod
    def optimal_hashes(m: int, n: int) -> int:
        """
        Calculate optimal number of hash functions.

        Args:
            m: Bit array size
            n: Expected number of items

        Returns:
            Optimal number of hash functions
        """
        return max(1, int((m / n) * math.log(2)))

    def _hashes(self, item: Union[str, bytes]) -> list:
        """Generate hash indices for an item."""
        if isinstance(item, str):
            item = item.encode("utf-8")

        # Use FNV-1a and a variation for independent hashes
        indices = []

        # FNV-1a hash
        h1 = 0xcbf29ce484222325
        for byte in item:
            h1 ^= byte
            h1 = (h1 * 0x100000001b3) & 0xffffffffffffffff

        # Second hash (different FNV offset)
        h2 = 0x84222325cbf29ce4
        for byte in item:
            h2 ^= byte
            h2 = (h2 * 0x100000001b3) & 0xffffffffffffffff

        # Generate k hashes using double hashing
        for i in range(self._hash_count):
            combined = (h1 + i * h2) % self._size
            indices.append(combined)

        return indices

    def insert(self, item: Union[str, bytes]) -> None:
        """
        Insert an item into the filter.

        Args:
            item: Item to insert
        """
        for idx in self._hashes(item):
            self._bits[idx] = True
        self._count += 1

    def contains(self, item: Union[str, bytes]) -> bool:
        """
        Check if item might be in the filter.

        Args:
            item: Item to check

        Returns:
            True if possibly in set, False if definitely not
        """
        return all(self._bits[idx] for idx in self._hashes(item))

    def __contains__(self, item: Union[str, bytes]) -> bool:
        """Support 'in' operator."""
        return self.contains(item)

    def false_positive_rate(self) -> float:
        """
        Estimate current false positive rate.

        Returns:
            Estimated false positive probability
        """
        if self._count == 0:
            return 0.0
        # (1 - e^(-k*n/m))^k
        exponent = -self._hash_count * self._count / self._size
        return (1 - math.exp(exponent)) ** self._hash_count

    def union(self, other: "BloomFilter") -> "BloomFilter":
        """
        Create union of two bloom filters.

        Args:
            other: Another bloom filter with same parameters

        Returns:
            New bloom filter containing both

        Raises:
            ValueError: If filters have different parameters
        """
        if self._size != other._size or self._hash_count != other._hash_count:
            raise ValueError("Bloom filters must have same parameters")

        result = BloomFilter.__new__(BloomFilter)
        result._size = self._size
        result._hash_count = self._hash_count
        result._bits = [a or b for a, b in zip(self._bits, other._bits)]
        result._count = self._count + other._count
        return result

    def clear(self) -> None:
        """Reset the filter."""
        self._bits = [False] * self._size
        self._count = 0
