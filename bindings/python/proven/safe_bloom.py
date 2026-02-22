# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeBloom - Probabilistic set membership with bloom filters.

Provides space-efficient probabilistic set membership testing.
All operations are delegated to the Idris core via FFI using opaque handles.
"""

from typing import Union

from .core import ProvenStatus, get_lib


class BloomFilter:
    """
    Bloom filter for probabilistic set membership via FFI.

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
        Create a bloom filter via FFI.

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

        lib = get_lib()
        result = lib.proven_bloom_create(expected_items, false_positive_rate)
        if result.status != ProvenStatus.OK or result.handle is None:
            raise RuntimeError("Failed to create bloom filter via FFI")

        self._handle = result.handle
        self._expected_items = expected_items
        self._false_positive_rate = false_positive_rate
        self._count = 0
        self._lib = lib

    def __del__(self):
        """Free the FFI bloom filter handle."""
        if hasattr(self, "_handle") and self._handle is not None:
            try:
                self._lib.proven_bloom_free(self._handle)
            except Exception:
                pass

    @property
    def count(self) -> int:
        """Get number of items inserted."""
        return self._count

    def insert(self, item: Union[str, bytes]) -> None:
        """
        Insert an item into the filter via FFI.

        Args:
            item: Item to insert
        """
        if isinstance(item, str):
            item = item.encode("utf-8")
        status = self._lib.proven_bloom_insert(self._handle, item, len(item))
        if status == ProvenStatus.OK:
            self._count += 1

    def contains(self, item: Union[str, bytes]) -> bool:
        """
        Check if item might be in the filter via FFI.

        Args:
            item: Item to check

        Returns:
            True if possibly in set, False if definitely not
        """
        if isinstance(item, str):
            item = item.encode("utf-8")
        result = self._lib.proven_bloom_contains(self._handle, item, len(item))
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    def __contains__(self, item: Union[str, bytes]) -> bool:
        """Support 'in' operator."""
        return self.contains(item)

    def clear(self) -> None:
        """Reset the filter by recreating via FFI."""
        self._lib.proven_bloom_free(self._handle)
        result = self._lib.proven_bloom_create(self._expected_items, self._false_positive_rate)
        if result.status == ProvenStatus.OK and result.handle is not None:
            self._handle = result.handle
        self._count = 0
