# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeJson - JSON parsing that cannot crash.

Provides exception-free JSON parsing with type-safe value access.
"""

from typing import Optional, Any, Dict, List, Union
import json


class JsonValue:
    """Type-safe wrapper for JSON values."""

    def __init__(self, value: Any):
        self._value = value

    @property
    def value(self) -> Any:
        """Get the raw value."""
        return self._value

    def as_string(self) -> Optional[str]:
        """Get as string if value is a string."""
        return self._value if isinstance(self._value, str) else None

    def as_int(self) -> Optional[int]:
        """Get as int if value is an integer."""
        if isinstance(self._value, bool):
            return None
        return self._value if isinstance(self._value, int) else None

    def as_float(self) -> Optional[float]:
        """Get as float if value is a number."""
        if isinstance(self._value, bool):
            return None
        if isinstance(self._value, (int, float)):
            return float(self._value)
        return None

    def as_bool(self) -> Optional[bool]:
        """Get as bool if value is a boolean."""
        return self._value if isinstance(self._value, bool) else None

    def as_array(self) -> Optional[List["JsonValue"]]:
        """Get as array if value is a list."""
        if isinstance(self._value, list):
            return [JsonValue(v) for v in self._value]
        return None

    def as_object(self) -> Optional[Dict[str, "JsonValue"]]:
        """Get as object if value is a dict."""
        if isinstance(self._value, dict):
            return {k: JsonValue(v) for k, v in self._value.items()}
        return None

    def get(self, key: str) -> Optional["JsonValue"]:
        """Get a field from an object."""
        if isinstance(self._value, dict) and key in self._value:
            return JsonValue(self._value[key])
        return None

    def at(self, index: int) -> Optional["JsonValue"]:
        """Get an element from an array."""
        if isinstance(self._value, list) and 0 <= index < len(self._value):
            return JsonValue(self._value[index])
        return None

    def is_null(self) -> bool:
        """Check if value is null."""
        return self._value is None

    def __repr__(self) -> str:
        return f"JsonValue({self._value!r})"


class SafeJson:
    """Exception-free JSON parsing and manipulation."""

    @staticmethod
    def parse(text: str) -> Optional[JsonValue]:
        """
        Parse JSON string without exceptions.

        Args:
            text: JSON string to parse

        Returns:
            JsonValue wrapper, or None if parsing fails

        Example:
            >>> SafeJson.parse('{"name": "Alice"}')
            JsonValue({'name': 'Alice'})
            >>> SafeJson.parse('invalid')
            None
        """
        try:
            return JsonValue(json.loads(text))
        except (json.JSONDecodeError, TypeError):
            return None

    @staticmethod
    def stringify(value: Any, pretty: bool = False) -> Optional[str]:
        """
        Convert value to JSON string without exceptions.

        Args:
            value: Value to serialize
            pretty: Whether to format with indentation

        Returns:
            JSON string, or None if serialization fails
        """
        try:
            if pretty:
                return json.dumps(value, indent=2)
            return json.dumps(value)
        except (TypeError, ValueError):
            return None

    @staticmethod
    def get_string(text: str, path: str) -> Optional[str]:
        """Get a string value at a path (dot-separated)."""
        result = SafeJson.parse(text)
        if result is None:
            return None
        for key in path.split("."):
            result = result.get(key)
            if result is None:
                return None
        return result.as_string()

    @staticmethod
    def get_int(text: str, path: str) -> Optional[int]:
        """Get an integer value at a path (dot-separated)."""
        result = SafeJson.parse(text)
        if result is None:
            return None
        for key in path.split("."):
            result = result.get(key)
            if result is None:
                return None
        return result.as_int()

    @staticmethod
    def get_bool(text: str, path: str) -> Optional[bool]:
        """Get a boolean value at a path (dot-separated)."""
        result = SafeJson.parse(text)
        if result is None:
            return None
        for key in path.split("."):
            result = result.get(key)
            if result is None:
                return None
        return result.as_bool()
