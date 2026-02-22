# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeJson - JSON parsing that cannot crash.

Provides exception-free JSON parsing with type-safe value access.
All computation is delegated to the formally verified Idris core via FFI.
"""

import json
from typing import Optional, Any, Dict, List, Union

from .core import ProvenStatus, ProvenError, get_lib, check_status


class JsonValue:
    """Type-safe wrapper for JSON values returned from FFI."""

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
    """Exception-free JSON parsing and manipulation via FFI."""

    @staticmethod
    def parse(text: str) -> Optional[JsonValue]:
        """
        Parse JSON string without exceptions.

        Delegates to the Idris core for validation, then deserializes.

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
        lib = get_lib()
        encoded = text.encode("utf-8")
        result = lib.proven_json_parse(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            return None
        if result.value is None:
            return None
        parsed_str = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        # The FFI returns validated JSON; deserialize it
        return JsonValue(json.loads(parsed_str))

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
        lib = get_lib()
        # Serialize to JSON first, then validate via FFI
        try:
            if pretty:
                raw = json.dumps(value, indent=2)
            else:
                raw = json.dumps(value)
        except (TypeError, ValueError):
            return None

        encoded = raw.encode("utf-8")
        result = lib.proven_json_stringify(encoded, len(encoded), pretty)
        if result.status != ProvenStatus.OK:
            return None
        if result.value is None:
            return None
        output = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        return output

    @staticmethod
    def get_string(text: str, path: str) -> Optional[str]:
        """Get a string value at a path (dot-separated)."""
        lib = get_lib()
        encoded_text = text.encode("utf-8")
        encoded_path = path.encode("utf-8")
        result = lib.proven_json_get_string(encoded_text, len(encoded_text),
                                            encoded_path, len(encoded_path))
        if result.status != ProvenStatus.OK:
            return None
        if result.value is None:
            return None
        output = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        return output

    @staticmethod
    def get_int(text: str, path: str) -> Optional[int]:
        """Get an integer value at a path (dot-separated)."""
        lib = get_lib()
        encoded_text = text.encode("utf-8")
        encoded_path = path.encode("utf-8")
        result = lib.proven_json_get_int(encoded_text, len(encoded_text),
                                         encoded_path, len(encoded_path))
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def get_bool(text: str, path: str) -> Optional[bool]:
        """Get a boolean value at a path (dot-separated)."""
        lib = get_lib()
        encoded_text = text.encode("utf-8")
        encoded_path = path.encode("utf-8")
        result = lib.proven_json_get_bool(encoded_text, len(encoded_text),
                                          encoded_path, len(encoded_path))
        if result.status != ProvenStatus.OK:
            return None
        return result.value
