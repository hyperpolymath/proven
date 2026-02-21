// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"encoding/json"
	"strconv"
	"strings"
)

// JsonValue represents a parsed JSON value with type safety.
type JsonValue struct {
	raw   any
	isSet bool
}

// NewJsonValue creates a JsonValue from any value.
func NewJsonValue(v any) JsonValue {
	return JsonValue{raw: v, isSet: true}
}

// IsNull checks if value is null.
func (j JsonValue) IsNull() bool {
	return j.isSet && j.raw == nil
}

// IsBool checks if value is a boolean.
func (j JsonValue) IsBool() bool {
	_, ok := j.raw.(bool)
	return ok
}

// IsNumber checks if value is a number.
func (j JsonValue) IsNumber() bool {
	_, ok := j.raw.(float64)
	return ok
}

// IsString checks if value is a string.
func (j JsonValue) IsString() bool {
	_, ok := j.raw.(string)
	return ok
}

// IsArray checks if value is an array.
func (j JsonValue) IsArray() bool {
	_, ok := j.raw.([]any)
	return ok
}

// IsObject checks if value is an object.
func (j JsonValue) IsObject() bool {
	_, ok := j.raw.(map[string]any)
	return ok
}

// AsBool returns value as bool.
func (j JsonValue) AsBool() (bool, bool) {
	v, ok := j.raw.(bool)
	return v, ok
}

// AsNumber returns value as float64.
func (j JsonValue) AsNumber() (float64, bool) {
	v, ok := j.raw.(float64)
	return v, ok
}

// AsInt returns value as int64.
func (j JsonValue) AsInt() (int64, bool) {
	v, ok := j.raw.(float64)
	if !ok {
		return 0, false
	}
	return int64(v), true
}

// AsString returns value as string.
func (j JsonValue) AsString() (string, bool) {
	v, ok := j.raw.(string)
	return v, ok
}

// AsArray returns value as array.
func (j JsonValue) AsArray() ([]JsonValue, bool) {
	arr, ok := j.raw.([]any)
	if !ok {
		return nil, false
	}
	result := make([]JsonValue, len(arr))
	for i, v := range arr {
		result[i] = NewJsonValue(v)
	}
	return result, true
}

// AsObject returns value as object.
func (j JsonValue) AsObject() (map[string]JsonValue, bool) {
	obj, ok := j.raw.(map[string]any)
	if !ok {
		return nil, false
	}
	result := make(map[string]JsonValue, len(obj))
	for k, v := range obj {
		result[k] = NewJsonValue(v)
	}
	return result, true
}

// Get retrieves a value at path (dot-separated).
func (j JsonValue) Get(path string) (JsonValue, bool) {
	if path == "" {
		return j, true
	}

	parts := strings.Split(path, ".")
	current := j

	for _, part := range parts {
		// Check if array index
		if idx, err := strconv.Atoi(part); err == nil {
			arr, ok := current.AsArray()
			if !ok || idx < 0 || idx >= len(arr) {
				return JsonValue{}, false
			}
			current = arr[idx]
		} else {
			obj, ok := current.AsObject()
			if !ok {
				return JsonValue{}, false
			}
			val, exists := obj[part]
			if !exists {
				return JsonValue{}, false
			}
			current = val
		}
	}

	return current, true
}

// Raw returns the underlying value.
func (j JsonValue) Raw() any {
	return j.raw
}

// ParseJson safely parses a JSON string.
func ParseJson(input string) (JsonValue, error) {
	var result any
	if err := json.Unmarshal([]byte(input), &result); err != nil {
		return JsonValue{}, err
	}
	return NewJsonValue(result), nil
}

// SafeParseJson parses JSON returning ok status.
func SafeParseJson(input string) (JsonValue, bool) {
	result, err := ParseJson(input)
	return result, err == nil
}

// ToJson converts a value to JSON string.
func ToJson(v any) (string, error) {
	bytes, err := json.Marshal(v)
	if err != nil {
		return "", err
	}
	return string(bytes), nil
}

// ToPrettyJson converts a value to indented JSON string.
func ToPrettyJson(v any) (string, error) {
	bytes, err := json.MarshalIndent(v, "", "  ")
	if err != nil {
		return "", err
	}
	return string(bytes), nil
}
