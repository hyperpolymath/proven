#!/bin/sh
# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_json.sh - Safe JSON validation for POSIX shells
# Source this file: . /path/to/safe_json.sh

# Validate JSON syntax (basic structural validation)
# Usage: json_is_valid '{"key": "value"}' && echo "valid"
# Returns: 0 if valid JSON structure, 1 otherwise
json_is_valid() {
    input="$1"

    # Empty input is invalid
    [ -z "$input" ] && return 1

    # Use jq if available for proper validation
    if command -v jq >/dev/null 2>&1; then
        printf '%s' "$input" | jq -e . >/dev/null 2>&1
        return $?
    fi

    # Fallback: basic structural validation
    # Check for balanced braces and brackets
    depth_brace=0
    depth_bracket=0
    in_string=0
    escape=0
    i=0
    len=${#input}

    while [ $i -lt $len ]; do
        c=$(printf '%s' "$input" | cut -c$((i + 1)))

        if [ $escape -eq 1 ]; then
            escape=0
            i=$((i + 1))
            continue
        fi

        case "$c" in
            '\\')
                [ $in_string -eq 1 ] && escape=1
                ;;
            '"')
                if [ $in_string -eq 0 ]; then
                    in_string=1
                else
                    in_string=0
                fi
                ;;
            '{')
                [ $in_string -eq 0 ] && depth_brace=$((depth_brace + 1))
                ;;
            '}')
                [ $in_string -eq 0 ] && depth_brace=$((depth_brace - 1))
                ;;
            '[')
                [ $in_string -eq 0 ] && depth_bracket=$((depth_bracket + 1))
                ;;
            ']')
                [ $in_string -eq 0 ] && depth_bracket=$((depth_bracket - 1))
                ;;
        esac

        # Early exit on unbalanced
        [ $depth_brace -lt 0 ] && return 1
        [ $depth_bracket -lt 0 ] && return 1

        i=$((i + 1))
    done

    # Check final balance
    [ $depth_brace -eq 0 ] && [ $depth_bracket -eq 0 ] && [ $in_string -eq 0 ]
}

# Check if input is a JSON object
# Usage: json_is_object '{"key": "value"}' && echo "object"
# Returns: 0 if JSON object, 1 otherwise
json_is_object() {
    input="$1"
    trimmed=$(printf '%s' "$input" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

    case "$trimmed" in
        "{"*"}")
            json_is_valid "$input"
            return $?
            ;;
    esac
    return 1
}

# Check if input is a JSON array
# Usage: json_is_array '[1, 2, 3]' && echo "array"
# Returns: 0 if JSON array, 1 otherwise
json_is_array() {
    input="$1"
    trimmed=$(printf '%s' "$input" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

    case "$trimmed" in
        "["*"]")
            json_is_valid "$input"
            return $?
            ;;
    esac
    return 1
}

# Escape a string for JSON
# Usage: escaped=$(json_escape_string "hello \"world\"")
# Returns: Escaped string (without outer quotes)
json_escape_string() {
    input="$1"
    # Escape backslash, double quote, control characters
    printf '%s' "$input" | sed \
        -e 's/\\/\\\\/g' \
        -e 's/"/\\"/g' \
        -e 's/	/\\t/g' \
        -e 's//\\n/g' \
        -e 's//\\r/g'
}

# Create a JSON string value
# Usage: json=$(json_string "hello world")
# Returns: "hello world" (quoted)
json_string() {
    escaped=$(json_escape_string "$1")
    printf '"%s"' "$escaped"
}

# Create a JSON number value
# Usage: json=$(json_number 42)
# Returns: 42
json_number() {
    value="$1"
    # Validate it's a number
    case "$value" in
        ''|*[!0-9.-]*)
            printf 'null'
            return 1
            ;;
        *)
            printf '%s' "$value"
            return 0
            ;;
    esac
}

# Create a JSON boolean value
# Usage: json=$(json_boolean true)
# Returns: true or false
json_boolean() {
    case "$1" in
        true|1|yes|on)
            printf 'true'
            ;;
        false|0|no|off|'')
            printf 'false'
            ;;
        *)
            printf 'false'
            ;;
    esac
}

# Create a JSON null value
# Usage: json=$(json_null)
# Returns: null
json_null() {
    printf 'null'
}

# Create a simple JSON object from key-value pairs
# Usage: json=$(json_object "name" "\"John\"" "age" "30")
# Returns: {"name": "John", "age": 30}
json_object() {
    result="{"
    first=1

    while [ $# -ge 2 ]; do
        key="$1"
        value="$2"
        shift 2

        if [ $first -eq 0 ]; then
            result="${result}, "
        fi
        first=0

        escaped_key=$(json_escape_string "$key")
        result="${result}\"${escaped_key}\": ${value}"
    done

    result="${result}}"
    printf '%s' "$result"
}

# Create a JSON array from values
# Usage: json=$(json_array "\"a\"" "\"b\"" "1" "true")
# Returns: ["a", "b", 1, true]
json_array() {
    result="["
    first=1

    for value in "$@"; do
        if [ $first -eq 0 ]; then
            result="${result}, "
        fi
        first=0
        result="${result}${value}"
    done

    result="${result}]"
    printf '%s' "$result"
}

# Get a value from JSON using jq (if available)
# Usage: value=$(json_get '{"name": "John"}' '.name')
# Returns: Value or empty on error
json_get() {
    json="$1"
    path="$2"

    if command -v jq >/dev/null 2>&1; then
        printf '%s' "$json" | jq -r "$path" 2>/dev/null
        return $?
    fi

    # Without jq, we can only handle simple cases
    printf ''
    return 1
}

# Get JSON type
# Usage: type=$(json_type '{"key": "value"}')
# Returns: object, array, string, number, boolean, null, or invalid
json_type() {
    input="$1"
    trimmed=$(printf '%s' "$input" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

    case "$trimmed" in
        "{"*)
            json_is_object "$input" && printf 'object' && return 0
            ;;
        "["*)
            json_is_array "$input" && printf 'array' && return 0
            ;;
        '"'*)
            printf 'string'
            return 0
            ;;
        true|false)
            printf 'boolean'
            return 0
            ;;
        null)
            printf 'null'
            return 0
            ;;
        -[0-9]*|[0-9]*)
            printf 'number'
            return 0
            ;;
    esac

    printf 'invalid'
    return 1
}

# Pretty print JSON (if jq available)
# Usage: json_pretty '{"a":1}'
# Returns: Formatted JSON
json_pretty() {
    json="$1"

    if command -v jq >/dev/null 2>&1; then
        printf '%s' "$json" | jq '.'
        return $?
    fi

    # Fallback: just output as-is
    printf '%s' "$json"
}

# Minify JSON (if jq available)
# Usage: json_minify '{ "a": 1 }'
# Returns: Compact JSON
json_minify() {
    json="$1"

    if command -v jq >/dev/null 2>&1; then
        printf '%s' "$json" | jq -c '.'
        return $?
    fi

    # Fallback: basic whitespace removal (not perfect)
    printf '%s' "$json" | tr -d '\n\t' | sed 's/  */ /g'
}

# Merge two JSON objects (if jq available)
# Usage: merged=$(json_merge '{"a":1}' '{"b":2}')
# Returns: Merged object
json_merge() {
    json1="$1"
    json2="$2"

    if command -v jq >/dev/null 2>&1; then
        printf '%s' "$json1" | jq -s ".[0] * $json2"
        return $?
    fi

    return 1
}

# Check if JSON contains a key (requires jq)
# Usage: json_has_key '{"name": "John"}' 'name' && echo "has key"
# Returns: 0 if key exists, 1 otherwise
json_has_key() {
    json="$1"
    key="$2"

    if command -v jq >/dev/null 2>&1; then
        printf '%s' "$json" | jq -e "has(\"$key\")" >/dev/null 2>&1
        return $?
    fi

    # Basic fallback: search for key pattern
    case "$json" in
        *"\"$key\""*)
            return 0
            ;;
    esac
    return 1
}

# Count array elements (requires jq)
# Usage: count=$(json_array_length '[1,2,3]')
# Returns: Number of elements
json_array_length() {
    json="$1"

    if command -v jq >/dev/null 2>&1; then
        printf '%s' "$json" | jq 'length'
        return $?
    fi

    return 1
}
