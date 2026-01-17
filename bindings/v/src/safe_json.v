// SPDX-License-Identifier: PMPL-1.0
// Safe JSON handling for V.
//
// Provides JSON parsing, validation, and manipulation with
// protection against malformed input and injection attacks.

module proven

import json

// JSON value types
pub enum JsonType {
	null_type
	bool_type
	number_type
	string_type
	array_type
	object_type
}

// JSON value wrapper
pub struct JsonValue {
pub:
	raw       string
	json_type JsonType
}

// Parse JSON string safely with size limit
pub fn parse_json(s string) ?JsonValue {
	if s.len == 0 {
		return none
	}

	// Size limit to prevent DoS
	if s.len > 10_000_000 {
		return none
	}

	trimmed := s.trim_space()
	if trimmed.len == 0 {
		return none
	}

	json_type := detect_json_type(trimmed)

	// Validate it parses correctly
	if !is_valid_json(trimmed) {
		return none
	}

	return JsonValue{
		raw:       trimmed
		json_type: json_type
	}
}

// Detect JSON type from string
fn detect_json_type(s string) JsonType {
	if s.len == 0 {
		return .null_type
	}

	first := s[0]
	return match first {
		`n` { JsonType.null_type }
		`t`, `f` { JsonType.bool_type }
		`"` { JsonType.string_type }
		`[` { JsonType.array_type }
		`{` { JsonType.object_type }
		`-`, `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` { JsonType.number_type }
		else { JsonType.null_type }
	}
}

// Validate JSON structure
pub fn is_valid_json(s string) bool {
	if s.len == 0 {
		return false
	}

	trimmed := s.trim_space()
	if trimmed.len == 0 {
		return false
	}

	// Try to decode as a generic map or array
	// V's json module will fail on invalid JSON
	_ := json.decode(json.Any, trimmed) or { return false }

	return true
}

// Escape a string for JSON
pub fn json_escape(s string) string {
	mut result := []u8{}

	for c in s {
		match c {
			`"` {
				result << `\\`
				result << `"`
			}
			`\\` {
				result << `\\`
				result << `\\`
			}
			`\n` {
				result << `\\`
				result << `n`
			}
			`\r` {
				result << `\\`
				result << `r`
			}
			`\t` {
				result << `\\`
				result << `t`
			}
			else {
				if c < 32 {
					// Escape control characters
					result << `\\`
					result << `u`
					result << `0`
					result << `0`
					hex := c.hex()
					if hex.len == 1 {
						result << `0`
					}
					for h in hex {
						result << h
					}
				} else {
					result << c
				}
			}
		}
	}

	return result.bytestr()
}

// Unescape a JSON string
pub fn json_unescape(s string) ?string {
	mut result := []u8{}
	mut i := 0

	for i < s.len {
		if s[i] == `\\` && i + 1 < s.len {
			match s[i + 1] {
				`"` {
					result << `"`
					i += 2
				}
				`\\` {
					result << `\\`
					i += 2
				}
				`/` {
					result << `/`
					i += 2
				}
				`b` {
					result << 8 // backspace
					i += 2
				}
				`f` {
					result << 12 // form feed
					i += 2
				}
				`n` {
					result << `\n`
					i += 2
				}
				`r` {
					result << `\r`
					i += 2
				}
				`t` {
					result << `\t`
					i += 2
				}
				`u` {
					if i + 5 < s.len {
						// Unicode escape - simplified handling
						i += 6
					} else {
						return none
					}
				}
				else {
					result << s[i]
					i += 1
				}
			}
		} else {
			result << s[i]
			i += 1
		}
	}

	return result.bytestr()
}

// Check JSON depth (prevent stack overflow)
pub fn json_depth(s string) int {
	mut depth := 0
	mut max_depth := 0
	mut in_string := false
	mut escape := false

	for c in s {
		if escape {
			escape = false
			continue
		}

		if c == `\\` && in_string {
			escape = true
			continue
		}

		if c == `"` {
			in_string = !in_string
			continue
		}

		if !in_string {
			if c == `{` || c == `[` {
				depth += 1
				if depth > max_depth {
					max_depth = depth
				}
			} else if c == `}` || c == `]` {
				depth -= 1
			}
		}
	}

	return max_depth
}

// Validate JSON depth limit
pub fn validate_json_depth(s string, max_allowed int) bool {
	return json_depth(s) <= max_allowed
}

// Extract string value from JSON
pub fn json_get_string(json_str string, key string) ?string {
	decoded := json.decode(map[string]json.Any, json_str) or { return none }

	if value := decoded[key] {
		if str := value.str() {
			return str
		}
	}

	return none
}

// Extract int value from JSON
pub fn json_get_int(json_str string, key string) ?i64 {
	decoded := json.decode(map[string]json.Any, json_str) or { return none }

	if value := decoded[key] {
		return value.i64()
	}

	return none
}

// Extract bool value from JSON
pub fn json_get_bool(json_str string, key string) ?bool {
	decoded := json.decode(map[string]json.Any, json_str) or { return none }

	if value := decoded[key] {
		return value.bool()
	}

	return none
}

// Pretty print JSON with indentation
pub fn json_pretty(s string, indent int) ?string {
	if !is_valid_json(s) {
		return none
	}

	indent_str := ' '.repeat(indent)
	mut result := []u8{}
	mut current_indent := 0
	mut in_string := false
	mut escape := false

	for c in s {
		if escape {
			result << c
			escape = false
			continue
		}

		if c == `\\` && in_string {
			result << c
			escape = true
			continue
		}

		if c == `"` {
			result << c
			in_string = !in_string
			continue
		}

		if in_string {
			result << c
			continue
		}

		match c {
			`{`, `[` {
				result << c
				current_indent += 1
				result << `\n`
				for _ in 0 .. current_indent {
					for b in indent_str {
						result << b
					}
				}
			}
			`}`, `]` {
				current_indent -= 1
				result << `\n`
				for _ in 0 .. current_indent {
					for b in indent_str {
						result << b
					}
				}
				result << c
			}
			`,` {
				result << c
				result << `\n`
				for _ in 0 .. current_indent {
					for b in indent_str {
						result << b
					}
				}
			}
			`:` {
				result << c
				result << ` `
			}
			` `, `\t`, `\n`, `\r` {
				// Skip whitespace
			}
			else {
				result << c
			}
		}
	}

	return result.bytestr()
}

// Minify JSON (remove whitespace)
pub fn json_minify(s string) ?string {
	if !is_valid_json(s) {
		return none
	}

	mut result := []u8{}
	mut in_string := false
	mut escape := false

	for c in s {
		if escape {
			result << c
			escape = false
			continue
		}

		if c == `\\` && in_string {
			result << c
			escape = true
			continue
		}

		if c == `"` {
			result << c
			in_string = !in_string
			continue
		}

		if in_string {
			result << c
			continue
		}

		// Skip whitespace outside strings
		if c != ` ` && c != `\t` && c != `\n` && c != `\r` {
			result << c
		}
	}

	return result.bytestr()
}
