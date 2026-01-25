// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe JSON parsing and access.

use crate::core::{Error, Result};

/// Safe JSON operations.
pub struct SafeJson;

impl SafeJson {
    /// Parse JSON from a string.
    #[cfg(feature = "serde")]
    pub fn parse(s: &str) -> Result<serde_json::Value> {
        serde_json::from_str(s).map_err(|e| Error::ParseError(e.to_string()))
    }

    /// Validate JSON syntax without parsing.
    pub fn is_valid(s: &str) -> bool {
        // Simple validation: check balanced braces/brackets
        let mut depth_brace = 0i32;
        let mut depth_bracket = 0i32;
        let mut in_string = false;
        let mut escape = false;

        for c in s.chars() {
            if escape {
                escape = false;
                continue;
            }
            match c {
                '\\' if in_string => escape = true,
                '"' => in_string = !in_string,
                '{' if !in_string => depth_brace += 1,
                '}' if !in_string => depth_brace -= 1,
                '[' if !in_string => depth_bracket += 1,
                ']' if !in_string => depth_bracket -= 1,
                _ => {}
            }
            if depth_brace < 0 || depth_bracket < 0 {
                return false;
            }
        }
        depth_brace == 0 && depth_bracket == 0 && !in_string
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_valid() {
        assert!(SafeJson::is_valid(r#"{"key": "value"}"#));
        assert!(SafeJson::is_valid(r#"[1, 2, 3]"#));
        assert!(SafeJson::is_valid(r#"{"nested": {"a": 1}}"#));
        assert!(!SafeJson::is_valid(r#"{"unclosed"#));
        assert!(!SafeJson::is_valid(r#"{"extra": }}"#));
    }
}
