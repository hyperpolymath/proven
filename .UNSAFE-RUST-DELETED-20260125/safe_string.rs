// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe string operations with injection prevention.

use crate::core::{Error, Result};

/// Safe string operations with injection prevention.
pub struct SafeString;

impl SafeString {
    /// Escape HTML special characters.
    ///
    /// # Example
    /// ```
    /// use proven::SafeString;
    ///
    /// let escaped = SafeString::escape_html("<script>alert('xss')</script>");
    /// assert_eq!(escaped, "&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;");
    /// ```
    pub fn escape_html(s: &str) -> String {
        let mut result = String::with_capacity(s.len() * 2);
        for c in s.chars() {
            match c {
                '<' => result.push_str("&lt;"),
                '>' => result.push_str("&gt;"),
                '&' => result.push_str("&amp;"),
                '"' => result.push_str("&quot;"),
                '\'' => result.push_str("&#x27;"),
                _ => result.push(c),
            }
        }
        result
    }

    /// Escape SQL single quotes.
    ///
    /// # Example
    /// ```
    /// use proven::SafeString;
    ///
    /// let escaped = SafeString::escape_sql("O'Brien");
    /// assert_eq!(escaped, "O''Brien");
    /// ```
    pub fn escape_sql(s: &str) -> String {
        s.replace('\'', "''")
    }

    /// Escape JavaScript special characters.
    pub fn escape_js(s: &str) -> String {
        let mut result = String::with_capacity(s.len() * 2);
        for c in s.chars() {
            match c {
                '\\' => result.push_str("\\\\"),
                '\'' => result.push_str("\\'"),
                '"' => result.push_str("\\\""),
                '\n' => result.push_str("\\n"),
                '\r' => result.push_str("\\r"),
                '\t' => result.push_str("\\t"),
                _ => result.push(c),
            }
        }
        result
    }

    /// URL encode a string.
    pub fn url_encode(s: &str) -> String {
        const UNRESERVED: &str =
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~";
        let mut result = String::with_capacity(s.len() * 3);
        for b in s.bytes() {
            if UNRESERVED.contains(b as char) {
                result.push(b as char);
            } else {
                result.push_str(&format!("%{:02X}", b));
            }
        }
        result
    }

    /// URL decode a string.
    pub fn url_decode(s: &str) -> Result<String> {
        let mut result = Vec::with_capacity(s.len());
        let mut chars = s.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '%' {
                let hex: String = chars.by_ref().take(2).collect();
                if hex.len() != 2 {
                    return Err(Error::InvalidFormat("Incomplete percent encoding".into()));
                }
                let byte = u8::from_str_radix(&hex, 16)
                    .map_err(|_| Error::InvalidFormat("Invalid hex in percent encoding".into()))?;
                result.push(byte);
            } else if c == '+' {
                result.push(b' ');
            } else {
                result.push(c as u8);
            }
        }

        String::from_utf8(result).map_err(|e| Error::InvalidUtf8(e.to_string()))
    }

    /// Escape shell special characters (wraps in single quotes).
    pub fn escape_shell(s: &str) -> String {
        let mut result = String::with_capacity(s.len() + 4);
        result.push('\'');
        for c in s.chars() {
            if c == '\'' {
                result.push_str("'\\''");
            } else {
                result.push(c);
            }
        }
        result.push('\'');
        result
    }

    /// Check if a string is valid UTF-8 (always true for Rust strings).
    pub fn is_valid_utf8(s: &str) -> bool {
        // Rust strings are always valid UTF-8
        true
    }

    /// Check if a string is valid UTF-8 from bytes.
    pub fn validate_utf8(bytes: &[u8]) -> Result<&str> {
        std::str::from_utf8(bytes).map_err(|e| Error::InvalidUtf8(e.to_string()))
    }

    /// Truncate a string to a maximum byte length, respecting UTF-8 boundaries.
    pub fn truncate_utf8(s: &str, max_bytes: usize) -> &str {
        if s.len() <= max_bytes {
            return s;
        }

        let mut boundary = max_bytes;
        while boundary > 0 && !s.is_char_boundary(boundary) {
            boundary -= 1;
        }
        &s[..boundary]
    }

    /// Check if a string contains potential injection patterns.
    pub fn detect_injection(s: &str) -> bool {
        let patterns = [
            "<script", "javascript:", "onerror=", "onclick=", "onload=", "eval(", "expression(",
            "vbscript:", "data:", "SELECT ", "INSERT ", "UPDATE ", "DELETE ", "DROP ", "UNION ",
            "--", "/*", "*/", "; ", "' OR ", "\" OR ",
        ];

        let lower = s.to_lowercase();
        patterns.iter().any(|p| lower.contains(&p.to_lowercase()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_html_escape() {
        assert_eq!(SafeString::escape_html("<script>"), "&lt;script&gt;");
        assert_eq!(SafeString::escape_html("a & b"), "a &amp; b");
        assert_eq!(SafeString::escape_html("\"quoted\""), "&quot;quoted&quot;");
    }

    #[test]
    fn test_sql_escape() {
        assert_eq!(SafeString::escape_sql("O'Brien"), "O''Brien");
        assert_eq!(SafeString::escape_sql("no quotes"), "no quotes");
    }

    #[test]
    fn test_url_encode() {
        assert_eq!(SafeString::url_encode("hello world"), "hello%20world");
        assert_eq!(SafeString::url_encode("a=1&b=2"), "a%3D1%26b%3D2");
    }

    #[test]
    fn test_url_decode() {
        assert_eq!(SafeString::url_decode("hello%20world").unwrap(), "hello world");
        assert_eq!(SafeString::url_decode("a+b").unwrap(), "a b");
    }

    #[test]
    fn test_shell_escape() {
        assert_eq!(SafeString::escape_shell("hello"), "'hello'");
        assert_eq!(SafeString::escape_shell("it's"), "'it'\\''s'");
    }

    #[test]
    fn test_truncate_utf8() {
        let s = "hello";
        assert_eq!(SafeString::truncate_utf8(s, 3), "hel");
        assert_eq!(SafeString::truncate_utf8(s, 10), "hello");

        // Test with multi-byte chars
        let s = "日本語";
        let truncated = SafeString::truncate_utf8(s, 4);
        assert!(truncated.len() <= 4);
    }

    #[test]
    fn test_detect_injection() {
        assert!(SafeString::detect_injection("<script>alert(1)</script>"));
        assert!(SafeString::detect_injection("'; DROP TABLE users; --"));
        assert!(!SafeString::detect_injection("hello world"));
    }
}
