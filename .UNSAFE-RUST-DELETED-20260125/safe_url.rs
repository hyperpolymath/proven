// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe URL parsing and manipulation.

use crate::core::{Error, Result};

/// Parsed URL components.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedUrl {
    /// URL scheme (e.g., "https")
    pub scheme: String,
    /// Optional username
    pub username: Option<String>,
    /// Optional password
    pub password: Option<String>,
    /// Host (domain or IP)
    pub host: String,
    /// Optional port
    pub port: Option<u16>,
    /// Path component
    pub path: String,
    /// Optional query string (without ?)
    pub query: Option<String>,
    /// Optional fragment (without #)
    pub fragment: Option<String>,
}

/// Safe URL operations.
pub struct SafeUrl;

impl SafeUrl {
    /// Parse a URL string.
    pub fn parse(url: &str) -> Result<ParsedUrl> {
        // Simple URL parser - production use should use a proper URL library
        let url = url.trim();
        if url.is_empty() {
            return Err(Error::EmptyInput);
        }

        // Extract scheme
        let (scheme, rest) = url
            .split_once("://")
            .ok_or_else(|| Error::InvalidFormat("Missing scheme".into()))?;

        // Extract fragment
        let (rest, fragment) = match rest.rsplit_once('#') {
            Some((r, f)) => (r, Some(f.to_string())),
            None => (rest, None),
        };

        // Extract query
        let (rest, query) = match rest.split_once('?') {
            Some((r, q)) => (r, Some(q.to_string())),
            None => (rest, None),
        };

        // Extract path
        let (authority, path) = match rest.find('/') {
            Some(i) => (&rest[..i], rest[i..].to_string()),
            None => (rest, "/".to_string()),
        };

        // Extract userinfo
        let (userinfo, hostport) = match authority.rsplit_once('@') {
            Some((u, h)) => (Some(u), h),
            None => (None, authority),
        };

        let (username, password) = match userinfo {
            Some(u) => match u.split_once(':') {
                Some((user, pass)) => (Some(user.to_string()), Some(pass.to_string())),
                None => (Some(u.to_string()), None),
            },
            None => (None, None),
        };

        // Extract port
        let (host, port) = if hostport.starts_with('[') {
            // IPv6
            match hostport.rfind(']') {
                Some(i) => {
                    let h = &hostport[1..i];
                    let p = hostport[i + 1..].strip_prefix(':').map(|s| {
                        s.parse::<u16>()
                            .map_err(|_| Error::InvalidFormat("Invalid port".into()))
                    });
                    match p {
                        Some(Ok(port)) => (h.to_string(), Some(port)),
                        Some(Err(e)) => return Err(e),
                        None => (h.to_string(), None),
                    }
                }
                None => return Err(Error::InvalidFormat("Invalid IPv6 address".into())),
            }
        } else {
            match hostport.rsplit_once(':') {
                Some((h, p)) => match p.parse::<u16>() {
                    Ok(port) => (h.to_string(), Some(port)),
                    Err(_) => (hostport.to_string(), None),
                },
                None => (hostport.to_string(), None),
            }
        };

        Ok(ParsedUrl {
            scheme: scheme.to_lowercase(),
            username,
            password,
            host,
            port,
            path,
            query,
            fragment,
        })
    }

    /// Check if a URL is valid.
    pub fn is_valid(url: &str) -> bool {
        Self::parse(url).is_ok()
    }

    /// Extract the domain from a URL.
    pub fn get_domain(url: &str) -> Result<String> {
        let parsed = Self::parse(url)?;
        Ok(parsed.host)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple() {
        let url = SafeUrl::parse("https://example.com/path").unwrap();
        assert_eq!(url.scheme, "https");
        assert_eq!(url.host, "example.com");
        assert_eq!(url.path, "/path");
    }

    #[test]
    fn test_parse_with_port() {
        let url = SafeUrl::parse("http://localhost:8080/api").unwrap();
        assert_eq!(url.host, "localhost");
        assert_eq!(url.port, Some(8080));
    }

    #[test]
    fn test_parse_with_query() {
        let url = SafeUrl::parse("https://example.com/search?q=test").unwrap();
        assert_eq!(url.query, Some("q=test".to_string()));
    }
}
