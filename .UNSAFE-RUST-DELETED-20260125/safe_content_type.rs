// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe Content-Type operations that prevent MIME sniffing attacks.
//!
//! All operations handle MIME sniffing prevention and validate
//! media types without panicking.

use crate::core::{Error, Result};

/// Media type category
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MediaCategory {
    /// text/*
    Text,
    /// image/*
    Image,
    /// audio/*
    Audio,
    /// video/*
    Video,
    /// application/*
    Application,
    /// multipart/*
    Multipart,
    /// message/*
    Message,
    /// font/*
    Font,
    /// model/*
    Model,
    /// custom or unknown type
    Custom,
}

/// Charset encoding
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Charset {
    /// UTF-8
    Utf8,
    /// UTF-16 Little Endian
    Utf16Le,
    /// UTF-16 Big Endian
    Utf16Be,
    /// ISO-8859-1 (Latin-1)
    Iso8859_1,
    /// US-ASCII
    Ascii,
    /// Windows-1252
    Windows1252,
    /// Other charset
    Other(String),
}

/// Media type
#[derive(Debug, Clone)]
pub struct MediaType {
    /// Top-level type (e.g., "application")
    pub media_type: String,
    /// Subtype (e.g., "json")
    pub subtype: String,
    /// Suffix (e.g., "json" from "vnd.api+json")
    pub suffix: Option<String>,
    /// Category
    pub category: MediaCategory,
}

/// Complete content type
#[derive(Debug, Clone)]
pub struct ContentType {
    /// Media type
    pub media: MediaType,
    /// Charset
    pub charset: Option<Charset>,
    /// Multipart boundary
    pub boundary: Option<String>,
    /// Additional parameters
    pub params: Vec<(String, String)>,
}

/// Safe content type operations
pub struct SafeContentType;

impl SafeContentType {
    /// Types that can be sniffed to something dangerous
    const SNIFFABLE_TO_DANGEROUS: &'static [&'static str] = &[
        "text/plain",
        "application/octet-stream",
        "application/x-unknown",
        "unknown/unknown",
    ];

    /// Parse category from type string
    pub fn parse_category(s: &str) -> MediaCategory {
        match s.to_lowercase().as_str() {
            "text" => MediaCategory::Text,
            "image" => MediaCategory::Image,
            "audio" => MediaCategory::Audio,
            "video" => MediaCategory::Video,
            "application" => MediaCategory::Application,
            "multipart" => MediaCategory::Multipart,
            "message" => MediaCategory::Message,
            "font" => MediaCategory::Font,
            "model" => MediaCategory::Model,
            _ => MediaCategory::Custom,
        }
    }

    /// Check if charset is Unicode
    pub fn is_unicode(charset: &Charset) -> bool {
        matches!(charset, Charset::Utf8 | Charset::Utf16Le | Charset::Utf16Be)
    }

    /// Render charset to string
    pub fn charset_to_string(charset: &Charset) -> String {
        match charset {
            Charset::Utf8 => "utf-8".to_string(),
            Charset::Utf16Le => "utf-16le".to_string(),
            Charset::Utf16Be => "utf-16be".to_string(),
            Charset::Iso8859_1 => "iso-8859-1".to_string(),
            Charset::Ascii => "us-ascii".to_string(),
            Charset::Windows1252 => "windows-1252".to_string(),
            Charset::Other(s) => s.to_lowercase(),
        }
    }

    /// Parse charset from string
    pub fn parse_charset(s: &str) -> Charset {
        match s.trim().to_lowercase().as_str() {
            "utf-8" | "utf8" => Charset::Utf8,
            "utf-16le" => Charset::Utf16Le,
            "utf-16be" => Charset::Utf16Be,
            "iso-8859-1" | "latin1" => Charset::Iso8859_1,
            "us-ascii" | "ascii" => Charset::Ascii,
            "windows-1252" | "cp1252" => Charset::Windows1252,
            other => Charset::Other(other.to_string()),
        }
    }

    /// Check if type can be sniffed to something dangerous
    pub fn can_sniff_to_dangerous(s: &str) -> bool {
        Self::SNIFFABLE_TO_DANGEROUS.contains(&s.to_lowercase().as_str())
    }

    /// Extract suffix from subtype (e.g., "json" from "vnd.api+json")
    fn extract_suffix(subtype: &str) -> (String, Option<String>) {
        match subtype.find('+') {
            Some(idx) => (
                subtype[..idx].to_string(),
                Some(subtype[idx + 1..].to_string()),
            ),
            None => (subtype.to_string(), None),
        }
    }

    /// Parse Content-Type header
    pub fn parse(raw: &str) -> Result<ContentType> {
        let trimmed = raw.trim();
        if trimmed.is_empty() || trimmed.len() > 1024 {
            return Err(Error::InvalidFormat("Content-Type length invalid".into()));
        }

        let parts: Vec<&str> = trimmed.splitn(2, ';').collect();
        let media_str = parts[0].trim();

        let (type_str, subtype_str) = match media_str.find('/') {
            Some(idx) => (
                media_str[..idx].trim().to_lowercase(),
                media_str[idx + 1..].trim().to_lowercase(),
            ),
            None => return Err(Error::InvalidFormat("Missing slash in media type".into())),
        };

        if type_str.is_empty() || subtype_str.is_empty() {
            return Err(Error::InvalidFormat("Empty type or subtype".into()));
        }

        let (base_subtype, suffix) = Self::extract_suffix(&subtype_str);
        let category = Self::parse_category(&type_str);

        let media = MediaType {
            media_type: type_str,
            subtype: base_subtype,
            suffix,
            category,
        };

        // Parse parameters
        let mut charset = None;
        let mut boundary = None;
        let mut params = Vec::new();

        if parts.len() > 1 {
            for param in parts[1].split(';') {
                let p = param.trim();
                if let Some(eq_idx) = p.find('=') {
                    let name = p[..eq_idx].trim().to_lowercase();
                    let mut value = p[eq_idx + 1..].trim();

                    // Remove quotes if present
                    if value.starts_with('"') && value.ends_with('"') && value.len() >= 2 {
                        value = &value[1..value.len() - 1];
                    }

                    match name.as_str() {
                        "charset" => charset = Some(Self::parse_charset(value)),
                        "boundary" => boundary = Some(value.to_string()),
                        _ => params.push((name, value.to_string())),
                    }
                }
            }
        }

        Ok(ContentType {
            media,
            charset,
            boundary,
            params,
        })
    }

    /// Create content type from type and subtype
    pub fn make(media_type: &str, subtype: &str) -> Result<ContentType> {
        let lower_t = media_type.trim().to_lowercase();
        let lower_s = subtype.trim().to_lowercase();

        if lower_t.is_empty() || lower_s.is_empty() {
            return Err(Error::InvalidFormat("Empty type or subtype".into()));
        }
        if lower_t.len() > 127 || lower_s.len() > 127 {
            return Err(Error::TooLong("Type or subtype too long".into()));
        }

        let (base_subtype, suffix) = Self::extract_suffix(&lower_s);
        let category = Self::parse_category(&lower_t);

        Ok(ContentType {
            media: MediaType {
                media_type: lower_t,
                subtype: base_subtype,
                suffix,
                category,
            },
            charset: None,
            boundary: None,
            params: Vec::new(),
        })
    }

    /// Render content type to string
    pub fn render(ct: &ContentType) -> String {
        let mut result = format!("{}/{}", ct.media.media_type, ct.media.subtype);

        if let Some(ref suffix) = ct.media.suffix {
            result.push('+');
            result.push_str(suffix);
        }

        if let Some(ref charset) = ct.charset {
            result.push_str("; charset=");
            result.push_str(&Self::charset_to_string(charset));
        }

        if let Some(ref boundary) = ct.boundary {
            result.push_str("; boundary=");
            result.push_str(boundary);
        }

        for (name, value) in &ct.params {
            result.push_str("; ");
            result.push_str(name);
            result.push('=');
            result.push_str(value);
        }

        result
    }

    /// Add charset to content type
    pub fn with_charset(ct: ContentType, charset: Charset) -> ContentType {
        ContentType {
            charset: Some(charset),
            ..ct
        }
    }

    /// Add boundary to content type
    pub fn with_boundary(ct: ContentType, boundary: String) -> ContentType {
        ContentType {
            boundary: Some(boundary),
            ..ct
        }
    }

    /// Check if content type is text
    pub fn is_text(ct: &ContentType) -> bool {
        ct.media.category == MediaCategory::Text
    }

    /// Check if content type is JSON
    pub fn is_json(ct: &ContentType) -> bool {
        ct.media.subtype == "json" || ct.media.suffix.as_deref() == Some("json")
    }

    /// Check if content type is XML
    pub fn is_xml(ct: &ContentType) -> bool {
        ct.media.subtype == "xml" || ct.media.suffix.as_deref() == Some("xml")
    }

    /// Check if content type is HTML
    pub fn is_html(ct: &ContentType) -> bool {
        ct.media.media_type == "text" && ct.media.subtype == "html"
    }

    /// Check if content type is multipart
    pub fn is_multipart(ct: &ContentType) -> bool {
        ct.media.category == MediaCategory::Multipart
    }

    /// Check if safe from MIME sniffing
    pub fn safe_from_sniffing(ct: &ContentType) -> bool {
        let full_type = format!("{}/{}", ct.media.media_type, ct.media.subtype);
        !Self::can_sniff_to_dangerous(&full_type)
    }

    // Common content types
    /// text/plain; charset=utf-8
    pub fn text_plain() -> ContentType {
        ContentType {
            media: MediaType {
                media_type: "text".to_string(),
                subtype: "plain".to_string(),
                suffix: None,
                category: MediaCategory::Text,
            },
            charset: Some(Charset::Utf8),
            boundary: None,
            params: Vec::new(),
        }
    }

    /// text/html; charset=utf-8
    pub fn text_html() -> ContentType {
        ContentType {
            media: MediaType {
                media_type: "text".to_string(),
                subtype: "html".to_string(),
                suffix: None,
                category: MediaCategory::Text,
            },
            charset: Some(Charset::Utf8),
            boundary: None,
            params: Vec::new(),
        }
    }

    /// application/json; charset=utf-8
    pub fn application_json() -> ContentType {
        ContentType {
            media: MediaType {
                media_type: "application".to_string(),
                subtype: "json".to_string(),
                suffix: None,
                category: MediaCategory::Application,
            },
            charset: Some(Charset::Utf8),
            boundary: None,
            params: Vec::new(),
        }
    }

    /// application/xml; charset=utf-8
    pub fn application_xml() -> ContentType {
        ContentType {
            media: MediaType {
                media_type: "application".to_string(),
                subtype: "xml".to_string(),
                suffix: None,
                category: MediaCategory::Application,
            },
            charset: Some(Charset::Utf8),
            boundary: None,
            params: Vec::new(),
        }
    }

    /// application/octet-stream
    pub fn application_octet_stream() -> ContentType {
        ContentType {
            media: MediaType {
                media_type: "application".to_string(),
                subtype: "octet-stream".to_string(),
                suffix: None,
                category: MediaCategory::Application,
            },
            charset: None,
            boundary: None,
            params: Vec::new(),
        }
    }

    /// application/x-www-form-urlencoded
    pub fn form_urlencoded() -> ContentType {
        ContentType {
            media: MediaType {
                media_type: "application".to_string(),
                subtype: "x-www-form-urlencoded".to_string(),
                suffix: None,
                category: MediaCategory::Application,
            },
            charset: None,
            boundary: None,
            params: Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple() {
        let ct = SafeContentType::parse("application/json").unwrap();
        assert_eq!(ct.media.media_type, "application");
        assert_eq!(ct.media.subtype, "json");
        assert_eq!(ct.media.category, MediaCategory::Application);
    }

    #[test]
    fn test_parse_with_charset() {
        let ct = SafeContentType::parse("text/html; charset=utf-8").unwrap();
        assert_eq!(ct.media.media_type, "text");
        assert_eq!(ct.media.subtype, "html");
        assert_eq!(ct.charset, Some(Charset::Utf8));
    }

    #[test]
    fn test_parse_with_suffix() {
        let ct = SafeContentType::parse("application/vnd.api+json").unwrap();
        assert_eq!(ct.media.subtype, "vnd.api");
        assert_eq!(ct.media.suffix, Some("json".to_string()));
    }

    #[test]
    fn test_is_json() {
        let ct1 = SafeContentType::parse("application/json").unwrap();
        assert!(SafeContentType::is_json(&ct1));

        let ct2 = SafeContentType::parse("application/vnd.api+json").unwrap();
        assert!(SafeContentType::is_json(&ct2));

        let ct3 = SafeContentType::parse("text/html").unwrap();
        assert!(!SafeContentType::is_json(&ct3));
    }

    #[test]
    fn test_can_sniff_dangerous() {
        assert!(SafeContentType::can_sniff_to_dangerous("text/plain"));
        assert!(SafeContentType::can_sniff_to_dangerous("application/octet-stream"));
        assert!(!SafeContentType::can_sniff_to_dangerous("application/json"));
    }

    #[test]
    fn test_render() {
        let ct = SafeContentType::application_json();
        let rendered = SafeContentType::render(&ct);
        assert_eq!(rendered, "application/json; charset=utf-8");
    }
}
