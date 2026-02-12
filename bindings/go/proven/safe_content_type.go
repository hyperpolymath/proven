// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"regexp"
	"strings"
)

// ContentType represents a parsed MIME type.
type ContentType struct {
	Type       string
	Subtype    string
	Parameters map[string]string
}

// String returns the full content type string.
func (ct ContentType) String() string {
	result := ct.Type + "/" + ct.Subtype
	for k, v := range ct.Parameters {
		result += "; " + k + "=" + v
	}
	return result
}

// Essence returns type/subtype without parameters.
func (ct ContentType) Essence() string {
	return ct.Type + "/" + ct.Subtype
}

// Charset returns the charset parameter if present.
func (ct ContentType) Charset() string {
	return ct.Parameters["charset"]
}

// IsText checks if this is a text type.
func (ct ContentType) IsText() bool {
	return ct.Type == "text"
}

// IsJSON checks if this is JSON.
func (ct ContentType) IsJSON() bool {
	return ct.Essence() == "application/json" ||
		strings.HasSuffix(ct.Subtype, "+json")
}

// IsXML checks if this is XML.
func (ct ContentType) IsXML() bool {
	return ct.Essence() == "application/xml" ||
		ct.Essence() == "text/xml" ||
		strings.HasSuffix(ct.Subtype, "+xml")
}

// IsHTML checks if this is HTML.
func (ct ContentType) IsHTML() bool {
	return ct.Essence() == "text/html"
}

// IsImage checks if this is an image.
func (ct ContentType) IsImage() bool {
	return ct.Type == "image"
}

// IsBinary checks if this is likely binary content.
func (ct ContentType) IsBinary() bool {
	if ct.Type == "image" || ct.Type == "audio" || ct.Type == "video" {
		return true
	}
	if ct.Essence() == "application/octet-stream" {
		return true
	}
	return false
}

// ParseContentType parses a Content-Type header value.
func ParseContentType(value string) (ContentType, bool) {
	ct := ContentType{
		Parameters: make(map[string]string),
	}

	// Split by semicolon
	parts := strings.Split(value, ";")
	if len(parts) == 0 {
		return ct, false
	}

	// Parse type/subtype
	essence := strings.TrimSpace(parts[0])
	typeParts := strings.SplitN(essence, "/", 2)
	if len(typeParts) != 2 {
		return ct, false
	}

	ct.Type = strings.ToLower(strings.TrimSpace(typeParts[0]))
	ct.Subtype = strings.ToLower(strings.TrimSpace(typeParts[1]))

	if ct.Type == "" || ct.Subtype == "" {
		return ct, false
	}

	// Parse parameters
	paramRe := regexp.MustCompile(`([^=]+)=([^;]+)`)
	for _, part := range parts[1:] {
		match := paramRe.FindStringSubmatch(strings.TrimSpace(part))
		if len(match) >= 3 {
			key := strings.ToLower(strings.TrimSpace(match[1]))
			val := strings.Trim(strings.TrimSpace(match[2]), "\"")
			ct.Parameters[key] = val
		}
	}

	return ct, true
}

// SafeParseContentType parses with fallback.
func SafeParseContentType(value string) ContentType {
	ct, ok := ParseContentType(value)
	if !ok {
		return ContentType{
			Type:       "application",
			Subtype:    "octet-stream",
			Parameters: make(map[string]string),
		}
	}
	return ct
}

// DangerousMimeTypes lists potentially dangerous MIME types.
var DangerousMimeTypes = []string{
	"application/javascript",
	"text/javascript",
	"application/x-javascript",
	"text/html",
	"application/xhtml+xml",
	"image/svg+xml",
	"application/x-shockwave-flash",
}

// IsDangerousMimeType checks if content type is potentially dangerous.
func IsDangerousMimeType(ct ContentType) bool {
	essence := ct.Essence()
	for _, dangerous := range DangerousMimeTypes {
		if essence == dangerous {
			return true
		}
	}
	return false
}

// Common content type constants.
var (
	ContentTypeJSON          = ContentType{Type: "application", Subtype: "json", Parameters: map[string]string{}}
	ContentTypeXML           = ContentType{Type: "application", Subtype: "xml", Parameters: map[string]string{}}
	ContentTypeHTML          = ContentType{Type: "text", Subtype: "html", Parameters: map[string]string{}}
	ContentTypePlainText     = ContentType{Type: "text", Subtype: "plain", Parameters: map[string]string{}}
	ContentTypeOctetStream   = ContentType{Type: "application", Subtype: "octet-stream", Parameters: map[string]string{}}
	ContentTypeFormURLEnc    = ContentType{Type: "application", Subtype: "x-www-form-urlencoded", Parameters: map[string]string{}}
	ContentTypeMultipartForm = ContentType{Type: "multipart", Subtype: "form-data", Parameters: map[string]string{}}
)

// WithCharset returns content type with charset parameter.
func (ct ContentType) WithCharset(charset string) ContentType {
	newCT := ContentType{
		Type:       ct.Type,
		Subtype:    ct.Subtype,
		Parameters: make(map[string]string),
	}
	for k, v := range ct.Parameters {
		newCT.Parameters[k] = v
	}
	newCT.Parameters["charset"] = charset
	return newCT
}
