// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeContentType provides Content-Type header operations preventing MIME sniffing.
// All computation is performed in Idris 2 via the Proven FFI.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// Media type category constants.
const (
	MediaCategoryText        = 0
	MediaCategoryImage       = 1
	MediaCategoryAudio       = 2
	MediaCategoryVideo       = 3
	MediaCategoryApplication = 4
	MediaCategoryMultipart   = 5
	MediaCategoryMessage     = 6
	MediaCategoryFont        = 7
	MediaCategoryModel       = 8
	MediaCategoryCustom      = 9
)

// Charset encoding constants.
const (
	CharsetUTF8        = 0
	CharsetUTF16LE     = 1
	CharsetUTF16BE     = 2
	CharsetISO8859_1   = 3
	CharsetASCII       = 4
	CharsetWindows1252 = 5
	CharsetOther       = 6
)

// ContentType represents a parsed Content-Type header.
type ContentType struct {
	MediaType  string
	Subtype    string
	Suffix     string
	Category   int32
	Charset    int32
	HasCharset bool
}

// ContentTypeParse parses a Content-Type header string.
func ContentTypeParse(contentType string) (*ContentType, error) {
	cs, length := cString(contentType)
	defer unsafeFree(cs)

	result := C.proven_content_type_parse(cs, length)
	status := int(result.status)
	if status != StatusOK {
		return nil, newError(status)
	}

	ct := &ContentType{
		Category:   int32(result.category),
		Charset:    int32(result.charset),
		HasCharset: bool(result.has_charset),
	}

	if result.media_type != nil {
		ct.MediaType = C.GoStringN(result.media_type, C.int(result.media_type_len))
	}
	if result.subtype != nil {
		ct.Subtype = C.GoStringN(result.subtype, C.int(result.subtype_len))
	}
	if result.suffix != nil {
		ct.Suffix = C.GoStringN(result.suffix, C.int(result.suffix_len))
	}

	// Free the C-allocated strings.
	C.proven_content_type_free(&result)

	return ct, nil
}

// ContentTypeCanSniffDangerous checks whether a content type can be sniffed
// to something dangerous by browsers.
func ContentTypeCanSniffDangerous(contentType string) (bool, error) {
	cs, length := cString(contentType)
	defer unsafeFree(cs)
	result := C.proven_content_type_can_sniff_dangerous(cs, length)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}

// ContentTypeRender renders a Content-Type header string from components.
func ContentTypeRender(mediaType, subtype, suffix string, charset int32, hasCharset bool) (string, error) {
	typeCS, typeLen := cString(mediaType)
	defer unsafeFree(typeCS)
	subtypeCS, subtypeLen := cString(subtype)
	defer unsafeFree(subtypeCS)
	suffixCS, suffixLen := cString(suffix)
	defer unsafeFree(suffixCS)

	return goStringResult(C.proven_content_type_render(
		typeCS, typeLen,
		subtypeCS, subtypeLen,
		suffixCS, suffixLen,
		C.int32_t(charset),
		C._Bool(hasCharset),
	))
}

// ContentTypeIsJSON checks whether the content type represents JSON.
func ContentTypeIsJSON(subtype, suffix string) (bool, error) {
	subtypeCS, subtypeLen := cString(subtype)
	defer unsafeFree(subtypeCS)
	suffixCS, suffixLen := cString(suffix)
	defer unsafeFree(suffixCS)

	result := C.proven_content_type_is_json(subtypeCS, subtypeLen, suffixCS, suffixLen)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}

// ContentTypeIsXML checks whether the content type represents XML.
func ContentTypeIsXML(subtype, suffix string) (bool, error) {
	subtypeCS, subtypeLen := cString(subtype)
	defer unsafeFree(subtypeCS)
	suffixCS, suffixLen := cString(suffix)
	defer unsafeFree(suffixCS)

	result := C.proven_content_type_is_xml(subtypeCS, subtypeLen, suffixCS, suffixLen)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}
