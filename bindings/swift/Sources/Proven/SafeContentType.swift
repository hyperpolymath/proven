// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe content type operations delegated to libproven FFI.
///
/// All parsing and validation is performed by the formally verified
/// Idris 2 core via the Zig FFI bridge.

import CProven

/// Parsed content type components returned from the FFI layer.
public struct ContentTypeComponents {
    public let type: String
    public let subtype: String
    public let suffix: String?
    public let parameters: String?
}

public enum SafeContentType {
    /// Parse a Content-Type header string into its components.
    public static func parse(_ header: String) -> Result<ContentTypeComponents, ProvenError> {
        withStringBytes(header) { ptr, len in
            var result = proven_content_type_parse(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                proven_content_type_free(&result)
                return .failure(error)
            }

            let typeStr: String
            if let p = result.type_str {
                typeStr = String(cString: p)
            } else {
                proven_content_type_free(&result)
                return .failure(.nullPointer)
            }

            let subtypeStr: String
            if let p = result.subtype_str {
                subtypeStr = String(cString: p)
            } else {
                proven_content_type_free(&result)
                return .failure(.nullPointer)
            }

            let suffixStr: String? = result.suffix_str.flatMap { result.suffix_len > 0 ? String(cString: $0) : nil }
            let paramsStr: String? = result.params_str.flatMap { result.params_len > 0 ? String(cString: $0) : nil }

            proven_content_type_free(&result)

            return .success(ContentTypeComponents(
                type: typeStr,
                subtype: subtypeStr,
                suffix: suffixStr,
                parameters: paramsStr
            ))
        }
    }

    /// Check if a content type string can be sniffed into a dangerous type.
    public static func canSniffDangerous(_ contentType: String) -> Result<Bool, ProvenError> {
        withStringBytes(contentType) { ptr, len in
            let result = proven_content_type_can_sniff_dangerous(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(result.value)
        }
    }

    /// Render a content type from its type and subtype components.
    public static func render(type: String, subtype: String) -> Result<String, ProvenError> {
        let typeUtf8 = Array(type.utf8)
        let subtypeUtf8 = Array(subtype.utf8)
        return typeUtf8.withUnsafeBufferPointer { typeBuf in
            subtypeUtf8.withUnsafeBufferPointer { subtypeBuf in
                let result = proven_content_type_render(
                    typeBuf.baseAddress, typeBuf.count,
                    subtypeBuf.baseAddress, subtypeBuf.count
                )
                return consumeStringResult(result)
            }
        }
    }

    /// Check if the given subtype/suffix combination represents JSON.
    public static func isJSON(subtype: String, suffix: String = "") -> Result<Bool, ProvenError> {
        let subtypeUtf8 = Array(subtype.utf8)
        let suffixUtf8 = Array(suffix.utf8)
        return subtypeUtf8.withUnsafeBufferPointer { subtypeBuf in
            suffixUtf8.withUnsafeBufferPointer { suffixBuf in
                let result = proven_content_type_is_json(
                    subtypeBuf.baseAddress, subtypeBuf.count,
                    suffixBuf.baseAddress, suffixBuf.count
                )
                if let error = ProvenError.fromStatus(result.status) {
                    return .failure(error)
                }
                return .success(result.value)
            }
        }
    }

    /// Check if the given subtype/suffix combination represents XML.
    public static func isXML(subtype: String, suffix: String = "") -> Result<Bool, ProvenError> {
        let subtypeUtf8 = Array(subtype.utf8)
        let suffixUtf8 = Array(suffix.utf8)
        return subtypeUtf8.withUnsafeBufferPointer { subtypeBuf in
            suffixUtf8.withUnsafeBufferPointer { suffixBuf in
                let result = proven_content_type_is_xml(
                    subtypeBuf.baseAddress, subtypeBuf.count,
                    suffixBuf.baseAddress, suffixBuf.count
                )
                if let error = ProvenError.fromStatus(result.status) {
                    return .failure(error)
                }
                return .success(result.value)
            }
        }
    }
}
