// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// MIME type with validation.
public struct ContentType: Equatable, Hashable {
    public let type: String
    public let subtype: String
    public let parameters: [String: String]

    public init?(type: String, subtype: String, parameters: [String: String] = [:]) {
        // Validate type and subtype (alphanumeric, hyphen, plus, dot)
        let validChars = CharacterSet.alphanumerics.union(CharacterSet(charactersIn: "-+."))

        guard !type.isEmpty && type.unicodeScalars.allSatisfy({ validChars.contains($0) }) else { return nil }
        guard !subtype.isEmpty && subtype.unicodeScalars.allSatisfy({ validChars.contains($0) }) else { return nil }

        self.type = type.lowercased()
        self.subtype = subtype.lowercased()
        self.parameters = parameters
    }

    /// Full MIME type string.
    public var mimeType: String {
        "\(type)/\(subtype)"
    }

    /// Full content type string with parameters.
    public var full: String {
        var result = mimeType
        for (key, value) in parameters.sorted(by: { $0.key < $1.key }) {
            result += "; \(key)=\(value)"
        }
        return result
    }

    /// Get charset parameter.
    public var charset: String? {
        parameters["charset"]
    }

    /// Get boundary parameter (for multipart).
    public var boundary: String? {
        parameters["boundary"]
    }

    /// Check if this is a text type.
    public var isText: Bool {
        type == "text" || (type == "application" && subtype.contains("json")) ||
        (type == "application" && subtype.contains("xml"))
    }

    /// Check if this is an image type.
    public var isImage: Bool {
        type == "image"
    }

    /// Check if this is an audio type.
    public var isAudio: Bool {
        type == "audio"
    }

    /// Check if this is a video type.
    public var isVideo: Bool {
        type == "video"
    }

    /// Check if this is a multipart type.
    public var isMultipart: Bool {
        type == "multipart"
    }

    /// Create a copy with different charset.
    public func withCharset(_ charset: String) -> ContentType {
        var newParams = parameters
        newParams["charset"] = charset
        return ContentType(type: type, subtype: subtype, parameters: newParams)!
    }
}

/// Content type utilities.
public enum SafeContentType {
    // MARK: - Common Content Types

    public static let textPlain = ContentType(type: "text", subtype: "plain")!
    public static let textHtml = ContentType(type: "text", subtype: "html")!
    public static let textCss = ContentType(type: "text", subtype: "css")!
    public static let textJavascript = ContentType(type: "text", subtype: "javascript")!
    public static let textXml = ContentType(type: "text", subtype: "xml")!
    public static let textCsv = ContentType(type: "text", subtype: "csv")!

    public static let applicationJson = ContentType(type: "application", subtype: "json")!
    public static let applicationXml = ContentType(type: "application", subtype: "xml")!
    public static let applicationOctetStream = ContentType(type: "application", subtype: "octet-stream")!
    public static let applicationPdf = ContentType(type: "application", subtype: "pdf")!
    public static let applicationZip = ContentType(type: "application", subtype: "zip")!
    public static let applicationGzip = ContentType(type: "application", subtype: "gzip")!
    public static let applicationJavascript = ContentType(type: "application", subtype: "javascript")!

    public static let formUrlencoded = ContentType(type: "application", subtype: "x-www-form-urlencoded")!
    public static let multipartFormData = ContentType(type: "multipart", subtype: "form-data")!
    public static let multipartMixed = ContentType(type: "multipart", subtype: "mixed")!

    public static let imageJpeg = ContentType(type: "image", subtype: "jpeg")!
    public static let imagePng = ContentType(type: "image", subtype: "png")!
    public static let imageGif = ContentType(type: "image", subtype: "gif")!
    public static let imageWebp = ContentType(type: "image", subtype: "webp")!
    public static let imageSvg = ContentType(type: "image", subtype: "svg+xml")!
    public static let imageIco = ContentType(type: "image", subtype: "x-icon")!

    public static let audioMpeg = ContentType(type: "audio", subtype: "mpeg")!
    public static let audioWav = ContentType(type: "audio", subtype: "wav")!
    public static let audioOgg = ContentType(type: "audio", subtype: "ogg")!
    public static let audioWebm = ContentType(type: "audio", subtype: "webm")!

    public static let videoMp4 = ContentType(type: "video", subtype: "mp4")!
    public static let videoWebm = ContentType(type: "video", subtype: "webm")!
    public static let videoOgg = ContentType(type: "video", subtype: "ogg")!

    public static let fontWoff = ContentType(type: "font", subtype: "woff")!
    public static let fontWoff2 = ContentType(type: "font", subtype: "woff2")!
    public static let fontTtf = ContentType(type: "font", subtype: "ttf")!
    public static let fontOtf = ContentType(type: "font", subtype: "otf")!

    // MARK: - Parsing

    /// Parse Content-Type header.
    public static func parse(_ header: String) -> ContentType? {
        let parts = header.components(separatedBy: ";")
        guard let mimeType = parts.first?.trimmingCharacters(in: .whitespaces) else { return nil }

        let mimeParts = mimeType.components(separatedBy: "/")
        guard mimeParts.count == 2 else { return nil }

        var parameters: [String: String] = [:]
        for part in parts.dropFirst() {
            let trimmed = part.trimmingCharacters(in: .whitespaces)
            if let equalsIndex = trimmed.firstIndex(of: "=") {
                let key = String(trimmed[..<equalsIndex]).lowercased()
                var value = String(trimmed[trimmed.index(after: equalsIndex)...])

                // Remove quotes if present
                if value.hasPrefix("\"") && value.hasSuffix("\"") {
                    value = String(value.dropFirst().dropLast())
                }

                parameters[key] = value
            }
        }

        return ContentType(type: mimeParts[0], subtype: mimeParts[1], parameters: parameters)
    }

    // MARK: - File Extension Mapping

    private static let extensionMap: [String: ContentType] = [
        "html": textHtml,
        "htm": textHtml,
        "css": textCss,
        "js": textJavascript,
        "mjs": textJavascript,
        "json": applicationJson,
        "xml": applicationXml,
        "txt": textPlain,
        "csv": textCsv,
        "pdf": applicationPdf,
        "zip": applicationZip,
        "gz": applicationGzip,
        "jpg": imageJpeg,
        "jpeg": imageJpeg,
        "png": imagePng,
        "gif": imageGif,
        "webp": imageWebp,
        "svg": imageSvg,
        "ico": imageIco,
        "mp3": audioMpeg,
        "wav": audioWav,
        "ogg": audioOgg,
        "mp4": videoMp4,
        "webm": videoWebm,
        "woff": fontWoff,
        "woff2": fontWoff2,
        "ttf": fontTtf,
        "otf": fontOtf
    ]

    /// Get content type from file extension.
    public static func fromExtension(_ ext: String) -> ContentType {
        let lowercased = ext.lowercased().trimmingCharacters(in: CharacterSet(charactersIn: "."))
        return extensionMap[lowercased] ?? applicationOctetStream
    }

    /// Get content type from filename.
    public static func fromFilename(_ filename: String) -> ContentType {
        guard let lastDot = filename.lastIndex(of: ".") else {
            return applicationOctetStream
        }
        let ext = String(filename[filename.index(after: lastDot)...])
        return fromExtension(ext)
    }

    /// Get file extension from content type.
    public static func toExtension(_ contentType: ContentType) -> String? {
        for (ext, ct) in extensionMap {
            if ct.mimeType == contentType.mimeType {
                return ext
            }
        }
        return nil
    }

    // MARK: - Content Type Helpers

    /// Create JSON content type with UTF-8.
    public static func json() -> ContentType {
        applicationJson.withCharset("utf-8")
    }

    /// Create HTML content type with UTF-8.
    public static func html() -> ContentType {
        textHtml.withCharset("utf-8")
    }

    /// Create plain text content type with UTF-8.
    public static func text() -> ContentType {
        textPlain.withCharset("utf-8")
    }

    /// Create multipart form data with boundary.
    public static func multipartForm(boundary: String) -> ContentType {
        var params = multipartFormData.parameters
        params["boundary"] = boundary
        return ContentType(type: "multipart", subtype: "form-data", parameters: params)!
    }

    /// Generate a random boundary for multipart.
    public static func generateBoundary() -> String {
        let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        return "----FormBoundary" + String((0..<16).map { _ in chars.randomElement()! })
    }
}
