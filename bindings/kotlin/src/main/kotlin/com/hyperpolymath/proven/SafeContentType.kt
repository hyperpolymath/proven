// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * MIME type representation.
 */
data class ContentType(
    val type: String,
    val subtype: String,
    val parameters: Map<String, String> = emptyMap()
) {
    val essence: String get() = "$type/$subtype"

    val charset: String? get() = parameters["charset"]

    val boundary: String? get() = parameters["boundary"]

    fun withCharset(charset: String): ContentType {
        return copy(parameters = parameters + ("charset" to charset))
    }

    fun withBoundary(boundary: String): ContentType {
        return copy(parameters = parameters + ("boundary" to boundary))
    }

    fun matches(other: ContentType): Boolean {
        if (type != "*" && other.type != "*" && type != other.type) return false
        if (subtype != "*" && other.subtype != "*" && subtype != other.subtype) return false
        return true
    }

    override fun toString(): String {
        return if (parameters.isEmpty()) {
            essence
        } else {
            val params = parameters.entries.joinToString("; ") { (k, v) ->
                if (v.any { it.isWhitespace() || it == '"' || it == ';' }) {
                    "$k=\"${v.replace("\"", "\\\"")}\""
                } else {
                    "$k=$v"
                }
            }
            "$essence; $params"
        }
    }

    companion object {
        // Text types
        val TEXT_PLAIN = ContentType("text", "plain", mapOf("charset" to "utf-8"))
        val TEXT_HTML = ContentType("text", "html", mapOf("charset" to "utf-8"))
        val TEXT_CSS = ContentType("text", "css", mapOf("charset" to "utf-8"))
        val TEXT_JAVASCRIPT = ContentType("text", "javascript", mapOf("charset" to "utf-8"))
        val TEXT_CSV = ContentType("text", "csv", mapOf("charset" to "utf-8"))
        val TEXT_XML = ContentType("text", "xml", mapOf("charset" to "utf-8"))
        val TEXT_MARKDOWN = ContentType("text", "markdown", mapOf("charset" to "utf-8"))

        // Application types
        val APPLICATION_JSON = ContentType("application", "json", mapOf("charset" to "utf-8"))
        val APPLICATION_XML = ContentType("application", "xml", mapOf("charset" to "utf-8"))
        val APPLICATION_OCTET_STREAM = ContentType("application", "octet-stream")
        val APPLICATION_PDF = ContentType("application", "pdf")
        val APPLICATION_ZIP = ContentType("application", "zip")
        val APPLICATION_GZIP = ContentType("application", "gzip")
        val APPLICATION_FORM_URLENCODED = ContentType("application", "x-www-form-urlencoded")
        val APPLICATION_JAVASCRIPT = ContentType("application", "javascript", mapOf("charset" to "utf-8"))
        val APPLICATION_WASM = ContentType("application", "wasm")
        val APPLICATION_GRAPHQL = ContentType("application", "graphql+json", mapOf("charset" to "utf-8"))
        val APPLICATION_LD_JSON = ContentType("application", "ld+json", mapOf("charset" to "utf-8"))
        val APPLICATION_MSGPACK = ContentType("application", "msgpack")
        val APPLICATION_PROTOBUF = ContentType("application", "x-protobuf")

        // Multipart types
        val MULTIPART_FORM_DATA = ContentType("multipart", "form-data")
        val MULTIPART_MIXED = ContentType("multipart", "mixed")
        val MULTIPART_ALTERNATIVE = ContentType("multipart", "alternative")

        // Image types
        val IMAGE_PNG = ContentType("image", "png")
        val IMAGE_JPEG = ContentType("image", "jpeg")
        val IMAGE_GIF = ContentType("image", "gif")
        val IMAGE_WEBP = ContentType("image", "webp")
        val IMAGE_SVG = ContentType("image", "svg+xml")
        val IMAGE_ICO = ContentType("image", "x-icon")
        val IMAGE_AVIF = ContentType("image", "avif")

        // Audio types
        val AUDIO_MPEG = ContentType("audio", "mpeg")
        val AUDIO_WAV = ContentType("audio", "wav")
        val AUDIO_OGG = ContentType("audio", "ogg")
        val AUDIO_WEBM = ContentType("audio", "webm")
        val AUDIO_FLAC = ContentType("audio", "flac")
        val AUDIO_AAC = ContentType("audio", "aac")

        // Video types
        val VIDEO_MP4 = ContentType("video", "mp4")
        val VIDEO_WEBM = ContentType("video", "webm")
        val VIDEO_OGG = ContentType("video", "ogg")
        val VIDEO_QUICKTIME = ContentType("video", "quicktime")

        // Font types
        val FONT_WOFF = ContentType("font", "woff")
        val FONT_WOFF2 = ContentType("font", "woff2")
        val FONT_TTF = ContentType("font", "ttf")
        val FONT_OTF = ContentType("font", "otf")

        // Wildcard
        val ANY = ContentType("*", "*")

        /**
         * Parse a Content-Type header value.
         */
        fun parse(value: String): ContentType? {
            val parts = value.split(';').map { it.trim() }
            if (parts.isEmpty()) return null

            val essence = parts[0]
            val slashIndex = essence.indexOf('/')
            if (slashIndex == -1) return null

            val type = essence.substring(0, slashIndex).lowercase()
            val subtype = essence.substring(slashIndex + 1).lowercase()

            if (type.isEmpty() || subtype.isEmpty()) return null

            val parameters = mutableMapOf<String, String>()
            for (param in parts.drop(1)) {
                val eqIndex = param.indexOf('=')
                if (eqIndex != -1) {
                    val key = param.substring(0, eqIndex).trim().lowercase()
                    var paramValue = param.substring(eqIndex + 1).trim()
                    // Remove quotes
                    if (paramValue.startsWith('"') && paramValue.endsWith('"')) {
                        paramValue = paramValue.drop(1).dropLast(1)
                    }
                    parameters[key] = paramValue
                }
            }

            return ContentType(type, subtype, parameters)
        }
    }
}

/**
 * Content type utilities.
 */
object SafeContentType {
    /**
     * Extension to MIME type mapping.
     */
    private val EXTENSION_MAP = mapOf(
        // Text
        "txt" to ContentType.TEXT_PLAIN,
        "html" to ContentType.TEXT_HTML,
        "htm" to ContentType.TEXT_HTML,
        "css" to ContentType.TEXT_CSS,
        "csv" to ContentType.TEXT_CSV,
        "xml" to ContentType.TEXT_XML,
        "md" to ContentType.TEXT_MARKDOWN,
        "markdown" to ContentType.TEXT_MARKDOWN,

        // Application
        "js" to ContentType.APPLICATION_JAVASCRIPT,
        "mjs" to ContentType.APPLICATION_JAVASCRIPT,
        "json" to ContentType.APPLICATION_JSON,
        "pdf" to ContentType.APPLICATION_PDF,
        "zip" to ContentType.APPLICATION_ZIP,
        "gz" to ContentType.APPLICATION_GZIP,
        "gzip" to ContentType.APPLICATION_GZIP,
        "wasm" to ContentType.APPLICATION_WASM,

        // Images
        "png" to ContentType.IMAGE_PNG,
        "jpg" to ContentType.IMAGE_JPEG,
        "jpeg" to ContentType.IMAGE_JPEG,
        "gif" to ContentType.IMAGE_GIF,
        "webp" to ContentType.IMAGE_WEBP,
        "svg" to ContentType.IMAGE_SVG,
        "ico" to ContentType.IMAGE_ICO,
        "avif" to ContentType.IMAGE_AVIF,

        // Audio
        "mp3" to ContentType.AUDIO_MPEG,
        "wav" to ContentType.AUDIO_WAV,
        "ogg" to ContentType.AUDIO_OGG,
        "flac" to ContentType.AUDIO_FLAC,
        "aac" to ContentType.AUDIO_AAC,

        // Video
        "mp4" to ContentType.VIDEO_MP4,
        "webm" to ContentType.VIDEO_WEBM,
        "ogv" to ContentType.VIDEO_OGG,
        "mov" to ContentType.VIDEO_QUICKTIME,

        // Fonts
        "woff" to ContentType.FONT_WOFF,
        "woff2" to ContentType.FONT_WOFF2,
        "ttf" to ContentType.FONT_TTF,
        "otf" to ContentType.FONT_OTF
    )

    /**
     * Get content type from file extension.
     */
    fun fromExtension(extension: String): ContentType? {
        return EXTENSION_MAP[extension.lowercase().removePrefix(".")]
    }

    /**
     * Get content type from filename.
     */
    fun fromFilename(filename: String): ContentType {
        val dotIndex = filename.lastIndexOf('.')
        if (dotIndex == -1) return ContentType.APPLICATION_OCTET_STREAM

        val extension = filename.substring(dotIndex + 1)
        return fromExtension(extension) ?: ContentType.APPLICATION_OCTET_STREAM
    }

    /**
     * Get file extension for content type.
     */
    fun toExtension(contentType: ContentType): String? {
        return EXTENSION_MAP.entries
            .find { it.value.essence == contentType.essence }
            ?.key
    }

    /**
     * Check if content type is text-based.
     */
    fun isText(contentType: ContentType): Boolean {
        if (contentType.type == "text") return true
        if (contentType.subtype.endsWith("+xml")) return true
        if (contentType.subtype.endsWith("+json")) return true

        return contentType.essence in setOf(
            "application/json",
            "application/xml",
            "application/javascript",
            "application/x-www-form-urlencoded",
            "application/graphql+json",
            "application/ld+json"
        )
    }

    /**
     * Check if content type is binary.
     */
    fun isBinary(contentType: ContentType): Boolean = !isText(contentType)

    /**
     * Check if content type is image.
     */
    fun isImage(contentType: ContentType): Boolean = contentType.type == "image"

    /**
     * Check if content type is audio.
     */
    fun isAudio(contentType: ContentType): Boolean = contentType.type == "audio"

    /**
     * Check if content type is video.
     */
    fun isVideo(contentType: ContentType): Boolean = contentType.type == "video"

    /**
     * Check if content type is media (image, audio, or video).
     */
    fun isMedia(contentType: ContentType): Boolean {
        return isImage(contentType) || isAudio(contentType) || isVideo(contentType)
    }

    /**
     * Check if content type is compressible.
     */
    fun isCompressible(contentType: ContentType): Boolean {
        if (isText(contentType)) return true
        if (contentType.subtype.endsWith("+xml")) return true
        if (contentType.subtype.endsWith("+json")) return true

        return contentType.essence in setOf(
            "image/svg+xml",
            "application/wasm"
        )
    }

    /**
     * Normalize a content type string.
     */
    fun normalize(value: String): String? {
        return ContentType.parse(value)?.toString()
    }

    /**
     * Check if two content types match (considering wildcards).
     */
    fun matches(pattern: ContentType, actual: ContentType): Boolean {
        return pattern.matches(actual)
    }

    /**
     * Check if content type matches any in list.
     */
    fun matchesAny(patterns: List<ContentType>, actual: ContentType): Boolean {
        return patterns.any { it.matches(actual) }
    }

    /**
     * Select best content type from Accept header.
     */
    fun selectBest(available: List<ContentType>, acceptHeader: String): ContentType? {
        val parsed = SafeHeader.parseAccept(acceptHeader)

        for ((mediaType, _) in parsed) {
            val requestType = ContentType.parse(mediaType) ?: continue
            val match = available.find { requestType.matches(it) }
            if (match != null) return match
        }

        return available.firstOrNull()
    }
}
