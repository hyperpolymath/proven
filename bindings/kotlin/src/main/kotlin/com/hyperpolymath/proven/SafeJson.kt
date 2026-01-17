// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * JSON value representation.
 */
sealed class JsonValue {
    data class JsonNull(val unit: Unit = Unit) : JsonValue()
    data class JsonBool(val value: Boolean) : JsonValue()
    data class JsonNumber(val value: Double) : JsonValue()
    data class JsonString(val value: String) : JsonValue()
    data class JsonArray(val value: List<JsonValue>) : JsonValue()
    data class JsonObject(val value: Map<String, JsonValue>) : JsonValue()

    companion object {
        val NULL = JsonNull()
    }

    fun isNull(): Boolean = this is JsonNull
    fun isBool(): Boolean = this is JsonBool
    fun isNumber(): Boolean = this is JsonNumber
    fun isString(): Boolean = this is JsonString
    fun isArray(): Boolean = this is JsonArray
    fun isObject(): Boolean = this is JsonObject

    fun asBoolean(): Boolean? = (this as? JsonBool)?.value
    fun asNumber(): Double? = (this as? JsonNumber)?.value
    fun asInt(): Int? = (this as? JsonNumber)?.value?.toInt()
    fun asString(): String? = (this as? JsonString)?.value
    fun asArray(): List<JsonValue>? = (this as? JsonArray)?.value
    fun asObject(): Map<String, JsonValue>? = (this as? JsonObject)?.value

    /**
     * Get value at path (e.g., "user.name" or "items[0].id").
     */
    fun getPath(path: String): JsonValue? {
        if (path.isEmpty()) return this

        val parts = parsePath(path)
        var current: JsonValue? = this

        for (part in parts) {
            current = when {
                part.isIndex -> {
                    val arr = current?.asArray() ?: return null
                    val idx = part.index ?: return null
                    if (idx < 0 || idx >= arr.size) null else arr[idx]
                }
                else -> {
                    val obj = current?.asObject() ?: return null
                    obj[part.key]
                }
            }
            if (current == null) return null
        }

        return current
    }

    private data class PathPart(val key: String, val isIndex: Boolean, val index: Int?)

    private fun parsePath(path: String): List<PathPart> {
        val parts = mutableListOf<PathPart>()
        val regex = Regex("""(\w+)|\[(\d+)\]""")

        for (match in regex.findAll(path)) {
            when {
                match.groupValues[1].isNotEmpty() -> {
                    parts.add(PathPart(match.groupValues[1], false, null))
                }
                match.groupValues[2].isNotEmpty() -> {
                    parts.add(PathPart("", true, match.groupValues[2].toIntOrNull()))
                }
            }
        }

        return parts
    }
}

/**
 * JSON utilities.
 */
object SafeJson {
    /**
     * Parse JSON string.
     */
    fun parse(json: String): Result<JsonValue> {
        return try {
            val trimmed = json.trim()
            val (value, _) = parseValue(trimmed, 0)
            Result.success(value)
        } catch (e: Exception) {
            Result.failure(e)
        }
    }

    /**
     * Stringify JSON value.
     */
    fun stringify(value: JsonValue, indent: Int = 0): String {
        return stringifyValue(value, indent, 0)
    }

    /**
     * Stringify JSON value with pretty printing.
     */
    fun prettyPrint(value: JsonValue): String = stringify(value, 2)

    private fun parseValue(json: String, pos: Int): Pair<JsonValue, Int> {
        val (_, startPos) = skipWhitespace(json, pos)

        return when {
            startPos >= json.length -> throw IllegalArgumentException("Unexpected end of input")
            json.startsWith("null", startPos) -> JsonValue.NULL to startPos + 4
            json.startsWith("true", startPos) -> JsonValue.JsonBool(true) to startPos + 4
            json.startsWith("false", startPos) -> JsonValue.JsonBool(false) to startPos + 5
            json[startPos] == '"' -> parseString(json, startPos)
            json[startPos] == '[' -> parseArray(json, startPos)
            json[startPos] == '{' -> parseObject(json, startPos)
            json[startPos].isDigit() || json[startPos] == '-' -> parseNumber(json, startPos)
            else -> throw IllegalArgumentException("Unexpected character at position $startPos")
        }
    }

    private fun parseString(json: String, pos: Int): Pair<JsonValue.JsonString, Int> {
        var i = pos + 1
        val sb = StringBuilder()

        while (i < json.length && json[i] != '"') {
            if (json[i] == '\\' && i + 1 < json.length) {
                i++
                sb.append(
                    when (json[i]) {
                        'n' -> '\n'
                        't' -> '\t'
                        'r' -> '\r'
                        '"' -> '"'
                        '\\' -> '\\'
                        '/' -> '/'
                        else -> json[i]
                    }
                )
            } else {
                sb.append(json[i])
            }
            i++
        }

        return JsonValue.JsonString(sb.toString()) to i + 1
    }

    private fun parseNumber(json: String, pos: Int): Pair<JsonValue.JsonNumber, Int> {
        var i = pos
        while (i < json.length && (json[i].isDigit() || json[i] in ".-+eE")) {
            i++
        }
        val numStr = json.substring(pos, i)
        return JsonValue.JsonNumber(numStr.toDouble()) to i
    }

    private fun parseArray(json: String, pos: Int): Pair<JsonValue.JsonArray, Int> {
        val items = mutableListOf<JsonValue>()
        var i = pos + 1

        while (i < json.length) {
            val (_, wsPos) = skipWhitespace(json, i)
            i = wsPos

            if (json[i] == ']') return JsonValue.JsonArray(items) to i + 1

            val (value, nextPos) = parseValue(json, i)
            items.add(value)
            i = nextPos

            val (_, afterValuePos) = skipWhitespace(json, i)
            i = afterValuePos

            when {
                json[i] == ',' -> i++
                json[i] == ']' -> return JsonValue.JsonArray(items) to i + 1
                else -> throw IllegalArgumentException("Expected ',' or ']' at position $i")
            }
        }

        throw IllegalArgumentException("Unterminated array")
    }

    private fun parseObject(json: String, pos: Int): Pair<JsonValue.JsonObject, Int> {
        val map = mutableMapOf<String, JsonValue>()
        var i = pos + 1

        while (i < json.length) {
            val (_, wsPos) = skipWhitespace(json, i)
            i = wsPos

            if (json[i] == '}') return JsonValue.JsonObject(map) to i + 1

            if (json[i] != '"') throw IllegalArgumentException("Expected string key at position $i")
            val (keyValue, afterKey) = parseString(json, i)
            i = afterKey

            val (_, colonPos) = skipWhitespace(json, i)
            i = colonPos

            if (json[i] != ':') throw IllegalArgumentException("Expected ':' at position $i")
            i++

            val (value, afterValue) = parseValue(json, i)
            map[keyValue.value] = value
            i = afterValue

            val (_, afterObjVal) = skipWhitespace(json, i)
            i = afterObjVal

            when {
                json[i] == ',' -> i++
                json[i] == '}' -> return JsonValue.JsonObject(map) to i + 1
                else -> throw IllegalArgumentException("Expected ',' or '}' at position $i")
            }
        }

        throw IllegalArgumentException("Unterminated object")
    }

    private fun skipWhitespace(json: String, pos: Int): Pair<Unit, Int> {
        var i = pos
        while (i < json.length && json[i].isWhitespace()) i++
        return Unit to i
    }

    private fun stringifyValue(value: JsonValue, indent: Int, depth: Int): String {
        return when (value) {
            is JsonValue.JsonNull -> "null"
            is JsonValue.JsonBool -> value.value.toString()
            is JsonValue.JsonNumber -> {
                if (value.value == value.value.toLong().toDouble()) {
                    value.value.toLong().toString()
                } else {
                    value.value.toString()
                }
            }
            is JsonValue.JsonString -> "\"${escapeString(value.value)}\""
            is JsonValue.JsonArray -> stringifyArray(value.value, indent, depth)
            is JsonValue.JsonObject -> stringifyObject(value.value, indent, depth)
        }
    }

    private fun stringifyArray(arr: List<JsonValue>, indent: Int, depth: Int): String {
        if (arr.isEmpty()) return "[]"
        if (indent == 0) {
            return "[${arr.joinToString(",") { stringifyValue(it, 0, 0) }}]"
        }

        val pad = " ".repeat(indent * (depth + 1))
        val closePad = " ".repeat(indent * depth)
        val items = arr.joinToString(",\n$pad") { stringifyValue(it, indent, depth + 1) }
        return "[\n$pad$items\n$closePad]"
    }

    private fun stringifyObject(obj: Map<String, JsonValue>, indent: Int, depth: Int): String {
        if (obj.isEmpty()) return "{}"
        if (indent == 0) {
            return "{${obj.entries.joinToString(",") { "\"${escapeString(it.key)}\":${stringifyValue(it.value, 0, 0)}" }}}"
        }

        val pad = " ".repeat(indent * (depth + 1))
        val closePad = " ".repeat(indent * depth)
        val items = obj.entries.joinToString(",\n$pad") {
            "\"${escapeString(it.key)}\": ${stringifyValue(it.value, indent, depth + 1)}"
        }
        return "{\n$pad$items\n$closePad}"
    }

    private fun escapeString(s: String): String {
        return s.replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\t", "\\t")
    }
}
