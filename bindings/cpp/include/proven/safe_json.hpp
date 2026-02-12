// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file safe_json.hpp
 * @brief Safe JSON parsing and validation
 *
 * Provides JSON validation and safe access patterns without external
 * dependencies. For full JSON parsing, consider integrating with
 * nlohmann/json or rapidjson.
 *
 * @example
 * @code
 * #include <proven/safe_json.hpp>
 *
 * int main() {
 *     using namespace proven;
 *
 *     // Validate JSON syntax
 *     if (SafeJson::isValid(R"({"key": "value"})")) {
 *         std::cout << "Valid JSON\n";
 *     }
 *
 *     // Check for balanced structures
 *     if (!SafeJson::isValid(R"({"unclosed")")) {
 *         std::cout << "Invalid JSON detected\n";
 *     }
 *
 *     return 0;
 * }
 * @endcode
 */

#ifndef PROVEN_SAFE_JSON_HPP
#define PROVEN_SAFE_JSON_HPP

#include "common.hpp"
#include <string>
#include <string_view>
#include <vector>
#include <optional>

namespace proven {

/**
 * @brief JSON value types
 */
enum class JsonType {
    Null,
    Boolean,
    Number,
    String,
    Array,
    Object
};

/**
 * @brief Get string representation of JSON type
 */
[[nodiscard]] constexpr const char* jsonTypeToString(JsonType type) noexcept {
    switch (type) {
        case JsonType::Null: return "null";
        case JsonType::Boolean: return "boolean";
        case JsonType::Number: return "number";
        case JsonType::String: return "string";
        case JsonType::Array: return "array";
        case JsonType::Object: return "object";
        default: return "unknown";
    }
}

/**
 * @brief Safe JSON operations
 *
 * Provides validation and lightweight parsing utilities for JSON data.
 */
class SafeJson {
public:
    /**
     * @brief Validate JSON syntax
     *
     * Checks for balanced braces, brackets, and proper string quoting.
     * This is a lightweight validation that doesn't parse the full JSON.
     *
     * @param json The JSON string to validate
     * @return true if valid JSON structure
     */
    [[nodiscard]] static bool isValid(std::string_view json) noexcept {
        int32_t braceDepth = 0;
        int32_t bracketDepth = 0;
        bool inString = false;
        bool escape = false;

        for (char c : json) {
            if (escape) {
                escape = false;
                continue;
            }

            switch (c) {
                case '\\':
                    if (inString) escape = true;
                    break;
                case '"':
                    inString = !inString;
                    break;
                case '{':
                    if (!inString) ++braceDepth;
                    break;
                case '}':
                    if (!inString) --braceDepth;
                    break;
                case '[':
                    if (!inString) ++bracketDepth;
                    break;
                case ']':
                    if (!inString) --bracketDepth;
                    break;
                default:
                    break;
            }

            if (braceDepth < 0 || bracketDepth < 0) {
                return false;
            }
        }

        return braceDepth == 0 && bracketDepth == 0 && !inString;
    }

    /**
     * @brief Detect JSON value type at start of string
     *
     * @param json The JSON string
     * @return The detected type, or std::nullopt if invalid
     */
    [[nodiscard]] static std::optional<JsonType> detectType(std::string_view json) noexcept {
        // Skip whitespace
        size_t i = 0;
        while (i < json.size() && isWhitespace(json[i])) {
            ++i;
        }

        if (i >= json.size()) {
            return std::nullopt;
        }

        char c = json[i];
        switch (c) {
            case 'n':
                if (json.substr(i).starts_with("null")) {
                    return JsonType::Null;
                }
                break;
            case 't':
            case 'f':
                if (json.substr(i).starts_with("true") ||
                    json.substr(i).starts_with("false")) {
                    return JsonType::Boolean;
                }
                break;
            case '"':
                return JsonType::String;
            case '[':
                return JsonType::Array;
            case '{':
                return JsonType::Object;
            case '-':
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                return JsonType::Number;
            default:
                break;
        }

        return std::nullopt;
    }

    /**
     * @brief Escape a string for use in JSON
     *
     * @param input The string to escape
     * @return JSON-escaped string (without surrounding quotes)
     */
    [[nodiscard]] static std::string escape(std::string_view input) {
        std::string result;
        result.reserve(input.size() + input.size() / 8);

        for (char c : input) {
            switch (c) {
                case '"':
                    result += "\\\"";
                    break;
                case '\\':
                    result += "\\\\";
                    break;
                case '\b':
                    result += "\\b";
                    break;
                case '\f':
                    result += "\\f";
                    break;
                case '\n':
                    result += "\\n";
                    break;
                case '\r':
                    result += "\\r";
                    break;
                case '\t':
                    result += "\\t";
                    break;
                default:
                    if (static_cast<unsigned char>(c) < 0x20) {
                        result += "\\u00";
                        result += hexDigit((c >> 4) & 0x0F);
                        result += hexDigit(c & 0x0F);
                    } else {
                        result += c;
                    }
                    break;
            }
        }

        return result;
    }

    /**
     * @brief Unescape a JSON string value
     *
     * @param input The JSON string value (without surrounding quotes)
     * @return Unescaped string, or std::nullopt if invalid escape sequence
     */
    [[nodiscard]] static std::optional<std::string> unescape(std::string_view input) {
        std::string result;
        result.reserve(input.size());

        for (size_t i = 0; i < input.size(); ++i) {
            if (input[i] == '\\') {
                if (i + 1 >= input.size()) {
                    return std::nullopt;
                }

                ++i;
                switch (input[i]) {
                    case '"':
                        result += '"';
                        break;
                    case '\\':
                        result += '\\';
                        break;
                    case '/':
                        result += '/';
                        break;
                    case 'b':
                        result += '\b';
                        break;
                    case 'f':
                        result += '\f';
                        break;
                    case 'n':
                        result += '\n';
                        break;
                    case 'r':
                        result += '\r';
                        break;
                    case 't':
                        result += '\t';
                        break;
                    case 'u':
                        // Unicode escape: \uXXXX
                        if (i + 4 >= input.size()) {
                            return std::nullopt;
                        }
                        // Simplified: only handle ASCII range
                        {
                            auto hex = parseHex4(input.substr(i + 1, 4));
                            if (!hex) return std::nullopt;
                            if (*hex < 0x80) {
                                result += static_cast<char>(*hex);
                            } else {
                                // For non-ASCII, encode as UTF-8
                                encodeUtf8(result, *hex);
                            }
                            i += 4;
                        }
                        break;
                    default:
                        return std::nullopt;
                }
            } else {
                result += input[i];
            }
        }

        return result;
    }

    /**
     * @brief Extract a string value from JSON string literal
     *
     * @param json A JSON string including the surrounding quotes
     * @return The extracted string value, or std::nullopt if invalid
     */
    [[nodiscard]] static std::optional<std::string> extractString(std::string_view json) {
        // Skip whitespace
        size_t start = 0;
        while (start < json.size() && isWhitespace(json[start])) {
            ++start;
        }

        if (start >= json.size() || json[start] != '"') {
            return std::nullopt;
        }

        // Find closing quote
        size_t end = start + 1;
        bool escape = false;
        while (end < json.size()) {
            if (escape) {
                escape = false;
            } else if (json[end] == '\\') {
                escape = true;
            } else if (json[end] == '"') {
                break;
            }
            ++end;
        }

        if (end >= json.size()) {
            return std::nullopt;
        }

        return unescape(json.substr(start + 1, end - start - 1));
    }

    /**
     * @brief Count array elements in JSON array
     *
     * @param json A JSON array string
     * @return Number of elements, or std::nullopt if not a valid array
     */
    [[nodiscard]] static std::optional<size_t> countArrayElements(std::string_view json) {
        // Skip whitespace
        size_t i = 0;
        while (i < json.size() && isWhitespace(json[i])) {
            ++i;
        }

        if (i >= json.size() || json[i] != '[') {
            return std::nullopt;
        }
        ++i;

        // Skip whitespace after [
        while (i < json.size() && isWhitespace(json[i])) {
            ++i;
        }

        // Empty array
        if (i < json.size() && json[i] == ']') {
            return 0;
        }

        size_t count = 1;
        int depth = 1;
        bool inString = false;
        bool escape = false;

        while (i < json.size() && depth > 0) {
            if (escape) {
                escape = false;
                ++i;
                continue;
            }

            char c = json[i];
            if (inString) {
                if (c == '\\') {
                    escape = true;
                } else if (c == '"') {
                    inString = false;
                }
            } else {
                switch (c) {
                    case '"':
                        inString = true;
                        break;
                    case '[':
                    case '{':
                        ++depth;
                        break;
                    case ']':
                    case '}':
                        --depth;
                        break;
                    case ',':
                        if (depth == 1) {
                            ++count;
                        }
                        break;
                    default:
                        break;
                }
            }
            ++i;
        }

        if (depth != 0) {
            return std::nullopt;
        }

        return count;
    }

    /**
     * @brief Minify JSON by removing whitespace
     *
     * @param json The JSON string to minify
     * @return Minified JSON string
     */
    [[nodiscard]] static std::string minify(std::string_view json) {
        std::string result;
        result.reserve(json.size());

        bool inString = false;
        bool escape = false;

        for (char c : json) {
            if (escape) {
                result += c;
                escape = false;
                continue;
            }

            if (inString) {
                result += c;
                if (c == '\\') {
                    escape = true;
                } else if (c == '"') {
                    inString = false;
                }
            } else {
                if (c == '"') {
                    inString = true;
                    result += c;
                } else if (!isWhitespace(c)) {
                    result += c;
                }
            }
        }

        return result;
    }

private:
    [[nodiscard]] static constexpr bool isWhitespace(char c) noexcept {
        return c == ' ' || c == '\t' || c == '\n' || c == '\r';
    }

    [[nodiscard]] static constexpr char hexDigit(int value) noexcept {
        return static_cast<char>(value < 10 ? '0' + value : 'a' + value - 10);
    }

    [[nodiscard]] static std::optional<uint16_t> parseHex4(std::string_view hex) {
        if (hex.size() != 4) return std::nullopt;

        uint16_t result = 0;
        for (char c : hex) {
            result <<= 4;
            if (c >= '0' && c <= '9') {
                result |= c - '0';
            } else if (c >= 'a' && c <= 'f') {
                result |= c - 'a' + 10;
            } else if (c >= 'A' && c <= 'F') {
                result |= c - 'A' + 10;
            } else {
                return std::nullopt;
            }
        }
        return result;
    }

    static void encodeUtf8(std::string& result, uint16_t codepoint) {
        if (codepoint < 0x80) {
            result += static_cast<char>(codepoint);
        } else if (codepoint < 0x800) {
            result += static_cast<char>(0xC0 | (codepoint >> 6));
            result += static_cast<char>(0x80 | (codepoint & 0x3F));
        } else {
            result += static_cast<char>(0xE0 | (codepoint >> 12));
            result += static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F));
            result += static_cast<char>(0x80 | (codepoint & 0x3F));
        }
    }
};

} // namespace proven

#endif // PROVEN_SAFE_JSON_HPP
