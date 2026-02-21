// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe JSON parsing and validation operations.
 * Provides JSON syntax validation without external dependencies.
 */
module proven.safe_json;

import std.array : appender;
import std.typecons : Nullable, nullable;

/// JSON value types.
enum JsonType
{
    null_,
    boolean,
    number,
    string_,
    array,
    object,
}

/// JSON validation result.
struct JsonValidationResult
{
    bool valid;
    string error;
    size_t errorPosition;

    static JsonValidationResult success()
    {
        return JsonValidationResult(true, "", 0);
    }

    static JsonValidationResult failure(string error, size_t position)
    {
        return JsonValidationResult(false, error, position);
    }
}

/// Validate JSON syntax without parsing.
bool isValidJson(string jsonString) pure nothrow @safe
{
    return validateJson(jsonString).valid;
}

/// Validate JSON and return detailed result.
JsonValidationResult validateJson(string jsonString) pure nothrow @safe
{
    if (jsonString.length == 0)
        return JsonValidationResult.failure("Empty input", 0);

    int depthBrace = 0;
    int depthBracket = 0;
    bool inString = false;
    bool escape = false;
    size_t position = 0;

    foreach (i, c; jsonString)
    {
        position = i;

        if (escape)
        {
            escape = false;
            continue;
        }

        if (inString)
        {
            if (c == '\\')
            {
                escape = true;
            }
            else if (c == '"')
            {
                inString = false;
            }
            continue;
        }

        switch (c)
        {
        case '"':
            inString = true;
            break;
        case '{':
            depthBrace++;
            break;
        case '}':
            depthBrace--;
            if (depthBrace < 0)
                return JsonValidationResult.failure("Unexpected }", i);
            break;
        case '[':
            depthBracket++;
            break;
        case ']':
            depthBracket--;
            if (depthBracket < 0)
                return JsonValidationResult.failure("Unexpected ]", i);
            break;
        default:
            break;
        }
    }

    if (inString)
        return JsonValidationResult.failure("Unclosed string", position);
    if (depthBrace != 0)
        return JsonValidationResult.failure("Unbalanced braces", position);
    if (depthBracket != 0)
        return JsonValidationResult.failure("Unbalanced brackets", position);

    return JsonValidationResult.success();
}

/// Check if character is whitespace in JSON.
private bool isJsonWhitespace(char c) pure nothrow @safe @nogc
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

/// Skip whitespace and return remaining string.
private string skipWhitespace(string s) pure nothrow @safe
{
    size_t i = 0;
    while (i < s.length && isJsonWhitespace(s[i]))
        i++;
    return s[i .. $];
}

/// Detect the type of a JSON value.
Nullable!JsonType detectJsonType(string jsonString) pure nothrow @safe
{
    auto trimmed = skipWhitespace(jsonString);
    if (trimmed.length == 0)
        return Nullable!JsonType.init;

    switch (trimmed[0])
    {
    case 'n':
        return nullable(JsonType.null_);
    case 't', 'f':
        return nullable(JsonType.boolean);
    case '"':
        return nullable(JsonType.string_);
    case '[':
        return nullable(JsonType.array);
    case '{':
        return nullable(JsonType.object);
    case '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
        return nullable(JsonType.number);
    default:
        return Nullable!JsonType.init;
    }
}

/// Escape a string for JSON output.
string escapeJsonString(string input) pure @safe
{
    auto result = appender!string;
    result.reserve(input.length + input.length / 4);
    result ~= '"';

    foreach (c; input)
    {
        switch (c)
        {
        case '"':
            result ~= "\\\"";
            break;
        case '\\':
            result ~= "\\\\";
            break;
        case '\b':
            result ~= "\\b";
            break;
        case '\f':
            result ~= "\\f";
            break;
        case '\n':
            result ~= "\\n";
            break;
        case '\r':
            result ~= "\\r";
            break;
        case '\t':
            result ~= "\\t";
            break;
        default:
            if (c < 0x20)
            {
                import std.format : format;
                result ~= format!"\\u%04x"(cast(uint) c);
            }
            else
            {
                result ~= c;
            }
        }
    }

    result ~= '"';
    return result[];
}

/// Unescape a JSON string.
Nullable!string unescapeJsonString(string input) pure @safe
{
    if (input.length < 2 || input[0] != '"' || input[$ - 1] != '"')
        return Nullable!string.init;

    auto content = input[1 .. $ - 1];
    auto result = appender!string;
    result.reserve(content.length);

    bool escape = false;
    foreach (i, c; content)
    {
        if (escape)
        {
            switch (c)
            {
            case '"':
                result ~= '"';
                break;
            case '\\':
                result ~= '\\';
                break;
            case 'b':
                result ~= '\b';
                break;
            case 'f':
                result ~= '\f';
                break;
            case 'n':
                result ~= '\n';
                break;
            case 'r':
                result ~= '\r';
                break;
            case 't':
                result ~= '\t';
                break;
            default:
                result ~= c;
            }
            escape = false;
        }
        else if (c == '\\')
        {
            escape = true;
        }
        else
        {
            result ~= c;
        }
    }

    if (escape)
        return Nullable!string.init;

    return nullable(result[]);
}

/// Check if a string looks like a JSON object.
bool looksLikeJsonObject(string jsonString) pure nothrow @safe
{
    auto trimmed = skipWhitespace(jsonString);
    return trimmed.length > 0 && trimmed[0] == '{';
}

/// Check if a string looks like a JSON array.
bool looksLikeJsonArray(string jsonString) pure nothrow @safe
{
    auto trimmed = skipWhitespace(jsonString);
    return trimmed.length > 0 && trimmed[0] == '[';
}

/// Count approximate depth of JSON nesting.
size_t countJsonDepth(string jsonString) pure nothrow @safe
{
    size_t maxDepth = 0;
    size_t currentDepth = 0;
    bool inString = false;
    bool escape = false;

    foreach (c; jsonString)
    {
        if (escape)
        {
            escape = false;
            continue;
        }

        if (inString)
        {
            if (c == '\\')
                escape = true;
            else if (c == '"')
                inString = false;
            continue;
        }

        switch (c)
        {
        case '"':
            inString = true;
            break;
        case '{', '[':
            currentDepth++;
            if (currentDepth > maxDepth)
                maxDepth = currentDepth;
            break;
        case '}', ']':
            if (currentDepth > 0)
                currentDepth--;
            break;
        default:
            break;
        }
    }

    return maxDepth;
}

// Unit tests
unittest
{
    // Test valid JSON
    assert(isValidJson(`{"key": "value"}`));
    assert(isValidJson(`[1, 2, 3]`));
    assert(isValidJson(`{"nested": {"a": 1}}`));
    assert(isValidJson(`"hello"`));
    assert(isValidJson(`123`));
    assert(isValidJson(`true`));
    assert(isValidJson(`null`));

    // Test invalid JSON
    assert(!isValidJson(`{"unclosed"`));
    assert(!isValidJson(`{"extra": }}`));
    assert(!isValidJson(``));

    // Test type detection
    assert(detectJsonType(`{}`).get == JsonType.object);
    assert(detectJsonType(`[]`).get == JsonType.array);
    assert(detectJsonType(`"hello"`).get == JsonType.string_);
    assert(detectJsonType(`123`).get == JsonType.number);
    assert(detectJsonType(`true`).get == JsonType.boolean);
    assert(detectJsonType(`null`).get == JsonType.null_);

    // Test escaping
    assert(escapeJsonString("hello\nworld") == `"hello\nworld"`);

    // Test depth counting
    assert(countJsonDepth(`{"a": {"b": {"c": 1}}}`) == 3);
}
