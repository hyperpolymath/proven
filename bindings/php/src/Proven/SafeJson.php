<?php
// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Safe JSON parsing and manipulation.
 *
 * Provides exception-free JSON operations with proper error handling.
 */
class SafeJson
{
    /**
     * Parse JSON from a string.
     *
     * @param string $json The JSON string to parse
     * @param bool $associative Whether to return arrays instead of objects
     * @return Result<mixed>
     */
    public static function parse(string $json, bool $associative = true): Result
    {
        $json = trim($json);
        if ($json === '') {
            return Result::err(ProvenError::emptyInput());
        }

        $result = json_decode($json, $associative);
        $error = json_last_error();

        if ($error !== JSON_ERROR_NONE) {
            return Result::err(ProvenError::parseError(json_last_error_msg()));
        }

        return Result::ok($result);
    }

    /**
     * Encode a value to JSON.
     *
     * @param mixed $value The value to encode
     * @param int $flags JSON encoding flags
     * @return Result<string>
     */
    public static function encode(mixed $value, int $flags = 0): Result
    {
        $flags |= JSON_THROW_ON_ERROR;

        try {
            $result = json_encode($value, $flags);
            if ($result === false) {
                return Result::err(ProvenError::invalidFormat('Failed to encode JSON'));
            }
            return Result::ok($result);
        } catch (\JsonException $e) {
            return Result::err(ProvenError::invalidFormat($e->getMessage()));
        }
    }

    /**
     * Encode a value to pretty-printed JSON.
     *
     * @param mixed $value The value to encode
     * @return Result<string>
     */
    public static function encodePretty(mixed $value): Result
    {
        return self::encode($value, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    }

    /**
     * Validate JSON syntax without fully parsing.
     *
     * @param string $json The JSON string to validate
     */
    public static function isValid(string $json): bool
    {
        $json = trim($json);
        if ($json === '') {
            return false;
        }

        // Use a simple balanced delimiter check for performance
        $depthBrace = 0;
        $depthBracket = 0;
        $inString = false;
        $escape = false;
        $length = strlen($json);

        for ($i = 0; $i < $length; $i++) {
            $char = $json[$i];

            if ($escape) {
                $escape = false;
                continue;
            }

            if ($char === '\\' && $inString) {
                $escape = true;
                continue;
            }

            if ($char === '"') {
                $inString = !$inString;
                continue;
            }

            if ($inString) {
                continue;
            }

            switch ($char) {
                case '{':
                    $depthBrace++;
                    break;
                case '}':
                    $depthBrace--;
                    break;
                case '[':
                    $depthBracket++;
                    break;
                case ']':
                    $depthBracket--;
                    break;
            }

            if ($depthBrace < 0 || $depthBracket < 0) {
                return false;
            }
        }

        return $depthBrace === 0 && $depthBracket === 0 && !$inString;
    }

    /**
     * Get a value at a JSON path (dot notation).
     *
     * @param array<mixed>|object $data The parsed JSON data
     * @param string $path The path (e.g., "user.name" or "items.0.id")
     * @return Option<mixed>
     */
    public static function get(array|object $data, string $path): Option
    {
        $keys = explode('.', $path);
        $current = $data;

        foreach ($keys as $key) {
            if (is_array($current)) {
                if (!array_key_exists($key, $current)) {
                    return Option::none();
                }
                $current = $current[$key];
            } elseif (is_object($current)) {
                if (!property_exists($current, $key)) {
                    return Option::none();
                }
                $current = $current->$key;
            } else {
                return Option::none();
            }
        }

        return Option::some($current);
    }

    /**
     * Get a string value at a JSON path.
     *
     * @param array<mixed>|object $data The parsed JSON data
     * @param string $path The path
     * @return Option<string>
     */
    public static function getString(array|object $data, string $path): Option
    {
        return self::get($data, $path)->andThen(function ($value) {
            return is_string($value) ? Option::some($value) : Option::none();
        });
    }

    /**
     * Get an integer value at a JSON path.
     *
     * @param array<mixed>|object $data The parsed JSON data
     * @param string $path The path
     * @return Option<int>
     */
    public static function getInt(array|object $data, string $path): Option
    {
        return self::get($data, $path)->andThen(function ($value) {
            if (is_int($value)) {
                return Option::some($value);
            }
            if (is_float($value) && floor($value) === $value) {
                return Option::some((int)$value);
            }
            return Option::none();
        });
    }

    /**
     * Get a float value at a JSON path.
     *
     * @param array<mixed>|object $data The parsed JSON data
     * @param string $path The path
     * @return Option<float>
     */
    public static function getFloat(array|object $data, string $path): Option
    {
        return self::get($data, $path)->andThen(function ($value) {
            if (is_float($value) || is_int($value)) {
                return Option::some((float)$value);
            }
            return Option::none();
        });
    }

    /**
     * Get a boolean value at a JSON path.
     *
     * @param array<mixed>|object $data The parsed JSON data
     * @param string $path The path
     * @return Option<bool>
     */
    public static function getBool(array|object $data, string $path): Option
    {
        return self::get($data, $path)->andThen(function ($value) {
            return is_bool($value) ? Option::some($value) : Option::none();
        });
    }

    /**
     * Get an array value at a JSON path.
     *
     * @param array<mixed>|object $data The parsed JSON data
     * @param string $path The path
     * @return Option<array<mixed>>
     */
    public static function getArray(array|object $data, string $path): Option
    {
        return self::get($data, $path)->andThen(function ($value) {
            return is_array($value) ? Option::some($value) : Option::none();
        });
    }

    /**
     * Merge two JSON objects/arrays deeply.
     *
     * @param array<mixed> $base The base array
     * @param array<mixed> $overlay The overlay array
     * @return array<mixed>
     */
    public static function deepMerge(array $base, array $overlay): array
    {
        foreach ($overlay as $key => $value) {
            if (is_array($value) && isset($base[$key]) && is_array($base[$key])) {
                $base[$key] = self::deepMerge($base[$key], $value);
            } else {
                $base[$key] = $value;
            }
        }
        return $base;
    }

    /**
     * Compare two JSON values for equality.
     *
     * @param mixed $a First value
     * @param mixed $b Second value
     */
    public static function equals(mixed $a, mixed $b): bool
    {
        $jsonA = self::encode($a);
        $jsonB = self::encode($b);

        if ($jsonA->isErr() || $jsonB->isErr()) {
            return false;
        }

        return $jsonA->unwrap() === $jsonB->unwrap();
    }

    /**
     * Flatten a nested JSON structure.
     *
     * @param array<mixed> $data The data to flatten
     * @param string $prefix Key prefix
     * @param string $separator Key separator
     * @return array<string, mixed>
     */
    public static function flatten(array $data, string $prefix = '', string $separator = '.'): array
    {
        $result = [];

        foreach ($data as $key => $value) {
            $newKey = $prefix === '' ? (string)$key : $prefix . $separator . $key;

            if (is_array($value) && !empty($value) && array_keys($value) !== range(0, count($value) - 1)) {
                // Associative array - recurse
                $result = array_merge($result, self::flatten($value, $newKey, $separator));
            } else {
                $result[$newKey] = $value;
            }
        }

        return $result;
    }
}
