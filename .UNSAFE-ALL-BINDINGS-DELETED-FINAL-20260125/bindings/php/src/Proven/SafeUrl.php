<?php
// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Parsed URL components.
 */
readonly class ParsedUrl
{
    public function __construct(
        /** URL scheme (e.g., "https") */
        public string $scheme,
        /** Host (domain or IP) */
        public string $host,
        /** Path component */
        public string $path,
        /** Optional port */
        public ?int $port = null,
        /** Optional username */
        public ?string $username = null,
        /** Optional password */
        public ?string $password = null,
        /** Optional query string (without ?) */
        public ?string $query = null,
        /** Optional fragment (without #) */
        public ?string $fragment = null,
    ) {}

    /**
     * Reconstruct the URL string.
     */
    public function toString(): string
    {
        $url = $this->scheme . '://';

        if ($this->username !== null) {
            $url .= rawurlencode($this->username);
            if ($this->password !== null) {
                $url .= ':' . rawurlencode($this->password);
            }
            $url .= '@';
        }

        $url .= $this->host;

        if ($this->port !== null) {
            $url .= ':' . $this->port;
        }

        $url .= $this->path;

        if ($this->query !== null) {
            $url .= '?' . $this->query;
        }

        if ($this->fragment !== null) {
            $url .= '#' . $this->fragment;
        }

        return $url;
    }

    /**
     * Get the origin (scheme + host + port).
     */
    public function getOrigin(): string
    {
        $origin = $this->scheme . '://' . $this->host;
        if ($this->port !== null) {
            $origin .= ':' . $this->port;
        }
        return $origin;
    }

    /**
     * Check if this URL uses a secure scheme (https, wss).
     */
    public function isSecure(): bool
    {
        return in_array($this->scheme, ['https', 'wss'], true);
    }

    public function __toString(): string
    {
        return $this->toString();
    }
}

/**
 * Safe URL parsing and manipulation.
 *
 * Provides RFC 3986 compliant URL parsing with safe handling of edge cases.
 */
class SafeUrl
{
    /**
     * Parse a URL string.
     *
     * @param string $url The URL to parse
     * @return Result<ParsedUrl>
     */
    public static function parse(string $url): Result
    {
        $url = trim($url);
        if ($url === '') {
            return Result::err(ProvenError::emptyInput());
        }

        // Extract scheme
        $schemeEnd = strpos($url, '://');
        if ($schemeEnd === false) {
            return Result::err(ProvenError::invalidFormat('Missing scheme'));
        }

        $scheme = strtolower(substr($url, 0, $schemeEnd));
        $rest = substr($url, $schemeEnd + 3);

        // Extract fragment
        $fragment = null;
        $fragmentPos = strrpos($rest, '#');
        if ($fragmentPos !== false) {
            $fragment = substr($rest, $fragmentPos + 1);
            $rest = substr($rest, 0, $fragmentPos);
        }

        // Extract query
        $query = null;
        $queryPos = strpos($rest, '?');
        if ($queryPos !== false) {
            $query = substr($rest, $queryPos + 1);
            $rest = substr($rest, 0, $queryPos);
        }

        // Extract path
        $pathPos = strpos($rest, '/');
        if ($pathPos !== false) {
            $path = substr($rest, $pathPos);
            $authority = substr($rest, 0, $pathPos);
        } else {
            $path = '/';
            $authority = $rest;
        }

        // Extract userinfo
        $username = null;
        $password = null;
        $atPos = strrpos($authority, '@');
        if ($atPos !== false) {
            $userinfo = substr($authority, 0, $atPos);
            $authority = substr($authority, $atPos + 1);

            $colonPos = strpos($userinfo, ':');
            if ($colonPos !== false) {
                $username = rawurldecode(substr($userinfo, 0, $colonPos));
                $password = rawurldecode(substr($userinfo, $colonPos + 1));
            } else {
                $username = rawurldecode($userinfo);
            }
        }

        // Extract port
        $port = null;
        $host = $authority;

        if (str_starts_with($authority, '[')) {
            // IPv6 address
            $bracketEnd = strpos($authority, ']');
            if ($bracketEnd === false) {
                return Result::err(ProvenError::invalidFormat('Invalid IPv6 address'));
            }
            $host = substr($authority, 1, $bracketEnd - 1);
            $afterBracket = substr($authority, $bracketEnd + 1);
            if (str_starts_with($afterBracket, ':')) {
                $portStr = substr($afterBracket, 1);
                if ($portStr !== '' && ctype_digit($portStr)) {
                    $port = (int)$portStr;
                    if ($port < 0 || $port > 65535) {
                        return Result::err(ProvenError::invalidFormat('Port out of range'));
                    }
                }
            }
        } else {
            // IPv4 or hostname
            $colonPos = strrpos($authority, ':');
            if ($colonPos !== false) {
                $portStr = substr($authority, $colonPos + 1);
                if (ctype_digit($portStr)) {
                    $port = (int)$portStr;
                    if ($port < 0 || $port > 65535) {
                        return Result::err(ProvenError::invalidFormat('Port out of range'));
                    }
                    $host = substr($authority, 0, $colonPos);
                }
            }
        }

        if ($host === '') {
            return Result::err(ProvenError::invalidFormat('Missing host'));
        }

        return Result::ok(new ParsedUrl(
            scheme: $scheme,
            host: $host,
            path: $path,
            port: $port,
            username: $username,
            password: $password,
            query: $query,
            fragment: $fragment,
        ));
    }

    /**
     * Check if a URL string is valid.
     *
     * @param string $url The URL to validate
     */
    public static function isValid(string $url): bool
    {
        return self::parse($url)->isOk();
    }

    /**
     * Extract the domain from a URL.
     *
     * @param string $url The URL
     * @return Result<string>
     */
    public static function getDomain(string $url): Result
    {
        return self::parse($url)->map(fn(ParsedUrl $parsed) => $parsed->host);
    }

    /**
     * Extract the scheme from a URL.
     *
     * @param string $url The URL
     * @return Result<string>
     */
    public static function getScheme(string $url): Result
    {
        return self::parse($url)->map(fn(ParsedUrl $parsed) => $parsed->scheme);
    }

    /**
     * Extract the path from a URL.
     *
     * @param string $url The URL
     * @return Result<string>
     */
    public static function getPath(string $url): Result
    {
        return self::parse($url)->map(fn(ParsedUrl $parsed) => $parsed->path);
    }

    /**
     * Parse query string into an associative array.
     *
     * @param string $queryString The query string (without leading ?)
     * @return array<string, string|array<string>>
     */
    public static function parseQueryString(string $queryString): array
    {
        $result = [];
        parse_str($queryString, $result);
        return $result;
    }

    /**
     * Build a query string from an associative array.
     *
     * @param array<string, mixed> $params The parameters
     */
    public static function buildQueryString(array $params): string
    {
        return http_build_query($params, '', '&', PHP_QUERY_RFC3986);
    }

    /**
     * Safely join a base URL with a relative path.
     *
     * @param string $base The base URL
     * @param string $relative The relative path
     * @return Result<string>
     */
    public static function join(string $base, string $relative): Result
    {
        $parsed = self::parse($base);
        if ($parsed->isErr()) {
            return $parsed;
        }

        /** @var ParsedUrl $baseUrl */
        $baseUrl = $parsed->unwrap();

        // Handle absolute URLs
        if (str_contains($relative, '://')) {
            return self::parse($relative)->map(fn(ParsedUrl $p) => $p->toString());
        }

        // Handle protocol-relative URLs
        if (str_starts_with($relative, '//')) {
            return self::parse($baseUrl->scheme . ':' . $relative)->map(fn(ParsedUrl $p) => $p->toString());
        }

        // Handle absolute paths
        if (str_starts_with($relative, '/')) {
            return Result::ok($baseUrl->getOrigin() . $relative);
        }

        // Handle relative paths
        $basePath = dirname($baseUrl->path);
        if ($basePath === '\\' || $basePath === '.') {
            $basePath = '/';
        }
        $newPath = rtrim($basePath, '/') . '/' . $relative;

        // Normalize path (remove . and ..)
        $parts = explode('/', $newPath);
        $normalized = [];
        foreach ($parts as $part) {
            if ($part === '..') {
                array_pop($normalized);
            } elseif ($part !== '.' && $part !== '') {
                $normalized[] = $part;
            }
        }
        $finalPath = '/' . implode('/', $normalized);

        return Result::ok($baseUrl->getOrigin() . $finalPath);
    }

    /**
     * Check if a URL is using a secure scheme (https, wss).
     *
     * @param string $url The URL to check
     */
    public static function isSecure(string $url): bool
    {
        $parsed = self::parse($url);
        if ($parsed->isErr()) {
            return false;
        }
        return $parsed->unwrap()->isSecure();
    }

    /**
     * Normalize a URL to a canonical form.
     *
     * @param string $url The URL to normalize
     * @return Result<string>
     */
    public static function normalize(string $url): Result
    {
        return self::parse($url)->map(fn(ParsedUrl $parsed) => $parsed->toString());
    }
}
