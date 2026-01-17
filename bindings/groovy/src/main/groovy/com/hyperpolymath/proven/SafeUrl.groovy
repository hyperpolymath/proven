// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.hyperpolymath.proven

import groovy.transform.CompileStatic
import java.net.URI
import java.net.URISyntaxException
import java.net.URLEncoder
import java.net.URLDecoder

/**
 * Safe URL operations with validation and encoding.
 * Prevents URL manipulation attacks and ensures proper encoding.
 */
@CompileStatic
class SafeUrl {

    /** Maximum URL length. */
    static final int MAX_URL_LENGTH = 2048

    /** Allowed schemes for safe URLs. */
    static final Set<String> SAFE_SCHEMES = ['http', 'https', 'mailto', 'tel'] as Set

    /** Dangerous schemes to block. */
    static final Set<String> DANGEROUS_SCHEMES = ['javascript', 'data', 'vbscript', 'file'] as Set

    /**
     * Validate and parse a URL.
     */
    static Result<URI, String> parse(String url) {
        if (url == null || url.trim().isEmpty()) {
            return Result.err("URL is null or empty")
        }
        if (url.length() > MAX_URL_LENGTH) {
            return Result.err("URL exceeds maximum length of ${MAX_URL_LENGTH}")
        }

        try {
            URI uri = new URI(url)
            return Result.ok(uri)
        } catch (URISyntaxException e) {
            return Result.err("Invalid URL syntax: ${e.reason}")
        }
    }

    /**
     * Check if URL is valid.
     */
    static boolean isValid(String url) {
        parse(url).ok
    }

    /**
     * Check if URL uses a safe scheme.
     */
    static Result<Boolean, String> isSafeScheme(String url) {
        parse(url).map { URI uri ->
            String scheme = uri.scheme?.toLowerCase()
            scheme != null && SAFE_SCHEMES.contains(scheme)
        }
    }

    /**
     * Check if URL uses a dangerous scheme.
     */
    static boolean isDangerousScheme(String url) {
        parse(url).match(
            { URI uri ->
                String scheme = uri.scheme?.toLowerCase()
                scheme != null && DANGEROUS_SCHEMES.contains(scheme)
            },
            { String err -> false }
        )
    }

    /**
     * Get the scheme (protocol) of a URL.
     */
    static Result<String, String> getScheme(String url) {
        parse(url).flatMap { URI uri ->
            uri.scheme != null ?
                Result.ok(uri.scheme.toLowerCase()) :
                Result.err("No scheme in URL")
        }
    }

    /**
     * Get the host of a URL.
     */
    static Result<String, String> getHost(String url) {
        parse(url).flatMap { URI uri ->
            uri.host != null ?
                Result.ok(uri.host.toLowerCase()) :
                Result.err("No host in URL")
        }
    }

    /**
     * Get the port of a URL (or default for scheme).
     */
    static Result<Integer, String> getPort(String url) {
        parse(url).map { URI uri ->
            if (uri.port != -1) {
                return uri.port
            }
            // Return default port for scheme
            switch (uri.scheme?.toLowerCase()) {
                case 'http': return 80
                case 'https': return 443
                case 'ftp': return 21
                default: return -1
            }
        }
    }

    /**
     * Get the path of a URL.
     */
    static Result<String, String> getPath(String url) {
        parse(url).map { URI uri ->
            uri.path ?: "/"
        }
    }

    /**
     * Get the query string of a URL.
     */
    static Result<String, String> getQuery(String url) {
        parse(url).flatMap { URI uri ->
            uri.query != null ?
                Result.ok(uri.query) :
                Result.err("No query string in URL")
        }
    }

    /**
     * Get the fragment of a URL.
     */
    static Result<String, String> getFragment(String url) {
        parse(url).flatMap { URI uri ->
            uri.fragment != null ?
                Result.ok(uri.fragment) :
                Result.err("No fragment in URL")
        }
    }

    /**
     * Parse query string into map of parameters.
     */
    static Result<Map<String, List<String>>, String> parseQueryParams(String url) {
        def queryResult = getQuery(url)
        if (queryResult.err) {
            return Result.ok([:])  // No query string is valid
        }

        try {
            Map<String, List<String>> params = [:].withDefault { [] }
            String query = queryResult.value

            query.split('&').each { String pair ->
                if (pair.isEmpty()) return
                int eq = pair.indexOf('=')
                if (eq < 0) {
                    params[URLDecoder.decode(pair, 'UTF-8')] << ''
                } else {
                    String key = URLDecoder.decode(pair.substring(0, eq), 'UTF-8')
                    String value = URLDecoder.decode(pair.substring(eq + 1), 'UTF-8')
                    params[key] << value
                }
            }

            return Result.ok(params)
        } catch (Exception e) {
            return Result.err("Failed to parse query: ${e.message}")
        }
    }

    /**
     * Safely encode a URL component.
     */
    static String encode(String component) {
        if (component == null) return ""
        try {
            URLEncoder.encode(component, 'UTF-8')
                      .replace('+', '%20')
                      .replace('%7E', '~')
        } catch (Exception e) {
            ""
        }
    }

    /**
     * Safely decode a URL component.
     */
    static Result<String, String> decode(String encoded) {
        if (encoded == null) {
            return Result.err("Input is null")
        }
        try {
            return Result.ok(URLDecoder.decode(encoded, 'UTF-8'))
        } catch (Exception e) {
            return Result.err("Failed to decode: ${e.message}")
        }
    }

    /**
     * Build a URL from components.
     */
    static Result<String, String> build(String scheme, String host, Integer port = null,
                                        String path = null, Map<String, String> params = null,
                                        String fragment = null) {
        if (scheme == null || host == null) {
            return Result.err("Scheme and host are required")
        }

        try {
            StringBuilder url = new StringBuilder()
            url.append(scheme.toLowerCase())
            url.append("://")
            url.append(host.toLowerCase())

            if (port != null && port > 0) {
                url.append(':')
                url.append(port)
            }

            if (path != null && !path.isEmpty()) {
                if (!path.startsWith('/')) {
                    url.append('/')
                }
                url.append(path)
            }

            if (params != null && !params.isEmpty()) {
                url.append('?')
                url.append(params.collect { k, v -> "${encode(k)}=${encode(v)}" }.join('&'))
            }

            if (fragment != null && !fragment.isEmpty()) {
                url.append('#')
                url.append(encode(fragment))
            }

            String result = url.toString()
            if (result.length() > MAX_URL_LENGTH) {
                return Result.err("Constructed URL exceeds maximum length")
            }

            return Result.ok(result)
        } catch (Exception e) {
            return Result.err("Failed to build URL: ${e.message}")
        }
    }

    /**
     * Normalize a URL (lowercase scheme/host, remove default ports).
     */
    static Result<String, String> normalize(String url) {
        parse(url).flatMap { URI uri ->
            try {
                String scheme = uri.scheme?.toLowerCase()
                String host = uri.host?.toLowerCase()

                int port = uri.port
                // Remove default ports
                if ((scheme == 'http' && port == 80) || (scheme == 'https' && port == 443)) {
                    port = -1
                }

                URI normalized = new URI(scheme, uri.userInfo, host, port,
                                         uri.path, uri.query, uri.fragment)
                return Result.ok(normalized.toString())
            } catch (Exception e) {
                return Result.err("Failed to normalize: ${e.message}")
            }
        }
    }

    /**
     * Check if URL is relative.
     */
    static boolean isRelative(String url) {
        parse(url).match(
            { URI uri -> uri.scheme == null },
            { String err -> false }
        )
    }

    /**
     * Resolve a relative URL against a base URL.
     */
    static Result<String, String> resolve(String base, String relative) {
        parse(base).flatMap { URI baseUri ->
            try {
                URI resolved = baseUri.resolve(relative)
                return Result.ok(resolved.toString())
            } catch (Exception e) {
                return Result.err("Failed to resolve: ${e.message}")
            }
        }
    }

    /**
     * Check if two URLs are equivalent after normalization.
     */
    static boolean equivalent(String url1, String url2) {
        def n1 = normalize(url1)
        def n2 = normalize(url2)
        n1.ok && n2.ok && n1.value == n2.value
    }
}
