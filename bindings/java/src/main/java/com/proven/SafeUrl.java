// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.proven;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.Optional;

/**
 * Safe URL parsing and validation.
 * Provides URL component extraction and encoding.
 * Calls native verified code via JNI when available.
 */
public final class SafeUrl {
    private SafeUrl() {}

    /**
     * Parsed URL components.
     */
    public record UrlComponents(
        String scheme,
        String host,
        Optional<Integer> port,
        String path,
        Optional<String> query,
        Optional<String> fragment
    ) {
        /**
         * Get the effective port (default for scheme if not specified).
         */
        public int getEffectivePort() {
            return port.orElseGet(() -> switch (scheme.toLowerCase()) {
                case "http" -> 80;
                case "https" -> 443;
                case "ftp" -> 21;
                case "ssh" -> 22;
                case "ws" -> 80;
                case "wss" -> 443;
                default -> -1;
            });
        }

        /**
         * Reconstruct the URL string.
         */
        public String toUrlString() {
            StringBuilder sb = new StringBuilder();
            sb.append(scheme).append("://").append(host);
            port.ifPresent(p -> sb.append(":").append(p));
            sb.append(path.isEmpty() ? "/" : path);
            query.ifPresent(q -> sb.append("?").append(q));
            fragment.ifPresent(f -> sb.append("#").append(f));
            return sb.toString();
        }
    }

    /**
     * Check if URL is valid.
     */
    public static boolean isValid(String url) {
        if (url == null || url.isEmpty()) return false;
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeUrlIsValid(url.getBytes(StandardCharsets.UTF_8));
        }
        return isValidPure(url);
    }

    /**
     * Parse URL into components.
     */
    public static ProvenResult<UrlComponents> parse(String url) {
        if (url == null || url.isEmpty()) {
            return ProvenResult.err(ProvenStatus.ERR_EMPTY_INPUT, "Empty URL");
        }

        try {
            URI uri = new URI(url);

            String scheme = uri.getScheme();
            if (scheme == null) {
                return ProvenResult.err(ProvenStatus.ERR_PARSE_FAILURE, "Missing scheme");
            }

            String host = uri.getHost();
            if (host == null) {
                return ProvenResult.err(ProvenStatus.ERR_PARSE_FAILURE, "Missing host");
            }

            int port = uri.getPort();
            String path = uri.getPath();
            String query = uri.getQuery();
            String fragment = uri.getFragment();

            return ProvenResult.ok(new UrlComponents(
                scheme,
                host,
                port > 0 ? Optional.of(port) : Optional.empty(),
                path != null ? path : "",
                Optional.ofNullable(query),
                Optional.ofNullable(fragment)
            ));
        } catch (URISyntaxException e) {
            return ProvenResult.err(ProvenStatus.ERR_PARSE_FAILURE, "Invalid URL: " + e.getMessage());
        }
    }

    /**
     * URL-encode a string.
     */
    public static String encode(String input) {
        if (input == null) return "";
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeUrlEncode(
                input.getBytes(StandardCharsets.UTF_8), status);
            if (ProvenStatus.fromCode(status[0]).isOk() && result != null) {
                return new String(result, StandardCharsets.UTF_8);
            }
        }
        return encodePure(input);
    }

    /**
     * URL-decode a string.
     */
    public static ProvenResult<String> decode(String input) {
        if (input == null || input.isEmpty()) {
            return ProvenResult.ok("");
        }
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            byte[] result = ProvenNative.nativeUrlDecode(
                input.getBytes(StandardCharsets.UTF_8), status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            if (s.isOk() && result != null) {
                return ProvenResult.ok(new String(result, StandardCharsets.UTF_8));
            }
            return ProvenResult.err(s);
        }
        return decodePure(input);
    }

    /**
     * Extract query parameters from a URL.
     */
    public static java.util.Map<String, String> getQueryParams(String url) {
        java.util.Map<String, String> params = new java.util.LinkedHashMap<>();
        var parsed = parse(url);
        if (parsed.isErr()) return params;

        parsed.unwrap().query().ifPresent(query -> {
            for (String param : query.split("&")) {
                String[] keyValue = param.split("=", 2);
                if (keyValue.length > 0) {
                    String key = decode(keyValue[0]).unwrapOr(keyValue[0]);
                    String value = keyValue.length > 1 ? decode(keyValue[1]).unwrapOr(keyValue[1]) : "";
                    params.put(key, value);
                }
            }
        });

        return params;
    }

    /**
     * Build a URL with query parameters.
     */
    public static String buildUrl(String base, java.util.Map<String, String> params) {
        if (params == null || params.isEmpty()) return base;

        StringBuilder sb = new StringBuilder(base);
        sb.append(base.contains("?") ? "&" : "?");

        boolean first = true;
        for (var entry : params.entrySet()) {
            if (!first) sb.append("&");
            sb.append(encode(entry.getKey()));
            sb.append("=");
            sb.append(encode(entry.getValue()));
            first = false;
        }

        return sb.toString();
    }

    /**
     * Check if URL uses a secure scheme (https, wss).
     */
    public static boolean isSecure(String url) {
        return parse(url)
            .map(c -> c.scheme().equalsIgnoreCase("https") || c.scheme().equalsIgnoreCase("wss"))
            .unwrapOr(false);
    }

    /**
     * Check if URL points to localhost.
     */
    public static boolean isLocalhost(String url) {
        return parse(url)
            .map(c -> {
                String host = c.host().toLowerCase();
                return host.equals("localhost") ||
                       host.equals("127.0.0.1") ||
                       host.equals("::1") ||
                       host.startsWith("127.");
            })
            .unwrapOr(false);
    }

    // Pure Java fallback implementations

    private static boolean isValidPure(String url) {
        try {
            URI uri = new URI(url);
            return uri.getScheme() != null && uri.getHost() != null;
        } catch (URISyntaxException e) {
            return false;
        }
    }

    private static String encodePure(String input) {
        StringBuilder sb = new StringBuilder();
        for (byte b : input.getBytes(StandardCharsets.UTF_8)) {
            int i = b & 0xFF;
            if ((i >= 'A' && i <= 'Z') || (i >= 'a' && i <= 'z') ||
                (i >= '0' && i <= '9') || i == '-' || i == '_' || i == '.' || i == '~') {
                sb.append((char) i);
            } else {
                sb.append(String.format("%%%02X", i));
            }
        }
        return sb.toString();
    }

    private static ProvenResult<String> decodePure(String input) {
        try {
            StringBuilder sb = new StringBuilder();
            byte[] bytes = input.getBytes(StandardCharsets.UTF_8);
            java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();

            for (int i = 0; i < bytes.length; i++) {
                byte b = bytes[i];
                if (b == '%' && i + 2 < bytes.length) {
                    int hi = Character.digit((char) bytes[i + 1], 16);
                    int lo = Character.digit((char) bytes[i + 2], 16);
                    if (hi >= 0 && lo >= 0) {
                        baos.write((hi << 4) | lo);
                        i += 2;
                        continue;
                    }
                }
                if (b == '+') {
                    baos.write(' ');
                } else {
                    baos.write(b);
                }
            }

            return ProvenResult.ok(baos.toString(StandardCharsets.UTF_8));
        } catch (Exception e) {
            return ProvenResult.err(ProvenStatus.ERR_PARSE_FAILURE, "Invalid URL encoding");
        }
    }
}
