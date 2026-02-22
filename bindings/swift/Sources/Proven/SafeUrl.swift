// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe URL parsing delegated to libproven FFI.
///
/// All parsing is performed by the formally verified Idris 2 core
/// via the Zig FFI bridge.

import CProven

/// Parsed URL components returned by SafeUrl.parse.
public struct ParsedUrl: Equatable, Sendable {
    public let scheme: String
    public let host: String
    public let port: UInt16?
    public let path: String
    public let query: String?
    public let fragment: String?
}

public enum SafeUrl {
    /// Parse a URL into its components.
    public static func parse(_ urlString: String) -> Result<ParsedUrl, ProvenError> {
        withStringBytes(urlString) { ptr, len in
            var result = proven_url_parse(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }

            let scheme: String
            if let sPtr = result.components.scheme, result.components.scheme_len > 0 {
                scheme = String(cString: sPtr)
            } else {
                scheme = ""
            }

            let host: String
            if let hPtr = result.components.host, result.components.host_len > 0 {
                host = String(cString: hPtr)
            } else {
                host = ""
            }

            let port: UInt16? = result.components.has_port ? result.components.port : nil

            let path: String
            if let pPtr = result.components.path, result.components.path_len > 0 {
                path = String(cString: pPtr)
            } else {
                path = "/"
            }

            let query: String?
            if let qPtr = result.components.query, result.components.query_len > 0 {
                query = String(cString: qPtr)
            } else {
                query = nil
            }

            let fragment: String?
            if let fPtr = result.components.fragment, result.components.fragment_len > 0 {
                fragment = String(cString: fPtr)
            } else {
                fragment = nil
            }

            // Free the C-allocated URL components.
            proven_url_free(&result.components)

            return .success(ParsedUrl(
                scheme: scheme,
                host: host,
                port: port,
                path: path,
                query: query,
                fragment: fragment
            ))
        }
    }
}
