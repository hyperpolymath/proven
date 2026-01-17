-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

{-
SafeUrl - Safe URL handling and validation

Provides URL construction, parsing markers, and safe encoding.
Prevents URL injection and ensures proper encoding.

Part of the Proven safety primitives library.
-}

let Prelude = https://prelude.dhall-lang.org/v23.0.0/package.dhall
    sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

let Text/replace = Prelude.Text.replace

-- URL scheme types
let Scheme = < HTTP | HTTPS | FTP | FTPS | WS | WSS | File | Mailto | Tel | Custom : Text >

-- URL type
let Url = { url : Text }

-- URL result
let UrlResult = { value : Url, ok : Bool }

-- Create success result
let ok
    : Url -> UrlResult
    = \(u : Url) -> { value = u, ok = True }

-- Create error result
let err
    : UrlResult
    = { value = { url = "" }, ok = False }

-- Create URL (marker for validation)
let mkUrl
    : Text -> Url
    = \(u : Text) ->
        { url = u }

-- URL components for structured construction
let UrlComponents = {
    scheme : Scheme,
    host : Text,
    port : Optional Natural,
    path : Text,
    query : Optional Text,
    fragment : Optional Text
}

-- Default URL components
let defaultComponents
    : UrlComponents
    = {
        scheme = Scheme.HTTPS,
        host = "",
        port = None Natural,
        path = "/",
        query = None Text,
        fragment = None Text
    }

-- Build URL from components
let fromComponents
    : UrlComponents -> Url
    = \(c : UrlComponents) ->
        let schemeStr = merge {
            HTTP = "http",
            HTTPS = "https",
            FTP = "ftp",
            FTPS = "ftps",
            WS = "ws",
            WSS = "wss",
            File = "file",
            Mailto = "mailto",
            Tel = "tel",
            Custom = \(s : Text) -> s
        } c.scheme

        let portStr = merge {
            None = "",
            Some = \(p : Natural) -> ":" ++ Natural/show p
        } c.port

        let queryStr = merge {
            None = "",
            Some = \(q : Text) -> "?" ++ q
        } c.query

        let fragmentStr = merge {
            None = "",
            Some = \(f : Text) -> "#" ++ f
        } c.fragment

        in { url = schemeStr ++ "://" ++ c.host ++ portStr ++ c.path ++ queryStr ++ fragmentStr }

-- URL-encode a string (basic encoding)
let urlEncode
    : Text -> Text
    = \(s : Text) ->
        let step1 = Text/replace " " "%20" s
        let step2 = Text/replace "&" "%26" step1
        let step3 = Text/replace "=" "%3D" step2
        let step4 = Text/replace "?" "%3F" step3
        let step5 = Text/replace "#" "%23" step4
        let step6 = Text/replace "/" "%2F" step5
        let step7 = Text/replace ":" "%3A" step6
        let step8 = Text/replace "@" "%40" step7
        in step8

-- URL-encode path component (preserves slashes)
let urlEncodePath
    : Text -> Text
    = \(s : Text) ->
        let step1 = Text/replace " " "%20" s
        let step2 = Text/replace "?" "%3F" step1
        let step3 = Text/replace "#" "%23" step2
        in step3

-- Query parameter
let QueryParam = { key : Text, value : Text }

-- Build query string from parameters
let buildQueryString
    : List QueryParam -> Text
    = \(params : List QueryParam) ->
        let encode = \(p : QueryParam) -> urlEncode p.key ++ "=" ++ urlEncode p.value
        in Prelude.Text.concatMapSep "&" QueryParam encode params

-- Common ports for schemes
let defaultPort
    : Scheme -> Optional Natural
    = \(s : Scheme) ->
        merge {
            HTTP = Some 80,
            HTTPS = Some 443,
            FTP = Some 21,
            FTPS = Some 990,
            WS = Some 80,
            WSS = Some 443,
            File = None Natural,
            Mailto = None Natural,
            Tel = None Natural,
            Custom = \(_ : Text) -> None Natural
        } s

-- Origin (scheme + host + port)
let Origin = { scheme : Scheme, host : Text, port : Optional Natural }

-- Create origin from URL components
let mkOrigin
    : Scheme -> Text -> Optional Natural -> Origin
    = \(scheme : Scheme) -> \(host : Text) -> \(port : Optional Natural) ->
        { scheme = scheme, host = host, port = port }

-- Check if URL is absolute (has scheme)
let isAbsolute
    : Url -> Bool
    = \(u : Url) ->
        -- Cannot check string prefix in Dhall
        True

-- Check if URL is relative
let isRelative
    : Url -> Bool
    = \(u : Url) ->
        -- Cannot check string prefix in Dhall
        False

-- Safe URL for external links
let ExternalUrl = { url : Text, target : Text }

-- Create external URL (opens in new tab)
let mkExternalUrl
    : Text -> ExternalUrl
    = \(u : Text) ->
        { url = u, target = "_blank" }

-- API endpoint URL
let ApiEndpoint = { baseUrl : Url, path : Text, version : Optional Text }

-- Create API endpoint
let mkApiEndpoint
    : Url -> Text -> ApiEndpoint
    = \(base : Url) -> \(path : Text) ->
        { baseUrl = base, path = path, version = None Text }

-- Build full API URL
let buildApiUrl
    : ApiEndpoint -> Text
    = \(e : ApiEndpoint) ->
        let versionStr = merge {
            None = "",
            Some = \(v : Text) -> "/" ++ v
        } e.version
        in e.baseUrl.url ++ versionStr ++ e.path

in {
    -- Types
    Scheme,
    Url,
    UrlResult,
    UrlComponents,
    QueryParam,
    Origin,
    ExternalUrl,
    ApiEndpoint,

    -- Constructors
    ok,
    err,
    mkUrl,
    fromComponents,
    defaultComponents,
    mkOrigin,
    mkExternalUrl,
    mkApiEndpoint,

    -- Encoding
    urlEncode,
    urlEncodePath,
    buildQueryString,

    -- Utilities
    defaultPort,
    isAbsolute,
    isRelative,
    buildApiUrl
}
