// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe URL parsing, validation, and manipulation with SSRF protection.
module SafeUrl =
    open System
    open System.Text
    open System.Text.RegularExpressions

    /// URL parsing errors.
    type UrlError =
        | InvalidScheme of string
        | InvalidHost of string
        | InvalidPort of int
        | InvalidPath of string
        | InvalidQuery of string
        | MalformedUrl of string
        | EmptyInput
        | SsrfDetected of string

    /// Allowed URL schemes.
    type UrlScheme =
        | Http
        | Https
        | Ftp
        | Ftps
        | Mailto
        | Tel
        | File
        | Data
        | Ws
        | Wss
        | Custom of string

    /// Parsed URL components.
    type ParsedUrl = {
        Scheme: UrlScheme
        Username: string option
        Password: string option
        Host: string
        Port: int option
        Path: string
        Query: Map<string, string list>
        Fragment: string option
    }

    /// Get scheme as string.
    let schemeToString (scheme: UrlScheme) : string =
        match scheme with
        | Http -> "http"
        | Https -> "https"
        | Ftp -> "ftp"
        | Ftps -> "ftps"
        | Mailto -> "mailto"
        | Tel -> "tel"
        | File -> "file"
        | Data -> "data"
        | Ws -> "ws"
        | Wss -> "wss"
        | Custom s -> s

    /// Parse scheme from string.
    let parseScheme (input: string) : UrlScheme option =
        match input.ToLowerInvariant() with
        | "http" -> Some Http
        | "https" -> Some Https
        | "ftp" -> Some Ftp
        | "ftps" -> Some Ftps
        | "mailto" -> Some Mailto
        | "tel" -> Some Tel
        | "file" -> Some File
        | "data" -> Some Data
        | "ws" -> Some Ws
        | "wss" -> Some Wss
        | s when Regex.IsMatch(s, @"^[a-z][a-z0-9+.-]*$") -> Some(Custom s)
        | _ -> None

    /// Check if scheme is secure (HTTPS, WSS, FTPS).
    let isSecureScheme (scheme: UrlScheme) : bool =
        match scheme with
        | Https | Wss | Ftps -> true
        | _ -> false

    /// Get default port for scheme.
    let getDefaultPort (scheme: UrlScheme) : int option =
        match scheme with
        | Http -> Some 80
        | Https -> Some 443
        | Ftp -> Some 21
        | Ftps -> Some 990
        | Ws -> Some 80
        | Wss -> Some 443
        | _ -> None

    /// URL-encode a string.
    let encode (input: string) : string =
        Uri.EscapeDataString(input)

    /// URL-decode a string.
    let decode (input: string) : Result<string, UrlError> =
        try
            Ok(Uri.UnescapeDataString(input))
        with
        | _ -> Error(MalformedUrl "failed to decode URL")

    /// URL-decode a string, returning Option.
    let tryDecode (input: string) : string option =
        match decode input with
        | Ok s -> Some s
        | Error _ -> None

    /// Encode path component.
    let encodePath (path: string) : string =
        path.Split('/')
        |> Array.map encode
        |> String.concat "/"

    /// Parse query string into map.
    let parseQueryString (query: string) : Map<string, string list> =
        if String.IsNullOrEmpty(query) then Map.empty
        else
            let trimmedQuery = if query.StartsWith("?") then query.Substring(1) else query
            trimmedQuery.Split('&')
            |> Array.filter (fun s -> not (String.IsNullOrEmpty(s)))
            |> Array.fold (fun acc pair ->
                let parts = pair.Split([| '=' |], 2)
                let key = tryDecode parts.[0] |> Option.defaultValue parts.[0]
                let value =
                    if parts.Length > 1 then
                        tryDecode parts.[1] |> Option.defaultValue parts.[1]
                    else ""
                let existingValues = Map.tryFind key acc |> Option.defaultValue []
                Map.add key (existingValues @ [value]) acc
            ) Map.empty

    /// Build query string from map.
    let buildQueryString (query: Map<string, string list>) : string =
        if Map.isEmpty query then ""
        else
            query
            |> Map.toSeq
            |> Seq.collect (fun (key, values) ->
                values |> Seq.map (fun value ->
                    sprintf "%s=%s" (encode key) (encode value)))
            |> String.concat "&"

    /// Parse URL string.
    let parse (input: string) : Result<ParsedUrl, UrlError> =
        if String.IsNullOrWhiteSpace(input) then
            Error EmptyInput
        else
            try
                let uri = Uri(input, UriKind.Absolute)
                let scheme =
                    match parseScheme uri.Scheme with
                    | Some s -> s
                    | None -> Custom uri.Scheme

                let userInfo =
                    if String.IsNullOrEmpty(uri.UserInfo) then None, None
                    else
                        let parts = uri.UserInfo.Split([| ':' |], 2)
                        let username = Some parts.[0]
                        let password = if parts.Length > 1 then Some parts.[1] else None
                        username, password

                let port =
                    if uri.Port = -1 then None
                    elif Some uri.Port = getDefaultPort scheme then None
                    else Some uri.Port

                let query = parseQueryString uri.Query

                let fragment =
                    if String.IsNullOrEmpty(uri.Fragment) then None
                    elif uri.Fragment.StartsWith("#") then Some(uri.Fragment.Substring(1))
                    else Some uri.Fragment

                Ok {
                    Scheme = scheme
                    Username = fst userInfo
                    Password = snd userInfo
                    Host = uri.Host
                    Port = port
                    Path = uri.AbsolutePath
                    Query = query
                    Fragment = fragment
                }
            with
            | :? UriFormatException as ex -> Error(MalformedUrl ex.Message)
            | _ -> Error(MalformedUrl "unknown error parsing URL")

    /// Parse URL, returning Option.
    let tryParse (input: string) : ParsedUrl option =
        match parse input with
        | Ok url -> Some url
        | Error _ -> None

    /// Check if string is a valid URL.
    let isValid (input: string) : bool =
        (tryParse input).IsSome

    /// Check if URL is secure (uses HTTPS/WSS/FTPS).
    let isSecure (url: ParsedUrl) : bool =
        isSecureScheme url.Scheme

    /// Check if URL host is a private/internal IP (SSRF protection).
    let isPrivateHost (host: string) : bool =
        let lowerHost = host.ToLowerInvariant()
        lowerHost = "localhost" ||
        lowerHost = "127.0.0.1" ||
        lowerHost = "::1" ||
        lowerHost.StartsWith("192.168.") ||
        lowerHost.StartsWith("10.") ||
        lowerHost.StartsWith("172.16.") ||
        lowerHost.StartsWith("172.17.") ||
        lowerHost.StartsWith("172.18.") ||
        lowerHost.StartsWith("172.19.") ||
        lowerHost.StartsWith("172.20.") ||
        lowerHost.StartsWith("172.21.") ||
        lowerHost.StartsWith("172.22.") ||
        lowerHost.StartsWith("172.23.") ||
        lowerHost.StartsWith("172.24.") ||
        lowerHost.StartsWith("172.25.") ||
        lowerHost.StartsWith("172.26.") ||
        lowerHost.StartsWith("172.27.") ||
        lowerHost.StartsWith("172.28.") ||
        lowerHost.StartsWith("172.29.") ||
        lowerHost.StartsWith("172.30.") ||
        lowerHost.StartsWith("172.31.") ||
        lowerHost.EndsWith(".local") ||
        lowerHost.EndsWith(".internal") ||
        lowerHost = "0.0.0.0" ||
        lowerHost.StartsWith("169.254.")  // Link-local

    /// Validate URL is safe for external requests (SSRF protection).
    let validateForSsrf (url: ParsedUrl) : Result<ParsedUrl, UrlError> =
        if isPrivateHost url.Host then
            Error(SsrfDetected url.Host)
        else
            Ok url

    /// Format URL back to string.
    let format (url: ParsedUrl) : string =
        let sb = StringBuilder()
        sb.Append(schemeToString url.Scheme).Append("://") |> ignore

        // User info
        match url.Username with
        | Some user ->
            sb.Append(encode user) |> ignore
            match url.Password with
            | Some pass -> sb.Append(":").Append(encode pass) |> ignore
            | None -> ()
            sb.Append("@") |> ignore
        | None -> ()

        // Host and port
        sb.Append(url.Host) |> ignore
        match url.Port with
        | Some port -> sb.Append(":").Append(port) |> ignore
        | None -> ()

        // Path
        sb.Append(url.Path) |> ignore

        // Query
        let queryStr = buildQueryString url.Query
        if not (String.IsNullOrEmpty(queryStr)) then
            sb.Append("?").Append(queryStr) |> ignore

        // Fragment
        match url.Fragment with
        | Some frag -> sb.Append("#").Append(encode frag) |> ignore
        | None -> ()

        sb.ToString()

    /// Get the origin (scheme + host + port).
    let getOrigin (url: ParsedUrl) : string =
        let sb = StringBuilder()
        sb.Append(schemeToString url.Scheme).Append("://").Append(url.Host) |> ignore
        match url.Port with
        | Some port -> sb.Append(":").Append(port) |> ignore
        | None -> ()
        sb.ToString()

    /// Get the host with port (if non-default).
    let getHostWithPort (url: ParsedUrl) : string =
        match url.Port with
        | Some port -> sprintf "%s:%d" url.Host port
        | None -> url.Host

    /// Join base URL with relative path.
    let join (baseUrl: ParsedUrl) (relativePath: string) : ParsedUrl =
        let cleanPath =
            if relativePath.StartsWith("/") then relativePath
            else
                let basePath =
                    let lastSlash = baseUrl.Path.LastIndexOf('/')
                    if lastSlash >= 0 then baseUrl.Path.Substring(0, lastSlash + 1)
                    else "/"
                basePath + relativePath
        { baseUrl with Path = cleanPath; Query = Map.empty; Fragment = None }

    /// Normalize URL (lowercase scheme/host, remove default port, normalize path).
    let normalize (url: ParsedUrl) : ParsedUrl =
        { url with
            Host = url.Host.ToLowerInvariant()
            Port = url.Port |> Option.filter (fun p -> Some p <> getDefaultPort url.Scheme)
            Path = if String.IsNullOrEmpty(url.Path) then "/" else url.Path
        }

    /// Check if two URLs are equivalent (after normalization).
    let areEquivalent (a: ParsedUrl) (b: ParsedUrl) : bool =
        let na = normalize a
        let nb = normalize b
        na.Scheme = nb.Scheme &&
        na.Host = nb.Host &&
        na.Port = nb.Port &&
        na.Path = nb.Path &&
        na.Query = nb.Query

    /// Extract domain from host (removes subdomains, keeps TLD).
    let extractDomain (host: string) : string =
        let parts = host.Split('.')
        if parts.Length <= 2 then host
        else
            // Simple heuristic: keep last 2 parts
            sprintf "%s.%s" parts.[parts.Length - 2] parts.[parts.Length - 1]

    /// Get query parameter value.
    let getQueryParam (key: string) (url: ParsedUrl) : string option =
        Map.tryFind key url.Query
        |> Option.bind (fun values ->
            if List.isEmpty values then None else Some (List.head values))

    /// Get all query parameter values.
    let getQueryParamAll (key: string) (url: ParsedUrl) : string list =
        Map.tryFind key url.Query |> Option.defaultValue []

    /// Set query parameter (replaces existing).
    let setQueryParam (key: string) (value: string) (url: ParsedUrl) : ParsedUrl =
        { url with Query = Map.add key [value] url.Query }

    /// Add query parameter (appends to existing).
    let addQueryParam (key: string) (value: string) (url: ParsedUrl) : ParsedUrl =
        let existingValues = Map.tryFind key url.Query |> Option.defaultValue []
        { url with Query = Map.add key (existingValues @ [value]) url.Query }

    /// Remove query parameter.
    let removeQueryParam (key: string) (url: ParsedUrl) : ParsedUrl =
        { url with Query = Map.remove key url.Query }

    /// Check if URL has query parameter.
    let hasQueryParam (key: string) (url: ParsedUrl) : bool =
        Map.containsKey key url.Query

    /// Set fragment.
    let setFragment (fragment: string) (url: ParsedUrl) : ParsedUrl =
        { url with Fragment = Some fragment }

    /// Remove fragment.
    let removeFragment (url: ParsedUrl) : ParsedUrl =
        { url with Fragment = None }

    /// Check if URL matches allowed domains.
    let isFromAllowedDomain (allowedDomains: string list) (url: ParsedUrl) : bool =
        let host = url.Host.ToLowerInvariant()
        allowedDomains |> List.exists (fun domain ->
            let d = domain.ToLowerInvariant()
            host = d || host.EndsWith("." + d))
