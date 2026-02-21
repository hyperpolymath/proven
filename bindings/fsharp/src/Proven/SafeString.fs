// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe string operations with XSS and injection prevention.
module SafeString =
    open System
    open System.Text
    open System.Text.RegularExpressions
    open System.Web

    let private htmlEntities =
        Map.ofList [
            ('&', "&amp;")
            ('<', "&lt;")
            ('>', "&gt;")
            ('"', "&quot;")
            ('\'', "&#x27;")
        ]

    /// Escape string for safe HTML output (XSS prevention).
    let escapeHtml (input: string) : string =
        input
        |> Seq.map (fun c -> Map.tryFind c htmlEntities |> Option.defaultValue (string c))
        |> String.concat ""

    /// Escape string for SQL single quotes.
    /// Note: PREFER PARAMETERIZED QUERIES! This is for edge cases only.
    let escapeSql (input: string) : string =
        input.Replace("'", "''")

    /// Escape string for JavaScript string context.
    let escapeJs (input: string) : string =
        let sb = StringBuilder()
        for c in input do
            match c with
            | '\\' -> sb.Append("\\\\") |> ignore
            | '\'' -> sb.Append("\\'") |> ignore
            | '"' -> sb.Append("\\\"") |> ignore
            | '\n' -> sb.Append("\\n") |> ignore
            | '\r' -> sb.Append("\\r") |> ignore
            | '\t' -> sb.Append("\\t") |> ignore
            | '<' -> sb.Append("\\x3C") |> ignore
            | '>' -> sb.Append("\\x3E") |> ignore
            | _ -> sb.Append(c) |> ignore
        sb.ToString()

    /// Escape string for shell command (single-quote wrapping).
    let escapeShell (input: string) : string =
        "'" + input.Replace("'", "'\\''") + "'"

    /// Escape special characters for use in regex patterns.
    let escapeRegex (input: string) : string =
        Regex.Escape(input)

    /// URL-encode a string.
    let urlEncode (input: string) : string =
        Uri.EscapeDataString(input)

    /// URL-decode a string.
    let urlDecode (input: string) : string option =
        try
            Some(Uri.UnescapeDataString(input))
        with
        | _ -> None

    /// Base64-encode a string.
    let base64Encode (input: string) : string =
        Convert.ToBase64String(Encoding.UTF8.GetBytes(input))

    /// Base64-decode a string.
    let base64Decode (input: string) : string option =
        try
            Some(Encoding.UTF8.GetString(Convert.FromBase64String(input)))
        with
        | _ -> None

    /// Safely truncate a string to a maximum length.
    let truncate (maxLength: int) (suffix: string) (input: string) : string =
        if maxLength <= 0 then ""
        elif input.Length <= maxLength then input
        elif maxLength <= suffix.Length then input.Substring(0, maxLength)
        else input.Substring(0, maxLength - suffix.Length) + suffix

    /// Safely truncate with default suffix.
    let truncateDefault (maxLength: int) (input: string) : string =
        truncate maxLength "..." input

    /// Strip HTML tags from string (basic - not a full parser).
    let stripHtml (input: string) : string =
        Regex.Replace(input, "<[^>]*>", "")

    /// Check if string contains only alphanumeric characters.
    let isAlphanumeric (input: string) : bool =
        not (String.IsNullOrEmpty(input)) && Regex.IsMatch(input, "^[a-zA-Z0-9]+$")

    /// Check if string contains only ASCII characters.
    let isAscii (input: string) : bool =
        input |> Seq.forall (fun c -> int c >= 0 && int c <= 127)

    /// Sanitize string to contain only specified allowed characters.
    let sanitize (allowed: string) (input: string) : string =
        let pattern = sprintf "[^%s]" allowed
        Regex.Replace(input, pattern, "")

    /// Sanitize with default allowed characters.
    let sanitizeDefault (input: string) : string =
        sanitize "a-zA-Z0-9_-" input

    /// Remove control characters from string.
    let removeControlChars (input: string) : string =
        Regex.Replace(input, @"[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]", "")

    /// Normalize whitespace (collapse multiple spaces, trim).
    let normalizeWhitespace (input: string) : string =
        Regex.Replace(input, @"\s+", " ").Trim()

    /// Check if string looks like it contains injection attempts.
    let containsSuspiciousPatterns (input: string) : bool =
        let lowerInput = input.ToLowerInvariant()
        lowerInput.Contains("<script") ||
        lowerInput.Contains("javascript:") ||
        Regex.IsMatch(lowerInput, @"on\w+\s*=") ||
        Regex.IsMatch(lowerInput, @"union\s+select") ||
        Regex.IsMatch(lowerInput, @";\s*drop\s+table")

    /// Convert string to slug (URL-friendly format).
    let slugify (input: string) : string =
        input.ToLowerInvariant()
        |> fun s -> Regex.Replace(s, @"[^a-z0-9\s-]", "")
        |> fun s -> Regex.Replace(s, @"\s+", "-")
        |> fun s -> Regex.Replace(s, @"-+", "-")
        |> fun s -> s.Trim('-')
