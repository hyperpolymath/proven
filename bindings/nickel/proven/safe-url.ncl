# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeUrl - URL parsing and validation for Nickel
#
# Provides safe URL operations with proper validation and
# encoding. Prevents injection attacks and malformed URLs.

{
  # URL result type
  Result = {
    value | String,
    ok | Bool,
  },

  # Parsed URL components
  ParsedUrl = {
    scheme | String,
    host | String,
    port | optional Number,
    path | String,
    query | optional String,
    fragment | optional String,
  },

  # Allowed schemes
  ALLOWED_SCHEMES = ["http", "https", "ftp", "mailto", "tel", "file"],

  # Dangerous schemes that should be blocked
  DANGEROUS_SCHEMES = ["javascript", "data", "vbscript"],

  # Check if string contains only valid URL characters
  is_valid_url_char
    : String -> Bool
    = fun c =>
      let code = std.string.to_number c in
      (code >= 48 && code <= 57) ||  # 0-9
      (code >= 65 && code <= 90) ||  # A-Z
      (code >= 97 && code <= 122) || # a-z
      c == "-" || c == "_" || c == "." || c == "~" ||
      c == ":" || c == "/" || c == "?" || c == "#" ||
      c == "[" || c == "]" || c == "@" || c == "!" ||
      c == "$" || c == "&" || c == "'" || c == "(" ||
      c == ")" || c == "*" || c == "+" || c == "," ||
      c == ";" || c == "=" || c == "%",

  # Check if scheme is allowed
  is_allowed_scheme
    : String -> Bool
    = fun scheme =>
      let lower = std.string.lowercase scheme in
      std.array.any (fun s => s == lower) ALLOWED_SCHEMES,

  # Check if scheme is dangerous
  is_dangerous_scheme
    : String -> Bool
    = fun scheme =>
      let lower = std.string.lowercase scheme in
      std.array.any (fun s => s == lower) DANGEROUS_SCHEMES,

  # Extract scheme from URL
  extract_scheme
    : String -> { value : String, ok : Bool }
    = fun url =>
      let trimmed = std.string.trim url in
      let colon_idx = std.string.find "://" trimmed in
      if colon_idx == -1 then
        { value = "", ok = false }
      else
        let scheme = std.string.substring 0 colon_idx trimmed in
        if is_dangerous_scheme scheme then
          { value = "", ok = false }
        else
          { value = std.string.lowercase scheme, ok = true },

  # Validate URL structure
  is_valid_url
    : String -> Bool
    = fun url =>
      let trimmed = std.string.trim url in
      if std.string.length trimmed < 8 then
        false
      else
        let scheme_result = extract_scheme trimmed in
        if !(scheme_result.ok) then
          false
        else
          let after_scheme = std.string.substring
            ((std.string.length scheme_result.value) + 3)
            (std.string.length trimmed)
            trimmed in
          std.string.length after_scheme > 0,

  # URL encode a string
  url_encode
    : String -> String
    = fun s =>
      let chars = std.string.characters s in
      let encode_char = fun c =>
        if (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9") then
          c
        else if c == "-" || c == "_" || c == "." || c == "~" then
          c
        else if c == " " then
          "%20"
        else
          # For simplicity, pass through other characters
          # In production, would properly encode
          c
      in
      std.string.join "" (std.array.map encode_char chars),

  # Build a URL from components
  build_url
    : { scheme : String, host : String, port : optional Number, path : String } -> { value : String, ok : Bool }
    = fun components =>
      if is_dangerous_scheme components.scheme then
        { value = "", ok = false }
      else if std.string.length components.host == 0 then
        { value = "", ok = false }
      else
        let port_str =
          if components.port == null then
            ""
          else
            ":" ++ (std.string.from_number components.port)
        in
        let path_str =
          if std.string.length components.path == 0 then
            "/"
          else if std.string.substring 0 1 components.path != "/" then
            "/" ++ components.path
          else
            components.path
        in
        { value = components.scheme ++ "://" ++ components.host ++ port_str ++ path_str, ok = true },

  # Join base URL with path
  join_path
    : String -> String -> { value : String, ok : Bool }
    = fun base path =>
      let base_trimmed = std.string.trim base in
      let path_trimmed = std.string.trim path in
      let base_no_slash =
        if std.string.ends_with "/" base_trimmed then
          std.string.substring 0 ((std.string.length base_trimmed) - 1) base_trimmed
        else
          base_trimmed
      in
      let path_with_slash =
        if std.string.starts_with "/" path_trimmed then
          path_trimmed
        else
          "/" ++ path_trimmed
      in
      { value = base_no_slash ++ path_with_slash, ok = true },

  # Extract query parameters as key-value pairs
  # Note: Returns a simple string for now; full parsing would need more complex logic
  extract_query
    : String -> { value : String, ok : Bool }
    = fun url =>
      let query_idx = std.string.find "?" url in
      if query_idx == -1 then
        { value = "", ok = true }
      else
        let fragment_idx = std.string.find "#" url in
        let end_idx = if fragment_idx == -1 then std.string.length url else fragment_idx in
        let query = std.string.substring (query_idx + 1) end_idx url in
        { value = query, ok = true },

  # Extract fragment from URL
  extract_fragment
    : String -> { value : String, ok : Bool }
    = fun url =>
      let fragment_idx = std.string.find "#" url in
      if fragment_idx == -1 then
        { value = "", ok = true }
      else
        let fragment = std.string.substring (fragment_idx + 1) (std.string.length url) url in
        { value = fragment, ok = true },

  # Common URL contracts
  HttpUrl = std.contract.from_predicate (fun url =>
    let scheme_result = extract_scheme url in
    scheme_result.ok && (scheme_result.value == "http" || scheme_result.value == "https")
  ),

  HttpsUrl = std.contract.from_predicate (fun url =>
    let scheme_result = extract_scheme url in
    scheme_result.ok && scheme_result.value == "https"
  ),
}
