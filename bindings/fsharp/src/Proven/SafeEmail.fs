// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe email validation and manipulation.
module SafeEmail =
    open System
    open System.Text.RegularExpressions

    /// Email validation result.
    type EmailResult =
        | Ok of localPart: string * domain: string
        | Error of string

    let disposableDomains = set [
        "tempmail.com"; "throwaway.email"; "guerrillamail.com"; "mailinator.com"
        "10minutemail.com"; "temp-mail.org"; "fakeinbox.com"; "trashmail.com"
        "yopmail.com"; "sharklasers.com"; "getairmail.com"; "tempail.com"
        "discard.email"; "maildrop.cc"
    ]

    let private localPartPattern = Regex(@"^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+$")
    let private domainPattern = Regex(@"^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?(\.[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?)*$")

    /// Parse and validate an email address.
    let parse (email: string) : EmailResult =
        if String.IsNullOrWhiteSpace(email) then Error "empty_email"
        elif email.Length > 254 then Error "email_too_long"
        elif not (email.Contains("@")) then Error "missing_at_symbol"
        elif email.Split('@').Length > 2 then Error "multiple_at_symbols"
        else
            let atIndex = email.IndexOf('@')
            let localPart = email.Substring(0, atIndex)
            let domain = email.Substring(atIndex + 1)

            if String.IsNullOrEmpty(localPart) then Error "empty_local_part"
            elif localPart.Length > 64 then Error "local_part_too_long"
            elif localPart.StartsWith(".") || localPart.EndsWith(".") then Error "local_part_dot_position"
            elif localPart.Contains("..") then Error "local_part_consecutive_dots"
            elif not (localPartPattern.IsMatch(localPart)) then Error "invalid_local_part_chars"
            elif String.IsNullOrEmpty(domain) then Error "empty_domain"
            elif domain.Length > 253 then Error "domain_too_long"
            elif not (domain.Contains(".")) && domain.ToLowerInvariant() <> "localhost" then Error "domain_missing_dot"
            elif domain.StartsWith(".") || domain.EndsWith(".") || domain.StartsWith("-") || domain.EndsWith("-") then Error "invalid_domain_format"
            elif not (domainPattern.IsMatch(domain)) then Error "invalid_domain_chars"
            else Ok(localPart, domain)

    /// Check if email is valid.
    let isValid (email: string) : bool =
        match parse email with
        | Ok _ -> true
        | Error _ -> false

    /// Extract domain from email.
    let getDomain (email: string) : string option =
        match parse email with
        | Ok(_, domain) -> Some domain
        | Error _ -> None

    /// Extract local part from email.
    let getLocalPart (email: string) : string option =
        match parse email with
        | Ok(localPart, _) -> Some localPart
        | Error _ -> None

    /// Normalize email (lowercase domain, preserve local part case).
    let normalize (email: string) : string option =
        match parse email with
        | Ok(localPart, domain) -> Some(sprintf "%s@%s" localPart (domain.ToLowerInvariant()))
        | Error _ -> None

    /// Fully normalize email (lowercase everything).
    let normalizeFull (email: string) : string option =
        match parse email with
        | Ok(localPart, domain) -> Some(sprintf "%s@%s" (localPart.ToLowerInvariant()) (domain.ToLowerInvariant()))
        | Error _ -> None

    /// Check if email is from a disposable service.
    let isDisposable (email: string) : bool =
        match getDomain email with
        | Some domain -> Set.contains (domain.ToLowerInvariant()) disposableDomains
        | None -> false

    /// Check if email is from a specific domain.
    let isFromDomain (email: string) (expectedDomain: string) : bool =
        match getDomain email with
        | Some domain -> domain.ToLowerInvariant() = expectedDomain.ToLowerInvariant()
        | None -> false

    /// Check if email is from one of a list of allowed domains.
    let isFromAllowedDomain (email: string) (allowedDomains: string list) : bool =
        match getDomain email with
        | Some domain ->
            let lowerDomain = domain.ToLowerInvariant()
            allowedDomains |> List.exists (fun d -> d.ToLowerInvariant() = lowerDomain)
        | None -> false

    /// Get the TLD (top-level domain) of an email.
    let getTld (email: string) : string option =
        match getDomain email with
        | Some domain ->
            let lastDot = domain.LastIndexOf('.')
            if lastDot >= 0 && lastDot < domain.Length - 1 then
                Some(domain.Substring(lastDot + 1).ToLowerInvariant())
            else None
        | None -> None

    /// Obfuscate email for display.
    let obfuscate (email: string) : string option =
        match parse email with
        | Ok(localPart, domain) ->
            let obfuscatedLocal =
                if localPart.Length <= 2 then "***"
                else sprintf "%c***%c" localPart.[0] localPart.[localPart.Length - 1]
            Some(sprintf "%s@%s" obfuscatedLocal domain)
        | Error _ -> None

    /// Check if two emails are equivalent.
    let areEquivalent (email1: string) (email2: string) : bool =
        match normalizeFull email1, normalizeFull email2 with
        | Some n1, Some n2 -> n1 = n2
        | _ -> false
