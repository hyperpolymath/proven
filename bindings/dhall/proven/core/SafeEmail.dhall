-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

{-
SafeEmail - RFC-compliant email validation

Provides email address types and validation markers.
Dhall cannot perform regex validation, so these are type markers
for use with runtime validation.

Part of the Proven safety primitives library.
-}

let Prelude = https://prelude.dhall-lang.org/v23.0.0/package.dhall
    sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

-- Email address type (validated conceptually)
let Email = { address : Text }

-- Email result
let EmailResult = { value : Email, ok : Bool }

-- Create success result
let ok
    : Email -> EmailResult
    = \(e : Email) -> { value = e, ok = True }

-- Create error result
let err
    : EmailResult
    = { value = { address = "" }, ok = False }

-- Create email (marker - runtime validation required)
let mkEmail
    : Text -> Email
    = \(addr : Text) ->
        { address = addr }

-- Email parts (for structured construction)
let EmailParts = {
    localPart : Text,
    domain : Text
}

-- Create email from parts
let fromParts
    : EmailParts -> Email
    = \(parts : EmailParts) ->
        { address = parts.localPart ++ "@" ++ parts.domain }

-- Common email domains (for validation whitelists)
let CommonDomains = {
    gmail = "gmail.com",
    outlook = "outlook.com",
    hotmail = "hotmail.com",
    yahoo = "yahoo.com",
    icloud = "icloud.com",
    protonmail = "protonmail.com",
    proton = "proton.me"
}

-- Email format types
let EmailFormat = < Standard | PlusAddressing | SubAddressing >

-- Email with format specification
let TypedEmail = {
    address : Text,
    format : EmailFormat
}

-- Create standard email
let mkStandardEmail
    : Text -> TypedEmail
    = \(addr : Text) ->
        { address = addr, format = EmailFormat.Standard }

-- Create plus-addressed email (user+tag@domain.com)
let mkPlusEmail
    : Text -> Text -> Text -> TypedEmail
    = \(user : Text) -> \(tag : Text) -> \(domain : Text) ->
        { address = user ++ "+" ++ tag ++ "@" ++ domain
        , format = EmailFormat.PlusAddressing
        }

-- Email configuration for services
let EmailConfig = {
    from : Email,
    replyTo : Optional Email,
    returnPath : Optional Email
}

-- Create email configuration
let mkEmailConfig
    : Email -> EmailConfig
    = \(from : Email) ->
        { from = from
        , replyTo = None Email
        , returnPath = None Email
        }

-- Email with display name
let EmailWithName = {
    name : Text,
    email : Email
}

-- Create email with display name
let mkEmailWithName
    : Text -> Email -> EmailWithName
    = \(name : Text) -> \(email : Email) ->
        { name = name, email = email }

-- Format email with name for headers
let formatWithName
    : EmailWithName -> Text
    = \(e : EmailWithName) ->
        "\"" ++ e.name ++ "\" <" ++ e.email.address ++ ">"

-- Email list type
let EmailList = { emails : List Email }

-- Create email list
let mkEmailList
    : List Email -> EmailList
    = \(emails : List Email) ->
        { emails = emails }

-- Disposable email domains (for blocking)
let DisposableDomains = [
    "tempmail.com",
    "guerrillamail.com",
    "10minutemail.com",
    "mailinator.com",
    "throwaway.email"
]

-- Role-based email prefixes (often blocked for marketing)
let RoleBasedPrefixes = [
    "admin",
    "info",
    "support",
    "sales",
    "contact",
    "webmaster",
    "postmaster",
    "abuse",
    "noreply",
    "no-reply"
]

in {
    -- Types
    Email,
    EmailResult,
    EmailParts,
    EmailFormat,
    TypedEmail,
    EmailConfig,
    EmailWithName,
    EmailList,

    -- Constructors
    ok,
    err,
    mkEmail,
    fromParts,
    mkStandardEmail,
    mkPlusEmail,
    mkEmailConfig,
    mkEmailWithName,
    mkEmailList,

    -- Utilities
    formatWithName,

    -- Constants
    CommonDomains,
    DisposableDomains,
    RoleBasedPrefixes
}
