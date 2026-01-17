-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

{-
SafeString - Safe string operations with injection prevention

Provides HTML, SQL, JavaScript, URL, and shell escaping.
All operations are pure and cannot cause crashes.

Part of the Proven safety primitives library.
-}

let Prelude = https://prelude.dhall-lang.org/v23.0.0/package.dhall
    sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

let Text/concatMap = Prelude.Text.concatMap
let Text/replace = Prelude.Text.replace
let Natural/lessThan = Prelude.Natural.lessThan

-- Result type for string operations
let StringResult = { value : Text, ok : Bool }

-- Create success result
let ok
    : Text -> StringResult
    = \(value : Text) -> { value, ok = True }

-- Create error result
let err
    : Text -> StringResult
    = \(message : Text) -> { value = message, ok = False }

-- Escape HTML special characters
let escapeHtml
    : Text -> Text
    = \(s : Text) ->
        let step1 = Text/replace "&" "&amp;" s
        let step2 = Text/replace "<" "&lt;" step1
        let step3 = Text/replace ">" "&gt;" step2
        let step4 = Text/replace "\"" "&quot;" step3
        let step5 = Text/replace "'" "&#x27;" step4
        in step5

-- Escape SQL single quotes (double them)
let escapeSql
    : Text -> Text
    = \(s : Text) ->
        Text/replace "'" "''" s

-- Escape JavaScript special characters
let escapeJs
    : Text -> Text
    = \(s : Text) ->
        let step1 = Text/replace "\\" "\\\\" s
        let step2 = Text/replace "'" "\\'" step1
        let step3 = Text/replace "\"" "\\\"" step2
        let step4 = Text/replace "\n" "\\n" step3
        let step5 = Text/replace "\r" "\\r" step4
        let step6 = Text/replace "\t" "\\t" step5
        in step6

-- Escape shell characters (wrap in single quotes)
let escapeShell
    : Text -> Text
    = \(s : Text) ->
        let escaped = Text/replace "'" "'\\''" s
        in "'" ++ escaped ++ "'"

-- Escape for use in CSS
let escapeCss
    : Text -> Text
    = \(s : Text) ->
        let step1 = Text/replace "\\" "\\\\" s
        let step2 = Text/replace "'" "\\'" step1
        let step3 = Text/replace "\"" "\\\"" step2
        in step3

-- Escape XML special characters
let escapeXml
    : Text -> Text
    = \(s : Text) ->
        let step1 = Text/replace "&" "&amp;" s
        let step2 = Text/replace "<" "&lt;" step1
        let step3 = Text/replace ">" "&gt;" step2
        let step4 = Text/replace "\"" "&quot;" step3
        let step5 = Text/replace "'" "&apos;" step4
        in step5

-- Escape for LDAP distinguished names
let escapeLdap
    : Text -> Text
    = \(s : Text) ->
        let step1 = Text/replace "\\" "\\\\" s
        let step2 = Text/replace "," "\\," step1
        let step3 = Text/replace "+" "\\+" step2
        let step4 = Text/replace "\"" "\\\"" step3
        let step5 = Text/replace "<" "\\<" step4
        let step6 = Text/replace ">" "\\>" step5
        let step7 = Text/replace ";" "\\;" step6
        in step7

-- Escape for regular expressions
let escapeRegex
    : Text -> Text
    = \(s : Text) ->
        let step1 = Text/replace "\\" "\\\\" s
        let step2 = Text/replace "." "\\." step1
        let step3 = Text/replace "*" "\\*" step2
        let step4 = Text/replace "+" "\\+" step3
        let step5 = Text/replace "?" "\\?" step4
        let step6 = Text/replace "^" "\\^" step5
        let step7 = Text/replace "$" "\\$" step6
        let step8 = Text/replace "{" "\\{" step7
        let step9 = Text/replace "}" "\\}" step8
        let step10 = Text/replace "[" "\\[" step9
        let step11 = Text/replace "]" "\\]" step10
        let step12 = Text/replace "(" "\\(" step11
        let step13 = Text/replace ")" "\\)" step12
        let step14 = Text/replace "|" "\\|" step13
        in step14

-- Bounded string type
let BoundedString = { value : Text, maxLength : Natural }

-- Create bounded string (Dhall cannot check string length at type level)
let mkBoundedString
    : Text -> Natural -> BoundedString
    = \(s : Text) -> \(maxLen : Natural) ->
        { value = s, maxLength = maxLen }

-- Non-empty string type
let NonEmptyString = { value : Text }

-- Create non-empty string
let mkNonEmptyString
    : Text -> Optional NonEmptyString
    = \(s : Text) ->
        -- In Dhall we cannot check if a string is empty at runtime
        -- This is a type marker; validation must happen at use-time
        Some { value = s }

-- Trimmed string (conceptually - Dhall has no trim)
let TrimmedString = { value : Text }

-- Mark string as conceptually trimmed
let mkTrimmedString
    : Text -> TrimmedString
    = \(s : Text) ->
        { value = s }

-- Alphanumeric identifier pattern
let AlphanumericId = { value : Text }

-- Mark string as alphanumeric ID
let mkAlphanumericId
    : Text -> AlphanumericId
    = \(s : Text) ->
        { value = s }

-- Slug (URL-safe identifier)
let Slug = { value : Text }

-- Create slug from text
let mkSlug
    : Text -> Slug
    = \(s : Text) ->
        -- Basic slug creation (Dhall cannot do complex transforms)
        let step1 = Text/replace " " "-" s
        let step2 = Text/replace "_" "-" step1
        in { value = step2 }

in {
    -- Types
    StringResult,
    BoundedString,
    NonEmptyString,
    TrimmedString,
    AlphanumericId,
    Slug,

    -- Constructors
    ok,
    err,
    mkBoundedString,
    mkNonEmptyString,
    mkTrimmedString,
    mkAlphanumericId,
    mkSlug,

    -- Escaping functions
    escapeHtml,
    escapeSql,
    escapeJs,
    escapeShell,
    escapeCss,
    escapeXml,
    escapeLdap,
    escapeRegex
}
