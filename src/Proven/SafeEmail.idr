-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeEmail - Email address operations that cannot crash
|||
||| This module provides:
||| - Safe email address parsing and validation (RFC 5321/5322 compliant)
||| - Email address manipulation
||| - Domain validation
||| - Protection against email header injection
module Proven.SafeEmail

import public Proven.Core
import public Proven.SafeEmail.Parser
import public Proven.SafeEmail.Validation

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Email Address Types
--------------------------------------------------------------------------------

||| Parsed email address components
public export
record EmailAddress where
  constructor MkEmailAddress
  localPart : String
  domain : String

||| Display name with email address
public export
record NamedEmail where
  constructor MkNamedEmail
  displayName : Maybe String
  address : EmailAddress

--------------------------------------------------------------------------------
-- Email Construction
--------------------------------------------------------------------------------

||| Create email from parts (validates)
public export
mkEmail : (local : String) -> (domain : String) -> Maybe EmailAddress
mkEmail local domain =
  if isValidLocalPart local && isValidDomain domain
    then Just (MkEmailAddress local domain)
    else Nothing

||| Create email without validation (use with caution)
public export
unsafeEmail : (local : String) -> (domain : String) -> EmailAddress
unsafeEmail = MkEmailAddress

||| Create named email
public export
mkNamedEmail : Maybe String -> EmailAddress -> NamedEmail
mkNamedEmail name addr = MkNamedEmail name addr

--------------------------------------------------------------------------------
-- Email Display
--------------------------------------------------------------------------------

||| Convert email to string format
public export
toString : EmailAddress -> String
toString email = email.localPart ++ "@" ++ email.domain

||| Convert named email to string format
public export
toStringNamed : NamedEmail -> String
toStringNamed named = case named.displayName of
  Nothing => toString named.address
  Just name => "\"" ++ escapeName name ++ "\" <" ++ toString named.address ++ ">"
  where
    escapeName : String -> String
    escapeName s = pack (go (unpack s))
      where
        go : List Char -> List Char
        go [] = []
        go ('"' :: cs) = '\\' :: '"' :: go cs
        go ('\\' :: cs) = '\\' :: '\\' :: go cs
        go (c :: cs) = c :: go cs

public export
Show EmailAddress where
  show = toString

public export
Show NamedEmail where
  show = toStringNamed

public export
Eq EmailAddress where
  e1 == e2 = toLower (toString e1) == toLower (toString e2)

--------------------------------------------------------------------------------
-- Email Accessors
--------------------------------------------------------------------------------

||| Get local part of email
public export
getLocalPart : EmailAddress -> String
getLocalPart = localPart

||| Get domain of email
public export
getDomain : EmailAddress -> String
getDomain = domain

||| Get display name
public export
getDisplayName : NamedEmail -> Maybe String
getDisplayName = displayName

||| Get email address from named email
public export
getAddress : NamedEmail -> EmailAddress
getAddress = address

--------------------------------------------------------------------------------
-- Email Validation
--------------------------------------------------------------------------------

||| Check if local part is valid according to RFC 5321
public export
isValidLocalPart : String -> Bool
isValidLocalPart "" = False
isValidLocalPart s =
  let chars = unpack s
  in length chars <= 64 &&
     not (isPrefixOf "." s) &&
     not (isSuffixOf "." s) &&
     not (isInfixOf ".." s) &&
     all isValidLocalChar chars

||| Characters valid in local part (simplified)
isValidLocalChar : Char -> Bool
isValidLocalChar c =
  isAlphaNum c || c `elem` unpack ".!#$%&'*+/=?^_`{|}~-"

||| Check if domain is valid
public export
isValidDomain : String -> Bool
isValidDomain "" = False
isValidDomain s =
  let labels = split '.' s
  in not (null labels) &&
     all isValidLabel labels &&
     length s <= 253
  where
    isValidLabel : String -> Bool
    isValidLabel "" = False
    isValidLabel label =
      length label <= 63 &&
      not (isPrefixOf "-" label) &&
      not (isSuffixOf "-" label) &&
      all isValidLabelChar (unpack label)

    isValidLabelChar : Char -> Bool
    isValidLabelChar c = isAlphaNum c || c == '-'

||| Full email validation
public export
isValidEmail : String -> Bool
isValidEmail s = case parseEmail s of
  Just email => isValidLocalPart email.localPart && isValidDomain email.domain
  Nothing => False

||| Validate email returning the parsed address
public export
validateEmail : String -> Maybe EmailAddress
validateEmail s = do
  email <- parseEmail s
  if isValidLocalPart email.localPart && isValidDomain email.domain
    then Just email
    else Nothing

--------------------------------------------------------------------------------
-- Email Normalization
--------------------------------------------------------------------------------

||| Normalize email address (lowercase domain)
public export
normalize : EmailAddress -> EmailAddress
normalize email = MkEmailAddress email.localPart (toLower email.domain)

||| Normalize local part (lowercase for comparison)
||| Note: Some servers are case-sensitive in local part
public export
normalizeFull : EmailAddress -> EmailAddress
normalizeFull email = MkEmailAddress (toLower email.localPart) (toLower email.domain)

||| Extract base email (remove +suffix)
public export
getBaseEmail : EmailAddress -> EmailAddress
getBaseEmail email =
  let local = email.localPart
      base = case break (== '+') (unpack local) of
               (before, _) => pack before
  in MkEmailAddress base email.domain

--------------------------------------------------------------------------------
-- Email Security
--------------------------------------------------------------------------------

||| Sanitize email for header (prevent injection)
public export
sanitizeForHeader : String -> String
sanitizeForHeader s = pack (filter isHeaderSafe (unpack s))
  where
    isHeaderSafe : Char -> Bool
    isHeaderSafe c = c /= '\n' && c /= '\r' && c /= '\0'

||| Escape email for HTML display
public export
escapeForHTML : EmailAddress -> String
escapeForHTML email = escapeHTML (toString email)
  where
    escapeHTML : String -> String
    escapeHTML s = pack (go (unpack s))
      where
        go : List Char -> List Char
        go [] = []
        go ('&' :: cs) = unpack "&amp;" ++ go cs
        go ('<' :: cs) = unpack "&lt;" ++ go cs
        go ('>' :: cs) = unpack "&gt;" ++ go cs
        go ('"' :: cs) = unpack "&quot;" ++ go cs
        go (c :: cs) = c :: go cs

||| Check for common disposable email domains
public export
isDisposableDomain : String -> Bool
isDisposableDomain domain =
  toLower domain `elem` disposableDomains
  where
    disposableDomains : List String
    disposableDomains =
      [ "mailinator.com", "guerrillamail.com", "tempmail.com"
      , "throwaway.email", "10minutemail.com", "trashmail.com"
      , "fakeinbox.com", "getnada.com", "yopmail.com"
      ]

||| Check if email uses disposable domain
public export
isDisposableEmail : EmailAddress -> Bool
isDisposableEmail email = isDisposableDomain email.domain

--------------------------------------------------------------------------------
-- Email Domain Operations
--------------------------------------------------------------------------------

||| Get top-level domain
public export
getTLD : EmailAddress -> Maybe String
getTLD email =
  let parts = split '.' email.domain
  in case parts of
       [] => Nothing
       _ => Just (last parts)

||| Get second-level domain (e.g., "example" from "mail.example.com")
public export
getSecondLevelDomain : EmailAddress -> Maybe String
getSecondLevelDomain email =
  let parts = split '.' email.domain
  in case parts of
       [] => Nothing
       [x] => Just x
       _ => Just (last (init parts))

||| Check if email domain matches a pattern
public export
domainMatches : String -> EmailAddress -> Bool
domainMatches pattern email =
  toLower email.domain == toLower pattern ||
  isSuffixOf ("." ++ toLower pattern) (toLower email.domain)

--------------------------------------------------------------------------------
-- Email List Operations
--------------------------------------------------------------------------------

||| Parse multiple emails separated by commas or semicolons
public export
parseEmailList : String -> List EmailAddress
parseEmailList s =
  let parts = concatMap (split ';') (split ',' s)
      trimmed = map trim parts
  in mapMaybe validateEmail trimmed
  where
    trim : String -> String
    trim = pack . dropWhile isSpace . reverse . dropWhile isSpace . reverse . unpack

||| Format email list for header
public export
formatEmailList : List EmailAddress -> String
formatEmailList [] = ""
formatEmailList emails = join ", " (map toString emails)

||| Remove duplicates from email list (case-insensitive)
public export
uniqueEmails : List EmailAddress -> List EmailAddress
uniqueEmails = nubBy (\e1, e2 => normalizeFull e1 == normalizeFull e2)
