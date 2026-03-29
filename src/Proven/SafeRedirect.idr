-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeRedirect - Open redirect prevention
|||
||| Provides type-safe URL redirect validation to prevent
||| open redirect vulnerabilities (CWE-601, OWASP A1:2021).
||| Prevents: phishing via redirect, credential theft, OAuth token leakage.
module Proven.SafeRedirect

import Data.String
import Data.List
import Data.Nat

%default total

-- ============================================================================
-- REDIRECT POLICY
-- ============================================================================

||| How to validate redirect targets
public export
data RedirectPolicy =
    AllowSameOrigin              -- Only same-origin paths
  | AllowList (List String)      -- Explicit domain allowlist
  | AllowRelativeOnly            -- Only relative paths (no scheme)
  | AllowSameAndList (List String) -- Same-origin + allowlist

public export
Eq RedirectPolicy where
  AllowSameOrigin == AllowSameOrigin = True
  AllowRelativeOnly == AllowRelativeOnly = True
  (AllowList a) == (AllowList b) = a == b
  (AllowSameAndList a) == (AllowSameAndList b) = a == b
  _ == _ = False

-- ============================================================================
-- URL ANALYSIS
-- ============================================================================

||| Check if a URL is relative (no scheme, no protocol-relative)
public export
isRelativeUrl : String -> Bool
isRelativeUrl url =
  not (isInfixOf "://" url) &&
  not (isPrefixOf "//" url) &&
  not (isPrefixOf "javascript:" (toLower url)) &&
  not (isPrefixOf "data:" (toLower url)) &&
  not (isPrefixOf "vbscript:" (toLower url))
  where
    toLower : String -> String
    toLower = pack . map toLower . unpack

||| Check if a URL starts with a safe path prefix
public export
isSafePath : String -> Bool
isSafePath url =
  isPrefixOf "/" url && not (isPrefixOf "//" url) && not (isPrefixOf "/\\" url)

||| Extract the host from an absolute URL (scheme://host/...)
public export
extractHost : String -> Maybe String
extractHost url =
  case break (== '/') (unpack url) of
    (schemePart, '/' :: '/' :: rest) =>
      let hostAndPath = pack rest
      in case break (\c => c == '/' || c == '?' || c == '#') (unpack hostAndPath) of
           (hostPart, _) =>
             let host = pack hostPart
             in if length host > 0 then Just host else Nothing
    _ => Nothing

||| Check if a URL belongs to an allowed domain
public export
isAllowedDomain : List String -> String -> Bool
isAllowedDomain allowedDomains url =
  case extractHost url of
    Nothing   => False
    Just host => any (\d => d == host || isSuffixOf ("." ++ d) host) allowedDomains
  where
    isSuffixOf : String -> String -> Bool
    isSuffixOf suffix str = isPrefixOf (reverse suffix) (reverse str)
      where
        reverse : String -> String
        reverse = pack . reverse . unpack

-- ============================================================================
-- REDIRECT VALIDATION
-- ============================================================================

||| Result of redirect validation
public export
data RedirectResult =
    Safe String       -- URL is safe to redirect to
  | Blocked String    -- Reason the URL was blocked

public export
Show RedirectResult where
  show (Safe url) = "Safe: " ++ url
  show (Blocked reason) = "Blocked: " ++ reason

||| Validate a redirect URL against a policy
public export
validateRedirect : RedirectPolicy -> String -> String -> RedirectResult
validateRedirect policy currentOrigin targetUrl =
  -- Always block dangerous schemes
  if isDangerousScheme targetUrl
    then Blocked "Dangerous scheme (javascript/data/vbscript)"
  -- Check by policy
  else case policy of
    AllowRelativeOnly =>
      if isRelativeUrl targetUrl && isSafePath targetUrl
        then Safe targetUrl
        else Blocked "Not a safe relative path"
    AllowSameOrigin =>
      if isRelativeUrl targetUrl || isPrefixOf currentOrigin targetUrl
        then Safe targetUrl
        else Blocked "Not same-origin"
    AllowList domains =>
      if isRelativeUrl targetUrl then Safe targetUrl
      else if isAllowedDomain domains targetUrl then Safe targetUrl
      else Blocked "Domain not in allowlist"
    AllowSameAndList domains =>
      if isRelativeUrl targetUrl || isPrefixOf currentOrigin targetUrl
        then Safe targetUrl
      else if isAllowedDomain domains targetUrl then Safe targetUrl
      else Blocked "Not same-origin and domain not in allowlist"
  where
    isDangerousScheme : String -> Bool
    isDangerousScheme url =
      let lower = pack (map toLower (unpack url))
      in isPrefixOf "javascript:" lower ||
         isPrefixOf "data:" lower ||
         isPrefixOf "vbscript:" lower

||| Quick check: is the redirect safe?
public export
isSafeRedirect : RedirectPolicy -> String -> String -> Bool
isSafeRedirect policy origin target =
  case validateRedirect policy origin target of
    Safe _ => True
    Blocked _ => False

-- ============================================================================
-- SAFE REDIRECT CONSTRUCTION
-- ============================================================================

||| A validated redirect URL
public export
record SafeRedirectUrl where
  constructor MkSafeRedirectUrl
  url : String

||| Create a safe redirect URL (validates against policy)
public export
mkSafeRedirect : RedirectPolicy -> String -> String -> Maybe SafeRedirectUrl
mkSafeRedirect policy origin target =
  case validateRedirect policy origin target of
    Safe url => Just (MkSafeRedirectUrl url)
    Blocked _ => Nothing

||| Sanitise a redirect URL: if unsafe, return fallback
public export
sanitiseRedirect : RedirectPolicy -> String -> String -> String -> String
sanitiseRedirect policy origin target fallback =
  case validateRedirect policy origin target of
    Safe url => url
    Blocked _ => fallback

-- ============================================================================
-- COMMON PATTERNS
-- ============================================================================

||| Post-login redirect validation (common pattern)
||| Only allows relative paths to prevent redirect-after-login attacks
public export
validateLoginRedirect : String -> Maybe SafeRedirectUrl
validateLoginRedirect target =
  if isRelativeUrl target && isSafePath target
    then Just (MkSafeRedirectUrl target)
    else Nothing

||| OAuth callback redirect validation
public export
validateOAuthRedirect : List String -> String -> Maybe SafeRedirectUrl
validateOAuthRedirect registeredUris callbackUri =
  if any (\uri => isPrefixOf uri callbackUri) registeredUris
    then Just (MkSafeRedirectUrl callbackUri)
    else Nothing
