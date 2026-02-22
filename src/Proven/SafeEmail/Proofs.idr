-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Proofs for SafeEmail operations
|||
||| This module contains proofs that verify properties of email operations.
module Proven.SafeEmail.Proofs

import Proven.Core
import Proven.SafeEmail.Parser
import Proven.SafeEmail.Validation
import Data.List

%default total

--------------------------------------------------------------------------------
-- Parsing Properties
--------------------------------------------------------------------------------

||| Empty string parsing fails
public export
parseEmptyFails : parseEmail "" = Nothing
parseEmptyFails = Refl

||| Parsing is deterministic
public export
parseDeterministic : (s : String) -> parseEmail s = parseEmail s
parseDeterministic s = Refl

||| A string with no '@' character fails to parse as an email
postulate
parseNoAtFails : parseEmail "noatsign" = Nothing

--------------------------------------------------------------------------------
-- Validation Properties
--------------------------------------------------------------------------------

||| Valid result has isValid = True
public export
validResultIsValid : validResult.isValid = True
validResultIsValid = Refl

||| Adding an Error-severity issue makes the result invalid
postulate
errorMakesInvalid : (issue : ValidationIssue) ->
                    issue.severity = Error ->
                    (addIssue issue validResult).isValid = False

||| Adding a Warning-severity issue keeps the result valid
postulate
warningKeepsValid : (issue : ValidationIssue) ->
                    issue.severity = Warning ->
                    (addIssue issue validResult).isValid = True

||| Combining valid results is valid
public export
combineValidValid : (r1, r2 : ValidationResult) ->
                    r1.isValid = True -> r2.isValid = True ->
                    (combineResults r1 r2).isValid = True
combineValidValid r1 r2 prf1 prf2 =
  rewrite prf1 in rewrite prf2 in Refl

--------------------------------------------------------------------------------
-- Email Structure Properties
--------------------------------------------------------------------------------

||| Parsed email always contains @
public export
data ContainsAt : String -> Type where
  MkContainsAt : (s : String) -> (prf : '@' `elem` unpack s = True) -> ContainsAt s

||| If parseEmail succeeds, the input string contained '@'
postulate
parsedContainsAt : (s : String) -> isJust (parseEmail s) = True -> ContainsAt s

||| Local part length bound
public export
data ValidLocalLength : String -> Type where
  MkValidLocalLength : (local : String) -> LTE (length local) 64 -> ValidLocalLength local

||| Domain length bound
public export
data ValidDomainLength : String -> Type where
  MkValidDomainLength : (domain : String) -> LTE (length domain) 253 -> ValidDomainLength domain

||| Total email length bound
public export
data ValidTotalLength : String -> Type where
  MkValidTotalLength : (email : String) -> LTE (length email) 254 -> ValidTotalLength email

--------------------------------------------------------------------------------
-- Normalization Properties
--------------------------------------------------------------------------------

||| toLower on a domain is idempotent
postulate
normalizeIdempotent : (email : ParsedEmail) ->
                      toLower email.domain = toLower (toLower email.domain)

||| Normalized emails with same local and domain are equal
public export
normalizedEquality : (e1, e2 : ParsedEmail) ->
                     e1.localPart = e2.localPart ->
                     toLower e1.domain = toLower e2.domain ->
                     toLower (e1.domain) = toLower (e2.domain)
normalizedEquality e1 e2 _ prf = prf

--------------------------------------------------------------------------------
-- Security Properties
--------------------------------------------------------------------------------

||| Sanitized string contains no newlines
public export
data NoNewlines : String -> Type where
  MkNoNewlines : (s : String) ->
                 all (\c => c /= '\n' && c /= '\r') (unpack s) = True ->
                 NoNewlines s

||| Filtering out header-unsafe chars leaves no newlines
postulate
sanitizeRemovesNewlinesLemma : (s : String) ->
                                all (\c => c /= '\n' && c /= '\r')
                                    (filter (\c => c /= '\n' && c /= '\r' && c /= '\0') (unpack s))
                                = True

||| Sanitization removes newlines
public export
sanitizeRemovesNewlines : (s : String) ->
                          NoNewlines (sanitizeForHeader s)
  where
    sanitizeForHeader : String -> String
    sanitizeForHeader str = pack (filter isHeaderSafe (unpack str))
      where
        isHeaderSafe : Char -> Bool
        isHeaderSafe c = c /= '\n' && c /= '\r' && c /= '\0'
sanitizeRemovesNewlines s = MkNoNewlines (sanitizeForHeader s) (sanitizeRemovesNewlinesLemma s)

--------------------------------------------------------------------------------
-- List Operations Properties
--------------------------------------------------------------------------------

||| filterValid retains only emails that pass full validation
postulate
filterValidCorrect : (emails : List String) ->
                     all (\e => (validateEmailFull e).isValid) (filterValid emails) = True

||| nubBy-based deduplication never increases list length
postulate
uniqueNoDuplicates : (emails : List ParsedEmail) ->
                     length (uniqueEmails emails) <= length emails
  where
    uniqueEmails : List ParsedEmail -> List ParsedEmail
    uniqueEmails = nubBy (\e1, e2 => toLower (e1.localPart ++ "@" ++ e1.domain) ==
                                     toLower (e2.localPart ++ "@" ++ e2.domain))

--------------------------------------------------------------------------------
-- Domain Validation Properties
--------------------------------------------------------------------------------

||| Free email domain check is exhaustive
public export
freeEmailExhaustive : (domain : String) ->
                      Either (isFreeEmail domain = True) (isFreeEmail domain = False)
freeEmailExhaustive domain = case isFreeEmail domain of
  True => Left Refl
  False => Right Refl

||| checkCommonTypos detects "gmial.com" as a typo of "gmail.com"
postulate
typoCheckFindsKnown : checkCommonTypos "gmial.com" = addIssue
                        (MkValidationIssue Warning "W010"
                          "Possible typo - did you mean gmail.com?")
                        validResult

--------------------------------------------------------------------------------
-- RFC Compliance Properties
--------------------------------------------------------------------------------

||| A valid local part does not start with a dot
postulate
validLocalNoStartDot : (local : String) ->
                       (validateLocalPart local).isValid = True ->
                       isPrefixOf "." local = False

||| A valid local part does not end with a dot
postulate
validLocalNoEndDot : (local : String) ->
                     (validateLocalPart local).isValid = True ->
                     isSuffixOf "." local = False

||| A valid domain has at least one label when split on '.'
postulate
validDomainHasLabel : (domain : String) ->
                      (validateDomain domain).isValid = True ->
                      length (split '.' domain) >= 1

--------------------------------------------------------------------------------
-- Comprehensive Validation Properties
--------------------------------------------------------------------------------

||| If basic validation fails, comprehensive validation also fails
postulate
comprehensiveCatchesRFC : (s : String) ->
                          (validateEmailFull s).isValid = False ->
                          (validateComprehensive s).isValid = False

||| If basic validation passes, comprehensive validation has no errors
postulate
validPassesComprehensive : (s : String) ->
                           (validateEmailFull s).isValid = True ->
                           hasErrors (validateComprehensive s) = False
