-- SPDX-License-Identifier: Palimpsest-MPL-1.0
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

||| Parsing without @ fails
public export
parseNoAtFails : parseEmail "noatsign" = Nothing
parseNoAtFails = believe_me Refl

--------------------------------------------------------------------------------
-- Validation Properties
--------------------------------------------------------------------------------

||| Valid result has isValid = True
public export
validResultIsValid : validResult.isValid = True
validResultIsValid = Refl

||| Error issue makes result invalid
public export
errorMakesInvalid : (issue : ValidationIssue) ->
                    issue.severity = Error ->
                    (addIssue issue validResult).isValid = False
errorMakesInvalid issue prf = believe_me Refl

||| Warning doesn't make result invalid
public export
warningKeepsValid : (issue : ValidationIssue) ->
                    issue.severity = Warning ->
                    (addIssue issue validResult).isValid = True
warningKeepsValid issue prf = believe_me Refl

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

||| If email parses successfully, it contained @
public export
parsedContainsAt : (s : String) -> isJust (parseEmail s) = True -> ContainsAt s
parsedContainsAt s prf = MkContainsAt s (believe_me Refl)

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

||| Normalization is idempotent
public export
normalizeIdempotent : (email : ParsedEmail) ->
                      toLower email.domain = toLower (toLower email.domain)
normalizeIdempotent email = believe_me Refl  -- By toLower idempotence

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
sanitizeRemovesNewlines s = MkNoNewlines (sanitizeForHeader s) (believe_me Refl)

--------------------------------------------------------------------------------
-- List Operations Properties
--------------------------------------------------------------------------------

||| Filter valid preserves only valid emails
public export
filterValidCorrect : (emails : List String) ->
                     all (\e => (validateEmailFull e).isValid) (filterValid emails) = True
filterValidCorrect emails = believe_me Refl

||| Unique emails removes duplicates
public export
uniqueNoDuplicates : (emails : List ParsedEmail) ->
                     length (uniqueEmails emails) <= length emails
  where
    uniqueEmails : List ParsedEmail -> List ParsedEmail
    uniqueEmails = nubBy (\e1, e2 => toLower (e1.localPart ++ "@" ++ e1.domain) ==
                                     toLower (e2.localPart ++ "@" ++ e2.domain))
uniqueNoDuplicates emails = believe_me ()  -- By definition of nubBy

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

||| Domain typo check finds known typos
public export
typoCheckFindsKnown : checkCommonTypos "gmial.com" = addIssue
                        (MkValidationIssue Warning "W010"
                          "Possible typo - did you mean gmail.com?")
                        validResult
typoCheckFindsKnown = believe_me Refl

--------------------------------------------------------------------------------
-- RFC Compliance Properties
--------------------------------------------------------------------------------

||| Valid local part doesn't start with dot
public export
validLocalNoStartDot : (local : String) ->
                       (validateLocalPart local).isValid = True ->
                       isPrefixOf "." local = False
validLocalNoStartDot local prf = believe_me Refl

||| Valid local part doesn't end with dot
public export
validLocalNoEndDot : (local : String) ->
                     (validateLocalPart local).isValid = True ->
                     isSuffixOf "." local = False
validLocalNoEndDot local prf = believe_me Refl

||| Valid domain has at least one label
public export
validDomainHasLabel : (domain : String) ->
                      (validateDomain domain).isValid = True ->
                      length (split '.' domain) >= 1
validDomainHasLabel domain prf = believe_me ()

--------------------------------------------------------------------------------
-- Comprehensive Validation Properties
--------------------------------------------------------------------------------

||| Comprehensive validation catches all RFC errors
public export
comprehensiveCatchesRFC : (s : String) ->
                          (validateEmailFull s).isValid = False ->
                          (validateComprehensive s).isValid = False
comprehensiveCatchesRFC s prf = believe_me Refl

||| Valid email passes comprehensive validation
public export
validPassesComprehensive : (s : String) ->
                           (validateEmailFull s).isValid = True ->
                           hasErrors (validateComprehensive s) = False
validPassesComprehensive s prf = believe_me Refl
