-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Email Address Validation
|||
||| Provides comprehensive email validation including:
||| - RFC 5321/5322 compliance checking
||| - Domain validation
||| - Common email pattern validation
module Proven.SafeEmail.Validation

import Proven.Core
import Proven.SafeEmail.Parser
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Validation Result Types
--------------------------------------------------------------------------------

||| Validation severity levels
public export
data ValidationSeverity = Error | Warning | Info

public export
Eq ValidationSeverity where
  Error == Error = True
  Warning == Warning = True
  Info == Info = True
  _ == _ = False

public export
Show ValidationSeverity where
  show Error = "ERROR"
  show Warning = "WARNING"
  show Info = "INFO"

||| Validation issue
public export
record ValidationIssue where
  constructor MkValidationIssue
  severity : ValidationSeverity
  code : String
  message : String

public export
Show ValidationIssue where
  show issue = "[" ++ show issue.severity ++ "] " ++ issue.code ++ ": " ++ issue.message

||| Validation result
public export
record ValidationResult where
  constructor MkValidationResult
  isValid : Bool
  issues : List ValidationIssue

||| Create successful validation result
public export
validResult : ValidationResult
validResult = MkValidationResult True []

||| Create failed validation result
public export
invalidResult : ValidationIssue -> ValidationResult
invalidResult issue = MkValidationResult False [issue]

||| Add issue to validation result
public export
addIssue : ValidationIssue -> ValidationResult -> ValidationResult
addIssue issue result =
  let newValid = result.isValid && issue.severity /= Error
  in MkValidationResult newValid (issue :: result.issues)

||| Combine validation results
public export
combineResults : ValidationResult -> ValidationResult -> ValidationResult
combineResults r1 r2 =
  MkValidationResult (r1.isValid && r2.isValid) (r1.issues ++ r2.issues)

--------------------------------------------------------------------------------
-- RFC Compliance Validation
--------------------------------------------------------------------------------

||| Validate local part according to RFC 5321
public export
validateLocalPart : String -> ValidationResult
validateLocalPart "" =
  invalidResult (MkValidationIssue Error "E001" "Local part cannot be empty")
validateLocalPart local =
  let result = validResult
      result' = if length local > 64
                  then addIssue (MkValidationIssue Error "E002" "Local part exceeds 64 characters") result
                  else result
      result'' = if isPrefixOf "." local
                   then addIssue (MkValidationIssue Error "E003" "Local part cannot start with dot") result'
                   else result'
      result''' = if isSuffixOf "." local
                    then addIssue (MkValidationIssue Error "E004" "Local part cannot end with dot") result''
                    else result''
      result'''' = if isInfixOf ".." local
                     then addIssue (MkValidationIssue Error "E005" "Local part cannot contain consecutive dots") result'''
                     else result'''
  in result''''

||| Validate domain according to RFC 5321
public export
validateDomain : String -> ValidationResult
validateDomain "" =
  invalidResult (MkValidationIssue Error "E010" "Domain cannot be empty")
validateDomain domain =
  let result = validResult
      labels = split '.' domain
      result' = if length domain > 253
                  then addIssue (MkValidationIssue Error "E011" "Domain exceeds 253 characters") result
                  else result
      result'' = if any null labels
                   then addIssue (MkValidationIssue Error "E012" "Domain contains empty label") result'
                   else result'
      result''' = if any (\l => length l > 63) labels
                    then addIssue (MkValidationIssue Error "E013" "Domain label exceeds 63 characters") result''
                    else result''
      result'''' = if length labels < 2
                     then addIssue (MkValidationIssue Warning "W001" "Domain has only one label (no TLD)") result'''
                     else result'''
  in result''''

||| Validate entire email address
public export
validateEmailFull : String -> ValidationResult
validateEmailFull s =
  case parseEmail s of
    Nothing => invalidResult (MkValidationIssue Error "E000" "Cannot parse email address")
    Just email =>
      let localResult = validateLocalPart email.localPart
          domainResult = validateDomain email.domain
          lengthResult = if length s > 254
                           then invalidResult (MkValidationIssue Error "E020" "Total length exceeds 254 characters")
                           else validResult
      in combineResults lengthResult (combineResults localResult domainResult)

--------------------------------------------------------------------------------
-- Extended Validation
--------------------------------------------------------------------------------

||| Check for common typos in domain
public export
checkCommonTypos : String -> ValidationResult
checkCommonTypos domain =
  let checks = [ ("gmial.com", "gmail.com")
               , ("gmal.com", "gmail.com")
               , ("gmaill.com", "gmail.com")
               , ("hotmal.com", "hotmail.com")
               , ("hotmai.com", "hotmail.com")
               , ("yaho.com", "yahoo.com")
               , ("yahooo.com", "yahoo.com")
               , ("outloo.com", "outlook.com")
               ]
      match = find (\(typo, _) => toLower domain == typo) checks
  in case match of
       Just (_, suggestion) =>
         addIssue (MkValidationIssue Warning "W010"
                    ("Possible typo - did you mean " ++ suggestion ++ "?"))
                  validResult
       Nothing => validResult

||| Check for role-based addresses
public export
checkRoleAddress : String -> ValidationResult
checkRoleAddress local =
  let roleAddresses = [ "admin", "administrator", "webmaster", "postmaster"
                      , "hostmaster", "info", "support", "sales", "marketing"
                      , "contact", "help", "abuse", "noreply", "no-reply"
                      , "mailer-daemon", "root", "security"
                      ]
  in if toLower local `elem` roleAddresses
       then addIssue (MkValidationIssue Info "I001"
                       "This appears to be a role-based address")
                     validResult
       else validResult

||| Check for plus addressing
public export
checkPlusAddressing : String -> ValidationResult
checkPlusAddressing local =
  if '+' `elem` unpack local
    then addIssue (MkValidationIssue Info "I002"
                    "Email uses plus addressing (e.g., user+tag@domain)")
                  validResult
    else validResult

||| Check for numeric-only local part
public export
checkNumericLocal : String -> ValidationResult
checkNumericLocal local =
  if all isDigit (unpack local)
    then addIssue (MkValidationIssue Info "I003"
                    "Local part contains only digits")
                  validResult
    else validResult

--------------------------------------------------------------------------------
-- Comprehensive Validation
--------------------------------------------------------------------------------

||| Perform all validations
public export
validateComprehensive : String -> ValidationResult
validateComprehensive s =
  case parseEmail s of
    Nothing => invalidResult (MkValidationIssue Error "E000" "Cannot parse email address")
    Just email =>
      let results = [ validateLocalPart email.localPart
                    , validateDomain email.domain
                    , checkCommonTypos email.domain
                    , checkRoleAddress email.localPart
                    , checkPlusAddressing email.localPart
                    , checkNumericLocal email.localPart
                    ]
      in foldl combineResults validResult results

||| Get only errors from validation
public export
getErrors : ValidationResult -> List ValidationIssue
getErrors result = filter (\i => i.severity == Error) result.issues

||| Get only warnings from validation
public export
getWarnings : ValidationResult -> List ValidationIssue
getWarnings result = filter (\i => i.severity == Warning) result.issues

||| Check if validation has errors
public export
hasErrors : ValidationResult -> Bool
hasErrors result = any (\i => i.severity == Error) result.issues

||| Check if validation has warnings
public export
hasWarnings : ValidationResult -> Bool
hasWarnings result = any (\i => i.severity == Warning) result.issues

--------------------------------------------------------------------------------
-- Domain-Specific Validation
--------------------------------------------------------------------------------

||| Common free email providers
public export
freeEmailDomains : List String
freeEmailDomains =
  [ "gmail.com", "yahoo.com", "hotmail.com", "outlook.com"
  , "aol.com", "icloud.com", "mail.com", "protonmail.com"
  , "zoho.com", "yandex.com", "gmx.com", "fastmail.com"
  ]

||| Check if email uses free provider
public export
isFreeEmail : String -> Bool
isFreeEmail domain = toLower domain `elem` freeEmailDomains

||| Validate business email (not from free provider)
public export
validateBusinessEmail : String -> ValidationResult
validateBusinessEmail s =
  case parseEmail s of
    Nothing => invalidResult (MkValidationIssue Error "E000" "Cannot parse email address")
    Just email =>
      if isFreeEmail email.domain
        then addIssue (MkValidationIssue Warning "W020"
                        "Email uses a free provider, not a business domain")
                      (validateEmailFull s)
        else validateEmailFull s

--------------------------------------------------------------------------------
-- Batch Validation
--------------------------------------------------------------------------------

||| Validate list of emails
public export
validateEmailList : List String -> List (String, ValidationResult)
validateEmailList = map (\s => (s, validateComprehensive s))

||| Filter valid emails from list
public export
filterValid : List String -> List String
filterValid = filter (\s => (validateEmailFull s).isValid)

||| Partition emails into valid and invalid
public export
partitionByValidity : List String -> (List String, List String)
partitionByValidity emails = (filterValid emails, filter (not . isValid) emails)
  where
    isValid : String -> Bool
    isValid s = (validateEmailFull s).isValid
