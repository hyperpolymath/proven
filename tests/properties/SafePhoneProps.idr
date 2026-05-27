-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafePhoneProps

import Proven.Core
import Proven.SafePhone

%default total

||| Property: E.164 format parses
prop_e164Parses : isOk (parsePhone "+14155551234") = True
prop_e164Parses = Refl

||| Property: US format parses
prop_usFormatParses : isOk (parsePhone "(415) 555-1234") = True
prop_usFormatParses = Refl

||| Property: International format parses
prop_internationalParses : isOk (parsePhone "+44 20 7946 0958") = True
prop_internationalParses = Refl

||| Property: Too short fails
prop_tooShortFails : isErr (parsePhone "123") = True
prop_tooShortFails = Refl

||| Property: Too long fails
prop_tooLongFails : isErr (parsePhone "+12345678901234567890") = True
prop_tooLongFails = Refl

||| Property: Letters fail
prop_lettersFail : isErr (parsePhone "+1ABCDEFGHIJ") = True
prop_lettersFail = Refl

||| OWED: Formatting to E.164 is consistent
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_formatE164Consistent_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_formatE164Consistent : (p : ValidPhone) ->
                              parsePhone (formatE164 p) = Ok p

||| OWED: Country code extractable
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_countryCodeExtractable_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_countryCodeExtractable : (p : ValidPhone) ->
                                length (getCountryCode p) >= 1 && length (getCountryCode p) <= 3

||| OWED: National number extractable
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_nationalNumberExtractable_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_nationalNumberExtractable : (p : ValidPhone) ->
                                   length (getNationalNumber p) >= 4

||| Property: Phone comparison is reflexive
prop_phoneEqReflexive : (p : ValidPhone) -> p == p = True
prop_phoneEqReflexive p = Refl

||| OWED: Different formats normalize to same
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_normalizationConsistent_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_normalizationConsistent : formatE164 (parsePhone "(415) 555-1234") = formatE164 (parsePhone "4155551234")

||| Test runner for phone properties
export
runPhoneProperties : IO ()
runPhoneProperties = do
  putStrLn "SafePhone Property Tests"
  putStrLn "========================"
  putStrLn "prop_e164Parses: PASS (proven by type)"
  putStrLn "prop_usFormatParses: PASS (proven by type)"
  putStrLn "prop_internationalParses: PASS (proven by type)"
  putStrLn "prop_tooShortFails: PASS (proven by type)"
  putStrLn "prop_tooLongFails: PASS (proven by type)"
  putStrLn "prop_lettersFail: PASS (proven by type)"
  putStrLn "prop_phoneEqReflexive: PASS (proven by type)"
  putStrLn ""
