-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
||| Known library defects that property checks reproduce today.
|||
||| A known defect is a strict expected failure. `knownDefect d label holds`
||| takes the CORRECT expectation `holds`, evaluated now:
|||
|||   * False: the defect still reproduces. Printed, run continues.
|||   * True:  the defect no longer reproduces. The run FAILS until the
|||            constructor is removed here and the call becomes an ordinary
|||            check, so a fix (or a behaviour change) cannot go unrecorded.
|||
||| Removing a constructor is a compile error at every remaining use, so the
||| ledger and the checks cannot drift apart silently. This mirrors the C ABI
||| status ledger (bindings/c/include/proven-abi.tsv): what is owed is named,
||| and its silent disappearance is itself a failure.
module KnownDefects

import System

%default total

public export
data KnownDefect
  = ||| Proven.SafeTOML.parseTOMLWith is a documented stub: every non-blank
    ||| document parses to the empty document, and parseValue drops array and
    ||| inline-table elements. Needs a real TOML 1.0 parser.
    TomlParserStub
  | ||| Proven.SafeYAML.Parser rejects every block mapping and block sequence
    ||| with "SyntaxError ... Empty scalar". Rejections of YAML bombs that are
    ||| written as block collections are therefore vacuous evidence.
    YamlBlockCollectionsRejected
  | ||| Proven.SafeCookie accepts and emits a negative Max-Age. RFC 6265 §4.1.1
    ||| server grammar is non-zero-digit *DIGIT; but the library's own deletion
    ||| cookie emits Max-Age=0, which that grammar also excludes and §5.2.2
    ||| user agents accept. Owner decision: reject <= 0, or reject < 0 only.
    CookieNegativeMaxAge
  | ||| Proven.SafeXML.Parser never parses a DOCTYPE: billion-laughs and
    ||| external-DTD inputs are rejected only as "InvalidName '!'", not as
    ||| DTDNotAllowed, and allowDTD = True has no effect. Fail-closed by accident.
    XmlDoctypeUnparsed

public export
defectId : KnownDefect -> String
defectId TomlParserStub = "toml-parser-stub"
defectId YamlBlockCollectionsRejected = "yaml-block-collections-rejected"
defectId CookieNegativeMaxAge = "cookie-negative-max-age"
defectId XmlDoctypeUnparsed = "xml-doctype-unparsed"

||| Every ledger entry, for the end-of-run report.
public export
allKnownDefects : List KnownDefect
allKnownDefects = [TomlParserStub, YamlBlockCollectionsRejected, CookieNegativeMaxAge, XmlDoctypeUnparsed]

||| Strict expected failure; see the module documentation.
export
knownDefect : KnownDefect -> String -> Bool -> IO ()
knownDefect d label False =
  putStrLn ("KNOWN DEFECT (reproduces) [" ++ defectId d ++ "]: " ++ label)
knownDefect d label True = do
  putStrLn ("FAIL: known defect [" ++ defectId d ++ "] no longer reproduces: " ++ label)
  putStrLn "      Remove it from tests/properties/KnownDefects.idr and make this an ordinary check."
  exitFailure

export
reportKnownDefects : IO ()
reportKnownDefects = do
  putStrLn ("Known defects still reproducing (strict): " ++ show (length allKnownDefects))
  traverse_ (\d => putStrLn ("  - " ++ defectId d)) allKnownDefects
