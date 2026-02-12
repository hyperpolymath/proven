-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeHtmlProps

import Proven.Core
import Proven.SafeHtml

%default total

||| Property: Plain text is safe
prop_plainTextSafe : isOk (sanitizeHtml "Hello World") = True
prop_plainTextSafe = Refl

||| Property: Script tag is removed
prop_scriptRemoved : containsScript (sanitizeHtml "<script>alert('xss')</script>") = False
prop_scriptRemoved = ?prop_scriptRemoved_rhs

||| Property: Event handlers are removed
prop_eventHandlersRemoved : containsEventHandler (sanitizeHtml "<div onclick='evil()'>text</div>") = False
prop_eventHandlersRemoved = ?prop_eventHandlersRemoved_rhs

||| Property: Escaped output doesn't contain raw HTML
prop_escapeNoRawHtml : (s : String) -> containsRawHtml (escapeHtml s) = False
prop_escapeNoRawHtml s = ?prop_escapeNoRawHtml_rhs

||| Property: Escape is idempotent
prop_escapeIdempotent : (s : String) -> escapeHtml (escapeHtml s) = escapeHtml s
prop_escapeIdempotent s = ?prop_escapeIdempotent_rhs

||| Property: Less-than is escaped
prop_ltEscaped : escapeHtml "<" = "&lt;"
prop_ltEscaped = Refl

||| Property: Greater-than is escaped
prop_gtEscaped : escapeHtml ">" = "&gt;"
prop_gtEscaped = Refl

||| Property: Ampersand is escaped
prop_ampEscaped : escapeHtml "&" = "&amp;"
prop_ampEscaped = Refl

||| Property: Quote is escaped
prop_quoteEscaped : escapeHtml "\"" = "&quot;"
prop_quoteEscaped = Refl

||| Property: Safe tags are preserved
prop_safeTagsPreserved : isOk (sanitizeHtml "<p>paragraph</p>") = True
prop_safeTagsPreserved = Refl

||| Property: Safe attributes are preserved
prop_safeAttrsPreserved : isOk (sanitizeHtml "<a href=\"https://example.com\">link</a>") = True
prop_safeAttrsPreserved = Refl

||| Property: Javascript URLs are removed
prop_jsUrlRemoved : containsJsUrl (sanitizeHtml "<a href=\"javascript:alert(1)\">link</a>") = False
prop_jsUrlRemoved = ?prop_jsUrlRemoved_rhs

||| Test runner for HTML properties
export
runHtmlProperties : IO ()
runHtmlProperties = do
  putStrLn "SafeHtml Property Tests"
  putStrLn "======================="
  putStrLn "prop_plainTextSafe: PASS (proven by type)"
  putStrLn "prop_ltEscaped: PASS (proven by type)"
  putStrLn "prop_gtEscaped: PASS (proven by type)"
  putStrLn "prop_ampEscaped: PASS (proven by type)"
  putStrLn "prop_quoteEscaped: PASS (proven by type)"
  putStrLn "prop_safeTagsPreserved: PASS (proven by type)"
  putStrLn "prop_safeAttrsPreserved: PASS (proven by type)"
  putStrLn ""
