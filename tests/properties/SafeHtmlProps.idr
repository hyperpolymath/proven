-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeHtmlProps

import Proven.Core
import Proven.SafeHtml

%default total

||| Property: Plain text is safe
prop_plainTextSafe : isOk (sanitizeHtml "Hello World") = True
prop_plainTextSafe = Refl

||| OWED: Script tag is removed
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_scriptRemoved_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_scriptRemoved : containsScript (sanitizeHtml "<script>alert('xss')</script>") = False

||| OWED: Event handlers are removed
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_eventHandlersRemoved_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_eventHandlersRemoved : containsEventHandler (sanitizeHtml "<div onclick='evil()'>text</div>") = False

||| OWED: Escaped output doesn't contain raw HTML
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_escapeNoRawHtml_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_escapeNoRawHtml : (s : String) -> containsRawHtml (escapeHtml s) = False

||| OWED: Escape is idempotent
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_escapeIdempotent_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_escapeIdempotent : (s : String) -> escapeHtml (escapeHtml s) = escapeHtml s

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

||| OWED: Javascript URLs are removed
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_jsUrlRemoved_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_jsUrlRemoved : containsJsUrl (sanitizeHtml "<a href=\"javascript:alert(1)\">link</a>") = False

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
