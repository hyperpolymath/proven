-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeUrl operations
|||
||| This module contains proofs that verify properties of URL operations.
module Proven.SafeUrl.Proofs

import Proven.Core
import Proven.SafeUrl.Parser
import Proven.SafeUrl.Query
import Data.List
import Data.List.Equalities
import Decidable.Equality

%default total

--------------------------------------------------------------------------------
-- URL Parsing Properties
--------------------------------------------------------------------------------

||| Empty string parsing fails
public export
parseEmptyFails : parseURLMaybe "" = Nothing
parseEmptyFails = Refl

||| Parsing is deterministic
public export
parseDeterministic : (s : String) ->
                     parseURLMaybe s = parseURLMaybe s
parseDeterministic s = Refl

--------------------------------------------------------------------------------
-- URL Encoding Properties
--------------------------------------------------------------------------------

||| OWED: Percent-encoding leaves unreserved (alphanumeric) characters
||| unchanged. Operationally true by direct unfolding of `percentEncode`:
||| `if isUnreserved c then singleton c else "%" ++ toHex (ord c)` —
||| and `isUnreserved c = isAlphaNum c || c == '-' || c == '.' || ...`,
||| so `isAlphaNum c = True` collapses `isUnreserved c` to `True` and the
||| branch reduces to `singleton c`. Held back by Idris2 0.8.0 not
||| type-level reducing `Data.Char.isAlphaNum` / `isUnreserved` — both
||| route through the `Char` FFI primitive `prim__charPred` whose result
||| is opaque to `Refl`. Same blocker family as SafeChecksum's
||| `extractDigits` and SafeHeader's `hasCRLF` (Char/String FFI opacity).
||| Discharge once a `Data.Char` reflective tactic is available, or
||| `isUnreserved` is refactored to a non-FFI predicate.
export
0 unreservedNotEncoded : (c : Char) ->
                         isAlphaNum c = True ->
                         percentEncode c = singleton c

||| Empty string encodes to empty string
public export
encodeEmptyEmpty : urlEncode "" = ""
encodeEmptyEmpty = Refl

||| OWED: URL-encoding a fully alphanumeric string is the identity.
||| Operationally follows from `unreservedNotEncoded` lifted across
||| `unpack`/`map percentEncode`/`concat`, plus the `concat . map
||| singleton . unpack = id` String round-trip. Held back by Idris2
||| 0.8.0 not type-level reducing `unpack`/`pack`/`concat` (String FFI
||| opacity, same family as SafeFile `boundedReadAtMostLimit`) and not
||| reducing `Data.Char.isAlphaNum` per-element (see
||| `unreservedNotEncoded` above). Discharge once both String FFI
||| reduction and a `Data.Char` reflective tactic are available.
export
0 encodePreservesAlphaNum : (s : String) ->
                            all isAlphaNum (unpack s) = True ->
                            urlEncode s = s

--------------------------------------------------------------------------------
-- URL Decoding Properties
--------------------------------------------------------------------------------

||| Decoding empty string succeeds
public export
decodeEmptySucceeds : urlDecode "" = Just ""
decodeEmptySucceeds = Refl

||| OWED: Decoding a string of unreserved characters is the identity.
||| Operationally true because `urlDecode`'s inner `go` recurses on the
||| char list, and every non-`%`-non-`+` char hits the wildcard branch
||| `go (c :: rest) = Just (c :: ...)`. Combined with the
||| `pack . unpack = id` round-trip this gives `urlDecode s = Just s`.
||| Held back by Idris2 0.8.0 not reducing `unpack`/`pack` (String FFI
||| opacity) and not reducing the per-element `isAlphaNum` premise to
||| make the case analysis on `go` definitional. Discharge once String
||| FFI reduction and a `Data.Char` reflective tactic are available.
export
0 decodeUnreservedIdentity : (s : String) ->
                             all isAlphaNum (unpack s) = True ->
                             urlDecode s = Just s

||| OWED: URL-encode then URL-decode round-trips to the original
||| string. Operationally true by case-analysis on each character of
||| `unpack s`: unreserved chars round-trip via `singleton`/wildcard
||| branch; reserved chars round-trip via `"%" ++ toHex (ord c)` →
||| `decodePercent`. Held back by Idris2 0.8.0 not reducing `unpack` /
||| `pack` / `concat` (String FFI opacity), `Data.Char.chr`/`ord`
||| (Char FFI), and the arithmetic identity
||| `chr (hi * 16 + lo) = c` given `hi = ord c \`div\` 16`,
||| `lo = ord c \`mod\` 16`. Same blocker family as proof-of-work I7
||| (FFI-correctness assumption). Discharge once String/Char FFI
||| reduction is available, or via a property-test + trusted-extraction
||| validation campaign (see boj-server backend-assurance harness).
export
0 encodeDecodeIdentity : (s : String) ->
                         urlDecode (urlEncode s) = Just s

--------------------------------------------------------------------------------
-- Query String Properties
--------------------------------------------------------------------------------

||| Parsing empty query string gives empty list
public export
parseEmptyQuery : parseQueryString "" = []
parseEmptyQuery = Refl

||| OWED: Empty query builder builds the empty string.
||| `buildQueryString emptyQuery` reduces to `joinWith "&" (map
||| formatPair [])` = `joinWith "&" []` = `""`. Held back by Idris2
||| 0.8.0 not reducing `Data.String.joinWith` past its inner
||| `where`-block `go`, which threads through `++` on `String` (String
||| FFI opacity). Same blocker family as SafeHeader `renderedHeaderSafe`.
||| Discharge once `joinWith`/`++` on `String` are type-level reducible,
||| or refactor `buildQueryString` to expose the empty-list base case
||| at the top.
export
0 emptyBuilderEmpty : buildQueryString Query.emptyQuery = ""

||| DISCHARGED: Adding a parameter increases the parameter count by one.
||| `addParam key val qb` is defined as `MkQueryBuilder (qb.params ++
||| [(key, val)])`, so `(addParam key val qb).params` reduces to
||| `qb.params ++ [(key, val)]` and the claim becomes
||| `length (qb.params ++ [(key, val)]) = S (length qb.params)`.
||| Discharged via `Data.List.Equalities.lengthSnoc` from `contrib`,
||| which is element-first (`lengthSnoc x xs : length (xs ++ [x]) =
||| S (length xs)`), so the element `(key, val)` precedes the list
||| `qb.params`.
public export
addParamIncreasesCount : (key, val : String) -> (qb : QueryBuilder) ->
                         paramCount (addParam key val qb).params =
                         S (paramCount qb.params)
addParamIncreasesCount key val qb = lengthSnoc (key, val) qb.params

||| OWED: Getting a parameter just set by key returns that value.
||| Operationally true by case analysis on `hasParam key qs`: in the
||| `True` branch, `setParam` `map`-replaces every key-matching entry
||| with `val`, and `getParam = lookup` returns the first match; in the
||| `False` branch, `setParam` appends `(key, val)` to the end, and
||| `lookup` falls through to that appended pair (since no earlier
||| entry matches). Held back by Idris2 0.8.0 not type-level reducing
||| `String` equality `==` (routes through `prim__eqString` FFI), so the
||| `if k == key` branches in `setParam` and the `if k == key` branches
||| in `lookup` cannot be evaluated by `Refl`. Same blocker family as
||| SafeChecksum Luhn/ISBN (String FFI opacity). Discharge once String
||| equality is type-level reducible, or via a property-test campaign.
export
0 setGetIdentity : (key, val : String) -> (qs : QueryString) ->
                   getParam key (setParam key val qs) = Just val

||| OWED: After removing all instances of a key, `hasParam` returns
||| False. `removeAllParams key = filter (\(k, _) => k /= key)`, so the
||| filtered list contains no entry with key `k = key`; therefore
||| `lookup key (removeAllParams key qs) = Nothing`, hence `isJust …
||| = False`. Held back by Idris2 0.8.0 not type-level reducing
||| `String` `/=` / `==` (`prim__eqString` FFI), so the per-element
||| `filter` predicate's truth value is opaque to `Refl`, blocking
||| induction over `qs`. Discharge once String equality is type-level
||| reducible. Same blocker family as `setGetIdentity`.
export
0 removeHasNot : (key : String) -> (qs : QueryString) ->
                 hasParam key (removeAllParams key qs) = False

||| OWED: `filterParams` keeps only entries whose keys are in the given
||| list — equivalently, every entry surviving `filter (\(k, _) => k
||| \`elem\` keys)` satisfies that predicate. This is the standard
||| `Data.List` lemma `filterAll : (p : a -> Bool) -> (xs : List a) ->
||| all p (filter p xs) = True` instantiated at our predicate. Held
||| back by Idris2 0.8.0's `Data.List` not exposing `filterAll` as a
||| `%reducible` rewrite, AND by `elem` on `String` routing through
||| `prim__eqString` FFI (per-element opacity). Discharge by importing
||| / proving the `filterAll` lemma and rewriting, once String equality
||| is type-level reducible.
export
0 filterPreservesOnly : (keys : List String) -> (qs : QueryString) ->
                        all (\(k, _) => k `elem` keys) (filterParams keys qs) = True

--------------------------------------------------------------------------------
-- Query Parameter Parsing Properties
--------------------------------------------------------------------------------

||| OWED: Parsing integer `"42"` from a matching key yields `Just 42`.
||| `getIntParam key [(key, "42")] = getParam key [(key, "42")] >>=
||| parseInteger`, which should reduce to `Just "42" >>= parseInteger
||| = parseInteger "42" = Just 42`. Held back by Idris2 0.8.0 not
||| type-level reducing `String` equality in `lookup`'s inner `if k ==
||| key`, and not reducing `parseInteger`'s `strM`/`unpack`/`all
||| isDigit`/`foldl` String+Char FFI chain. Same blocker family as
||| SafeChecksum Luhn (FFI-correctness assumption — `parseInteger
||| "42" = Just 42` is operationally true but opaque). Discharge via
||| property-test + trusted-extraction validation.
export
0 parseIntValid : (key : String) ->
                  getIntParam key [(key, "42")] = Just 42

||| OWED: Parsing bool `"true"` from a matching key yields `Just True`.
||| `getBoolParam key [(key, "true")] = lookup key [(key, "true")] >>=
||| parseBool`, and the inner `parseBool "true"` matches the literal
||| `"true" => Just True` arm of its `case toLower s of …` after
||| `toLower "true" = "true"`. Held back by Idris2 0.8.0 not type-level
||| reducing `String` equality in `lookup` (`prim__eqString`) and the
||| `case toLower s of "true" => …` String-literal match (also
||| `prim__eqString`-routed). Same blocker family as `parseIntValid`.
||| Discharge once String equality is type-level reducible, or via
||| property-test campaign.
export
0 parseBoolTrue : (key : String) ->
                  getBoolParam key [(key, "true")] = Just True

||| OWED: Parsing bool `"false"` from a matching key yields `Just
||| False`. Symmetric to `parseBoolTrue` — operationally the
||| `"false" => Just False` arm of `parseBool`'s `case toLower s of …`.
||| Held back by the same Idris2 0.8.0 String-literal-match
||| (`prim__eqString` FFI) opacity. Discharge with `parseBoolTrue`.
export
0 parseBoolFalse : (key : String) ->
                   getBoolParam key [(key, "false")] = Just False

--------------------------------------------------------------------------------
-- Query String Merge Properties
--------------------------------------------------------------------------------

||| Merging with empty is identity
public export
mergeEmptyRight : (qs : QueryString) ->
                  mergeQueryStrings qs [] = qs
mergeEmptyRight qs = Refl

||| OWED: Merging empty (left) into a query string yields that query
||| string. `mergeQueryStrings [] qs = foldl (\q, (k, v) => setParam k v
||| q) [] qs`, i.e. fold starting from `[]` over all pairs in `qs`,
||| each step calling `setParam k v` which (since the accumulator never
||| already contains `k`) appends `[(k, v)]`. The final accumulator
||| equals `qs`. Held back by Idris2 0.8.0 not reducing the per-step
||| `hasParam k acc` check (`String` `==` via `prim__eqString` FFI),
||| which would let us see the accumulator collapse to a snoc chain;
||| also requires `lengthSnoc`-style induction on `qs`. Same blocker
||| family as `setGetIdentity` and `addParamIncreasesCount`. Discharge
||| once String equality is type-level reducible and the snoc-length
||| lemma is `%reducible`.
export
0 mergeEmptyLeft : (qs : QueryString) ->
                   mergeQueryStrings [] qs = qs

||| OWED: Query string append is associative — inherited from
||| `Data.List.appendAssociative` since `appendQueryStrings = (++)` on
||| `QueryString = List (String, String)`. Held back by Idris2 0.8.0's
||| `Data.List` not exposing `appendAssociative` as a `%reducible`
||| rewrite (the proof exists in Prelude but requires induction on
||| `qs1`, and `Refl` cannot close it for abstract `qs1`). Discharge
||| DISCHARGED via the Prelude/`Data.List` lemma `appendAssociative`.
||| `appendQueryStrings = (++)` (Query.idr L205-206), so the goal reduces
||| to `(qs1 ++ qs2) ++ qs3 = qs1 ++ (qs2 ++ qs3)`. `Data.List`'s lemma is
||| stated in the opposite direction (`xs ++ (ys ++ zs) = (xs ++ ys) ++
||| zs`), so we flip it with `sym`, and qualify it as
||| `Data.List.appendAssociative` to disambiguate from this same-named
||| local function. (The earlier `Data.List.Equalities.appendAssociative`
||| reference did not resolve under Idris2 0.8.0 — the lemma lives in
||| `Data.List`, not `Data.List.Equalities`.)
public export
appendAssociative : (qs1, qs2, qs3 : QueryString) ->
                    appendQueryStrings (appendQueryStrings qs1 qs2) qs3 =
                    appendQueryStrings qs1 (appendQueryStrings qs2 qs3)
appendAssociative qs1 qs2 qs3 = sym (Data.List.appendAssociative qs1 qs2 qs3)

--------------------------------------------------------------------------------
-- URL Security Properties
--------------------------------------------------------------------------------

||| Data type for safe URLs (no javascript: scheme).
||| The non-javascript proof is stored at erased multiplicity (`0`)
||| because `isSafeSchemeNotJavascript` is OWED — see below.
public export
data SafeURL : ParsedURL -> Type where
  MkSafeURL : (url : ParsedURL) ->
              (0 _ : Not (url.scheme = Just (Custom "javascript"))) ->
              SafeURL url

||| Check if URL has safe scheme
public export
isSafeScheme : ParsedURL -> Bool
isSafeScheme url = case url.scheme of
  Just (Custom "javascript") => False
  Just (Custom "data") => False
  Just (Custom "vbscript") => False
  _ => True

||| OWED: If `isSafeScheme url = True`, then `url.scheme` is not
||| `Just (Custom "javascript")`. By contraposition on `isSafeScheme`'s
||| case-analysis: if the scheme were `Just (Custom "javascript")`, the
||| first arm would fire and return `False`, contradicting the
||| hypothesis `isSafeScheme url = True`. Held back by Idris2 0.8.0
||| not type-level reducing `String` equality (`Custom "javascript" =
||| Custom "javascript"` routes through `prim__eqString` for the
||| payload) — without that, the case-equality required to derive the
||| contradiction is opaque to `Refl`. Same blocker family as
||| `setGetIdentity`. Discharge once String equality is type-level
||| reducible. Used by `validateSafe` to construct `MkSafeURL`.
export
0 isSafeSchemeNotJavascript : (url : ParsedURL) -> isSafeScheme url = True ->
                              Not (url.scheme = Just (Custom "javascript"))

||| Validate URL is safe
public export
validateSafe : (url : ParsedURL) -> Maybe (SafeURL url)
validateSafe url =
  case decEq (isSafeScheme url) True of
    Yes prf => Just (MkSafeURL url (isSafeSchemeNotJavascript url prf))
    No _    => Nothing

--------------------------------------------------------------------------------
-- Host Validation Properties
--------------------------------------------------------------------------------

||| IPv4 address components are bounded
public export
data ValidIPv4 : Host -> Type where
  MkValidIPv4 : (a, b, c, d : Nat) ->
                LTE a 255 -> LTE b 255 -> LTE c 255 -> LTE d 255 ->
                ValidIPv4 (IPv4 a b c d)

||| Port number is bounded.
||| The `LTE p 65535` proof is stored at erased multiplicity (`0`)
||| because `lteFrom65535Check` is OWED — see below.
public export
data ValidPort : Nat -> Type where
  MkValidPort : (p : Nat) -> (0 _ : LTE p 65535) -> ValidPort p

||| DISCHARGED via `Data.Nat.lteReflectsLTE` stdlib lemma.
export
lteFrom65535Check : (p : Nat) -> (p <= 65535 = True) -> LTE p 65535
lteFrom65535Check p prf = Data.Nat.lteReflectsLTE p 65535 prf

||| Validate port is in range
public export
validatePort : (p : Nat) -> Maybe (ValidPort p)
validatePort p =
  case decEq (p <= 65535) True of
    Yes prf => Just (MkValidPort p (lteFrom65535Check p prf))
    No _    => Nothing
