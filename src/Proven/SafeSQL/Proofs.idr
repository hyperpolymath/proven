-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Concrete lexical and parameter-separation contracts.
||| These do not certify arbitrary Literal, SQLRaw, identifiers, driver behavior
||| or whole-query injection safety.
module Proven.SafeSQL.Proofs

import Proven.Core
import Proven.SafeSQL.Types
import public Proven.SafeSQL.Params
import Data.List
import Data.String
import Decidable.Equality

%default total

||| A complete literal has delimiters and an encoded body; it is not quote-free.
public export
escapeStringQuotesSafe : (d : SQLDialect) -> (s : String) ->
  RenderedLiteral (defaultQuoteMode d) (unpack s) (escapeString d s)
escapeStringQuotesSafe d s = renderLiteralCorrect (defaultQuoteMode d) s

public export
escapedBodyQuotesPaired : (d : SQLDialect) -> (s : String) ->
  PairedQuotes (bodyChars (defaultQuoteMode d) (unpack s))
escapedBodyQuotesPaired d s =
  encodingQuotesPaired (bodyCorrect (defaultQuoteMode d) (unpack s))

||| Accepted checked rendering has the same lexical evidence as the renderer.
public export
checkedLiteralCorrect : (mode : QuoteMode) -> (input, output : String) ->
  checkedEscapeString mode input = Ok output ->
  RenderedLiteral mode (unpack input) output
checkedLiteralCorrect mode input output accepted with ('\0' `elem` unpack input)
  checkedLiteralCorrect mode input output Refl | True impossible
  checkedLiteralCorrect mode input (renderLiteral mode input) Refl | False =
    renderLiteralCorrect mode input

||| Values cannot alter the template rendered by renderQuery. This says nothing
||| about whether the caller's Literal/NamedParam/Identifier fragments are safe.
public export
parameterValuesDoNotChangeSQL : (q : ParameterizedQuery) ->
  (values : List SQLValue) -> (named : List (String, SQLValue)) ->
  renderQuery (MkQuery q.fragments values named q.dialect) = renderQuery q
parameterValuesDoNotChangeSQL (MkQuery fragments params namedParams dialect) values named = Refl

public export
isRawValue : SQLValue -> Bool
isRawValue (SQLRaw _) = True
isRawValue _ = False

||| This evidence only excludes SQLRaw; it does not certify driver bindings.
public export
data BoundParameters : List SQLValue -> Type where
  NoParameters : BoundParameters []
  MoreParameters : isRawValue value = False -> BoundParameters rest ->
                   BoundParameters (value :: rest)

public export
checkBoundParameters : (values : List SQLValue) -> Maybe (BoundParameters values)
checkBoundParameters [] = Just NoParameters
checkBoundParameters (value :: rest) = case decEq (isRawValue value) False of
  No _ => Nothing
  Yes notRaw => case checkBoundParameters rest of
    Nothing => Nothing
    Just evidence => Just (MoreParameters notRaw evidence)

||| Legacy heuristic only. A successful scan is not an injection-safety proof.
public export
validateQuerySafety : ParameterizedQuery -> Result SQLError ()
validateQuerySafety q =
  let literals = mapMaybe getLiteral q.fragments
      combined = concat literals
  in if containsDangerousSQL combined
       then Err (InjectionDetected (analyzeForInjection combined) combined)
       else Ok ()
  where
    getLiteral : QueryFragment -> Maybe String
    getLiteral (Literal s) = Just s
    getLiteral _ = Nothing

||| Reject SQLRaw in BOTH positional and named parameter collections.
public export
validateQueryStrict : ParameterizedQuery -> Result SQLError ()
validateQueryStrict q =
  case checkBoundParameters (q.params ++ map Builtin.snd q.namedParams) of
    Nothing => Err (InvalidQuery "Query contains SQLRaw value - manual review required")
    Just _ => validateQuerySafety q

public export
strictParametersBound : (q : ParameterizedQuery) ->
  validateQueryStrict q = Ok () ->
  BoundParameters (q.params ++ map Builtin.snd q.namedParams)
strictParametersBound q accepted with (checkBoundParameters (q.params ++ map Builtin.snd q.namedParams))
  strictParametersBound q Refl | Nothing impossible
  strictParametersBound q accepted | Just evidence = evidence

public export
rawHeadImpossible : BoundParameters (SQLRaw text :: rest) -> Void
rawHeadImpossible (MoreParameters Refl _) impossible

||| Heuristic warning for development; absence of a match is not a proof.
public export
assertNoInjection : String -> Result SQLError ()
assertNoInjection s =
  case analyzeForInjection s of
    Safe => Ok ()
    sev => Err (InjectionDetected sev s)

public export
assertValidIdentifier : String -> Result SQLError SafeIdentifier
assertValidIdentifier s =
  case mkIdentifier s of
    Just ident => Ok ident
    Nothing => Err (InvalidIdentifier s "Contains invalid characters or is empty")

||| Exact scope, including the trust boundaries the former claims omitted.
public export
safetyGuarantees : String
safetyGuarantees = """
SQL text rendering constructs a quoted-literal grammar witness for an explicit
quote mode. Legacy rendering removes NUL; checked rendering rejects it.
Parameter values do not affect renderQuery's template. Strict validation
excludes SQLRaw from positional and named parameters. Caller-supplied SQL,
identifier and placeholder provenance, connection settings, encoding, driver
binding and whole-query injection safety require separate validation.
"""
