-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module JWTSQLProofRegressions

import Proven.SafeJWT
import Proven.SafeJWT.Containment
import Proven.SafeSQL
import Proven.SafeString.Proofs
import Proven.SafeString.Escape

%default total

noTokens : (h : JWTHeader) -> (c : JWTClaims) -> (k : SigningKey) -> (s : String) ->
           createToken h c k = Ok s -> Void
noTokens = creationNeverSucceeds

noAuthentication : (opts : ValidationOptions) -> (k : SigningKey) ->
  (now : Integer) -> (jwt : DecodedJWT) -> (result : ValidatedJWT) ->
  Proven.SafeJWT.Validate.validate opts k now jwt = Ok result -> Void
noAuthentication = validationNeverSucceeds

literalMatchesProduction : (d : SQLDialect) -> (s : String) ->
  RenderedLiteral (defaultQuoteMode d) (unpack s) (escapeString d s)
literalMatchesProduction = escapeStringQuotesSafe

bodyQuotesArePaired : (d : SQLDialect) -> (s : String) ->
  PairedQuotes (bodyChars (defaultQuoteMode d) (unpack s))
bodyQuotesArePaired = escapedBodyQuotesPaired

legacyBodyMatchesProduction : (s : String) ->
  RenderedBody BackslashEscapes (unpack s) (escapeSQL s)
legacyBodyMatchesProduction = escapeSQLSafe

parametersCannotChangeTemplate : (q : ParameterizedQuery) ->
  (values : List SQLValue) -> (named : List (String, SQLValue)) ->
  renderQuery (MkQuery q.fragments values named q.dialect) = renderQuery q
parametersCannotChangeTemplate = parameterValuesDoNotChangeSQL

bothCollectionsAreChecked : (q : ParameterizedQuery) ->
  validateQueryStrict q = Ok () ->
  BoundParameters (q.params ++ map Builtin.snd q.namedParams)
bothCollectionsAreChecked = strictParametersBound

-- Non-vacuous positive grammar witnesses.
apostropheEncoding : BodyEncoding StandardQuotes ['\''] ['\'', '\'']
apostropheEncoding = DoubleQuote EndBody

slashEncoding : BodyEncoding BackslashEscapes ['\\'] ['\\', '\\']
slashEncoding = DoubleSlash EndBody

nulPolicy : BodyEncoding StandardQuotes ['\0'] []
nulPolicy = DropNull EndBody

emptyParameters : BoundParameters []
emptyParameters = NoParameters

integerParameter : BoundParameters [SQLInt 1]
integerParameter = MoreParameters Refl NoParameters
