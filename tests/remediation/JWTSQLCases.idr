-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module JWTSQLCases

import public Proven.SafeJWT
import public Proven.SafeSQL
import Proven.SafeString.Escape
import Data.List
import Data.String

%default total

public export
algorithms : List JWTAlgorithm
algorithms = [None, HS256, HS384, HS512, RS256, RS384, RS512,
              ES256, ES384, ES512, PS256, PS384, PS512, EdDSA]

public export
keys : List SigningKey
keys = [NoKey, SecretKey [], SecretKey [1,2,3], RSAPublicKey [] [],
        RSAPrivateKey [] [] [], ECPublicKey "P-256" [] [],
        ECPrivateKey "P-256" [] [] [], EdPublicKey [], EdPrivateKey []]

public export
claimSamples : List JWTClaims
claimSamples = [emptyClaims, claimsWithExp "alice" 1000,
               withStringClaim "quoted\"key" "text\n\"quote" emptyClaims,
               withCustomClaim "nested" (ClaimObject [("x", ClaimString "value")]) emptyClaims]

public export
sqlQuery : List SQLValue -> List (String, SQLValue) -> ParameterizedQuery
sqlQuery values named = MkQuery [Literal "SELECT ", Param 0] values named SQLite

||| These cases use APIs present at the audited baseline, allowing executable
||| before/after comparison without relying on the new proof vocabulary.
public export
commonCases : List (String, Bool)
commonCases =
  (do alg <- algorithms
      key <- keys
      claims <- claimSamples
      pure ("no fabricated token " ++ show alg,
            isErr (createToken (minimalHeader alg) claims key))) ++
  (do secret <- ["", "secret", "changed", pack [chr 256]]
      claims <- claimSamples
      pure ("HS256 creation rejects missing crypto", isErr (createHS256Token secret claims))) ++
  [ ("SQLite preserves newline", escapeString SQLite "a\nb" == "'a\nb'")
  , ("SQLite preserves carriage return", escapeString SQLite "a\rb" == "'a\rb'")
  , ("PostgreSQL standard strings preserve slash", escapeString PostgreSQL "a\\b" == "'a\\b'")
  , ("PostgreSQL preserves newline", escapeString PostgreSQL "a\nb" == "'a\nb'")
  , ("MSSQL uses a single literal", escapeString MSSQL "a\nb" == "'a\nb'")
  , ("Oracle preserves newline", escapeString Oracle "a\nb" == "'a\nb'")
  , ("quote doubling", escapeString SQLite "O'Brien" == "'O''Brien'")
  , ("empty literal", escapeString SQLite "" == "''")
  , ("SQLite preserves slash", escapeString SQLite "\\n" == "'\\n'")
  , ("MySQL escapes slash", escapeString MySQL "\\n" == "'\\\\n'")
  , ("legacy NUL removal", escapeString SQLite (pack ['a', chr 0, 'b']) == "'ab'")
  , ("legacy body contract", Proven.SafeString.Escape.escapeSQL "'\\" == "''\\\\")
  , ("reject named SQLRaw", isErr (validateQueryStrict (sqlQuery [] [("x", SQLRaw "1")])))
  , ("reject later named SQLRaw", isErr (validateQueryStrict (sqlQuery [] [("x", SQLInt 1), ("y", SQLRaw "2")])))
  , ("reject positional SQLRaw", isErr (validateQueryStrict (sqlQuery [SQLRaw "1"] [])))
  , ("ordinary bound data allowed", isOk (validateQueryStrict (sqlQuery [SQLText "'; DROP TABLE t;--"] [("x", SQLInt 1)])))
  , ("parameters do not change SQL", renderQuery (sqlQuery [SQLText "attacker"] [("x", SQLInt 2)]) ==
                                     renderQuery (sqlQuery [SQLInt 1] [("x", SQLText "different")] ))
  ]
