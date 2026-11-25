-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module JWTSQLMain

import JWTSQLCases
import Proven.SafeSQL.Literal
import Proven.FFI.SafeJWT
import Data.List
import Data.String
import System

%default total

sampleJWT : JWTAlgorithm -> List Bits8 -> DecodedJWT
sampleJWT alg signature =
  MkDecodedJWT "fixture" (minimalHeader alg) emptyClaims signature "header" "payload"

jwtCases : List (String, Bool)
jwtCases = do
  alg <- algorithms
  key <- keys
  signature <- [[], [0], [1,2,3], map (cast . ord) (unpack "stub-signature")]
  let jwt = sampleJWT alg signature
  [ ("unavailable signature " ++ show alg, isErr (verifySignature key jwt))
  , ("permissive cannot authenticate " ++ show alg,
     isErr (Proven.SafeJWT.Validate.validate permissiveValidation key 0 jwt))
  , ("HMAC cannot authenticate", not (verifyHMAC alg [1,2] "header.payload" signature))
  , ("RSA cannot authenticate", not (verifyRSA alg key "header.payload" signature))
  , ("ECDSA cannot authenticate", not (verifyECDSA alg key "header.payload" signature))
  , ("EdDSA cannot authenticate", not (verifyEdDSA key "header.payload" signature))
  ]

errorCases : List (String, Bool)
errorCases =
  [ ("explicit backend error", case createHS256Token "secret" emptyClaims of
       Err (CryptoUnavailable HS256) => True
       _ => False)
  , ("unsigned creation rejected", case createToken (minimalHeader None) emptyClaims NoKey of
       Err AlgorithmNoneNotAllowed => True
       _ => False)
  , ("unsigned authentication rejected", case verifySignature NoKey (sampleJWT None []) of
       Err AlgorithmNoneNotAllowed => True
       _ => False)
  , ("claims checks still succeed", isOk (validateDecoded permissiveValidation 0 (sampleJWT HS256 [])))
  , ("expiration still rejects", isErr (validateExp 1000 (MkClockSkew 0 0) (claimsWithExp "alice" 999)))
  , ("unexpired claims pass", isOk (validateExp 1000 (MkClockSkew 0 0) (claimsWithExp "alice" 1001)))
  ]

covering
inspectionCases : List (String, Bool)
inspectionCases =
  let token = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJhbGljZSJ9.ZmFrZQ"
  in [ ("inspection still decodes", isOk (inspectJWT token))
     , ("inspection preserves subject", case inspectJWT token of
          Ok jwt => jwt.claims.sub == Just "alice"
          Err _ => False)
     , ("claims-only remains inspection", isOk (validateClaimsOnly permissiveValidation 0 token))
     , ("high-level authentication rejects", isErr (verifyJWT "secret" 0 token))
     , ("Idris verification export rejects", fst (proven_idris_jwt_verify "secret" 0 token) == 1)
     , ("Idris Boolean export rejects", proven_idris_jwt_is_valid "secret" 0 token == 0)
     , ("Idris inspection export works", fst (proven_idris_jwt_decode token) == 0)
     ]

alphabet : List Char
alphabet = ['a', '\'', '\\', '\n', '\r', ';', chr 0]

stringsOf : Nat -> List (List Char)
stringsOf Z = [[]]
stringsOf (S n) = do c <- alphabet; rest <- stringsOf n; pure (c :: rest)

inputs : List (List Char)
inputs = concatMap stringsOf [0..3]

checkedCases : List (String, Bool)
checkedCases = do
  mode <- [StandardQuotes, BackslashEscapes]
  chars <- inputs
  let input = pack chars
  [ ("checked NUL policy", isErr (checkedEscapeString mode input) == (chr 0 `elem` chars))
  , ("checked result is actual renderer", case checkedEscapeString mode input of
       Err _ => chr 0 `elem` chars
       Ok output => output == renderLiteral mode input)
  ]

||| Independent expected UTF-8 bytes for the SQLite receiver tests.
asciiHex : Char -> String
asciiHex 'a' = "61"
asciiHex '\'' = "27"
asciiHex '\\' = "5C"
asciiHex '\n' = "0A"
asciiHex '\r' = "0D"
asciiHex ';' = "3B"
asciiHex _ = ""

sqliteCases : List (String, String)
sqliteCases = map (\cs => (pack cs, concatMap asciiHex cs)) inputs ++
  [ ("O'Brien", "4F27427269656E")
  , ("'; DROP TABLE sentinel;--", "273B2044524F50205441424C452073656E74696E656C3B2D2D")
  , ("/*comment*/", "2F2A636F6D6D656E742A2F")
  , (pack [chr 256, chr 128512], "C480F09F9880")
  , (pack ['x', chr 0, chr 233], "78C3A9")
  ]

sqliteStatement : (String, String) -> String
sqliteStatement (input, expectedHex) =
  "INSERT INTO assertions(ok) VALUES(CAST(" ++ escapeString SQLite input ++
  " AS BLOB)=X'" ++ expectedHex ++ "');"

emitSQLite : IO ()
emitSQLite = do
  putStrLn "CREATE TABLE sentinel(n INTEGER); INSERT INTO sentinel VALUES(1);"
  putStrLn "CREATE TABLE assertions(ok INTEGER NOT NULL CHECK(ok=1));"
  traverse_ (putStrLn . sqliteStatement) sqliteCases
  putStrLn ("INSERT INTO assertions(ok) VALUES((SELECT COUNT(*) FROM assertions)=" ++ show (length sqliteCases) ++ ");")
  putStrLn "INSERT INTO assertions(ok) VALUES((SELECT COUNT(*) FROM sentinel)=1);"
  putStrLn ("-- " ++ show (length sqliteCases) ++ " SQLite literal/byte cases plus count and sentinel assertions")

covering
main : IO ()
main = do
  args <- getArgs
  if "--sqlite" `elem` args
    then emitSQLite
    else do
      let tests = commonCases ++ jwtCases ++ errorCases ++ inspectionCases ++ checkedCases
      let failures = filter (not . snd) tests
      traverse_ (\(name, _) => putStrLn ("FAIL: " ++ name)) failures
      if null failures
        then putStrLn ("PASS: " ++ show (length tests) ++ " JWT/SQL runtime assertions")
        else do
          putStrLn (show (length failures) ++ " JWT/SQL regression assertion(s) failed")
          exitFailure
