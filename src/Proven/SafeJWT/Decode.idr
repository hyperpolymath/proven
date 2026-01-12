-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Safe JWT decoding without exceptions
|||
||| This module provides safe decoding of JWT tokens, parsing
||| the header and claims without throwing exceptions.
module Proven.SafeJWT.Decode

import Proven.Core
import Proven.SafeJWT.Types
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Base64URL Decoding
--------------------------------------------------------------------------------

||| Base64URL alphabet
base64UrlAlphabet : String
base64UrlAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

||| Get index of character in base64url alphabet
base64Index : Char -> Maybe Nat
base64Index c = findIndex (== c) (unpack base64UrlAlphabet)

||| Check if character is valid base64url
isBase64UrlChar : Char -> Bool
isBase64UrlChar c = isJust (base64Index c)

||| Decode a single base64url character to 6 bits
decodeChar : Char -> Maybe Bits8
decodeChar c = map cast (base64Index c)

||| Decode base64url string to bytes
||| Handles missing padding automatically
public export
base64UrlDecode : String -> Result JWTError (List Bits8)
base64UrlDecode "" = Ok []
base64UrlDecode s =
  let chars = unpack s
      -- Remove any padding that might be present
      noPadding = filter (/= '=') chars
  in if all isBase64UrlChar noPadding
       then Ok (decodeChars noPadding)
       else Err (Base64Error "input" "Invalid base64url character")
  where
    ||| Decode groups of 4 characters to 3 bytes
    decodeChars : List Char -> List Bits8
    decodeChars [] = []
    decodeChars [a] = []  -- Invalid, ignore
    decodeChars [a, b] =
      case (decodeChar a, decodeChar b) of
        (Just va, Just vb) =>
          let byte1 = (va `shiftL` 2) .|. (vb `shiftR` 4)
          in [byte1]
        _ => []
    decodeChars [a, b, c] =
      case (decodeChar a, decodeChar b, decodeChar c) of
        (Just va, Just vb, Just vc) =>
          let byte1 = (va `shiftL` 2) .|. (vb `shiftR` 4)
              byte2 = ((vb .&. 0x0F) `shiftL` 4) .|. (vc `shiftR` 2)
          in [byte1, byte2]
        _ => []
    decodeChars (a :: b :: c :: d :: rest) =
      case (decodeChar a, decodeChar b, decodeChar c, decodeChar d) of
        (Just va, Just vb, Just vc, Just vd) =>
          let byte1 = (va `shiftL` 2) .|. (vb `shiftR` 4)
              byte2 = ((vb .&. 0x0F) `shiftL` 4) .|. (vc `shiftR` 2)
              byte3 = ((vc .&. 0x03) `shiftL` 6) .|. vd
          in byte1 :: byte2 :: byte3 :: decodeChars rest
        _ => decodeChars rest

||| Convert bytes to UTF-8 string
public export
bytesToString : List Bits8 -> String
bytesToString bytes = pack (map (chr . cast) bytes)

||| Decode base64url to string
public export
base64UrlDecodeString : String -> Result JWTError String
base64UrlDecodeString s = map bytesToString (base64UrlDecode s)

--------------------------------------------------------------------------------
-- Token Splitting
--------------------------------------------------------------------------------

||| Split a JWT token into its three segments
public export
splitToken : String -> Result JWTError TokenSegments
splitToken token =
  let parts = splitOn '.' token
  in case parts of
       [h, p, s] => Ok (MkTokenSegments h p s)
       [h, p] => Ok (MkTokenSegments h p "")  -- Unsecured JWT
       _ => Err (InvalidFormat ("Expected 3 parts separated by '.', got " ++ show (length parts)))
  where
    splitOn : Char -> String -> List String
    splitOn delim s = go (unpack s) [] []
      where
        go : List Char -> List Char -> List String -> List String
        go [] current acc = reverse (pack (reverse current) :: acc)
        go (c :: cs) current acc =
          if c == delim
            then go cs [] (pack (reverse current) :: acc)
            else go cs (c :: current) acc

--------------------------------------------------------------------------------
-- Minimal JSON Parsing (for JWT claims)
--------------------------------------------------------------------------------

||| Simple JSON value type (sufficient for JWT)
data JsonValue : Type where
  JsonString : String -> JsonValue
  JsonNumber : Double -> JsonValue
  JsonInt : Integer -> JsonValue
  JsonBool : Bool -> JsonValue
  JsonNull : JsonValue
  JsonArray : List JsonValue -> JsonValue
  JsonObject : List (String, JsonValue) -> JsonValue

||| Parse a JSON string (simplified parser)
||| This handles the common cases in JWT headers and claims
parseJsonString : List Char -> (String, List Char)
parseJsonString chars = go chars []
  where
    go : List Char -> List Char -> (String, List Char)
    go [] acc = (pack (reverse acc), [])
    go ('"' :: rest) acc = (pack (reverse acc), rest)
    go ('\\' :: '"' :: rest) acc = go rest ('"' :: acc)
    go ('\\' :: 'n' :: rest) acc = go rest ('\n' :: acc)
    go ('\\' :: 'r' :: rest) acc = go rest ('\r' :: acc)
    go ('\\' :: 't' :: rest) acc = go rest ('\t' :: acc)
    go ('\\' :: '\\' :: rest) acc = go rest ('\\' :: acc)
    go ('\\' :: '/' :: rest) acc = go rest ('/' :: acc)
    go (c :: rest) acc = go rest (c :: acc)

||| Parse a JSON number
parseJsonNumber : List Char -> (Either Integer Double, List Char)
parseJsonNumber chars =
  let (numChars, rest) = span isNumChar chars
      numStr = pack numChars
  in if '.' `elem` numChars || 'e' `elem` numChars || 'E' `elem` numChars
       then (Right (cast numStr), rest)
       else (Left (cast numStr), rest)
  where
    isNumChar : Char -> Bool
    isNumChar c = isDigit c || c == '-' || c == '+' || c == '.' || c == 'e' || c == 'E'

||| Skip whitespace
skipWs : List Char -> List Char
skipWs = dropWhile isSpace

mutual
  ||| Parse a JSON value
  parseValue : List Char -> Maybe (JsonValue, List Char)
  parseValue chars =
    case skipWs chars of
      '"' :: rest =>
        let (s, remaining) = parseJsonString rest
        in Just (JsonString s, remaining)
      't' :: 'r' :: 'u' :: 'e' :: rest => Just (JsonBool True, rest)
      'f' :: 'a' :: 'l' :: 's' :: 'e' :: rest => Just (JsonBool False, rest)
      'n' :: 'u' :: 'l' :: 'l' :: rest => Just (JsonNull, rest)
      '[' :: rest => parseArray rest
      '{' :: rest => parseObject rest
      c :: rest =>
        if isDigit c || c == '-'
          then let (num, remaining) = parseJsonNumber (c :: rest)
               in case num of
                    Left i => Just (JsonInt i, remaining)
                    Right d => Just (JsonNumber d, remaining)
          else Nothing
      [] => Nothing

  ||| Parse a JSON array
  parseArray : List Char -> Maybe (JsonValue, List Char)
  parseArray chars = go (skipWs chars) []
    where
      go : List Char -> List JsonValue -> Maybe (JsonValue, List Char)
      go (']' :: rest) acc = Just (JsonArray (reverse acc), rest)
      go cs acc =
        case parseValue cs of
          Just (val, remaining) =>
            case skipWs remaining of
              ',' :: rest => go (skipWs rest) (val :: acc)
              ']' :: rest => Just (JsonArray (reverse (val :: acc)), rest)
              _ => Nothing
          Nothing => Nothing

  ||| Parse a JSON object
  parseObject : List Char -> Maybe (JsonValue, List Char)
  parseObject chars = go (skipWs chars) []
    where
      go : List Char -> List (String, JsonValue) -> Maybe (JsonValue, List Char)
      go ('}' :: rest) acc = Just (JsonObject (reverse acc), rest)
      go ('"' :: rest) acc =
        let (key, afterKey) = parseJsonString rest
        in case skipWs afterKey of
             ':' :: afterColon =>
               case parseValue (skipWs afterColon) of
                 Just (val, remaining) =>
                   case skipWs remaining of
                     ',' :: rest' => go (skipWs rest') ((key, val) :: acc)
                     '}' :: rest' => Just (JsonObject (reverse ((key, val) :: acc)), rest')
                     _ => Nothing
                 Nothing => Nothing
             _ => Nothing
      go _ _ = Nothing

||| Parse JSON string to JsonValue
parseJson : String -> Maybe JsonValue
parseJson s = map fst (parseValue (unpack s))

--------------------------------------------------------------------------------
-- Header Parsing
--------------------------------------------------------------------------------

||| Extract string from JsonValue
jsonString : JsonValue -> Maybe String
jsonString (JsonString s) = Just s
jsonString _ = Nothing

||| Extract integer from JsonValue
jsonInt : JsonValue -> Maybe Integer
jsonInt (JsonInt i) = Just i
jsonInt (JsonNumber d) = Just (cast d)
jsonInt _ = Nothing

||| Get field from JSON object
getField : String -> List (String, JsonValue) -> Maybe JsonValue
getField key [] = Nothing
getField key ((k, v) :: rest) = if k == key then Just v else getField key rest

||| Parse JWT header from JSON
public export
parseHeader : String -> Result JWTError JWTHeader
parseHeader json =
  case parseJson json of
    Just (JsonObject fields) =>
      case getField "alg" fields >>= jsonString of
        Just algStr =>
          case parseAlgorithm algStr of
            Just alg =>
              let typ = getField "typ" fields >>= jsonString
                  cty = getField "cty" fields >>= jsonString
                  kid = getField "kid" fields >>= jsonString
                  x5u = getField "x5u" fields >>= jsonString
                  x5t = getField "x5t" fields >>= jsonString
                  x5tS256 = getField "x5t#S256" fields >>= jsonString
              in Ok (MkJWTHeader alg typ cty kid x5u x5t x5tS256)
            Nothing => Err (UnsupportedAlgorithm algStr)
        Nothing => Err (JsonError "header" "Missing 'alg' field")
    Just _ => Err (JsonError "header" "Expected JSON object")
    Nothing => Err (JsonError "header" "Invalid JSON")

--------------------------------------------------------------------------------
-- Claims Parsing
--------------------------------------------------------------------------------

||| Convert JsonValue to ClaimValue
jsonToClaimValue : JsonValue -> ClaimValue
jsonToClaimValue (JsonString s) = ClaimString s
jsonToClaimValue (JsonInt i) = ClaimInt i
jsonToClaimValue (JsonNumber d) = ClaimInt (cast d)
jsonToClaimValue (JsonBool b) = ClaimBool b
jsonToClaimValue JsonNull = ClaimNull
jsonToClaimValue (JsonArray xs) = ClaimArray (mapMaybe getString xs)
  where
    getString : JsonValue -> Maybe String
    getString (JsonString s) = Just s
    getString _ = Nothing
jsonToClaimValue (JsonObject fields) = ClaimObject (map (\(k, v) => (k, jsonToClaimValue v)) fields)

||| Parse audience claim (can be string or array)
parseAudience : JsonValue -> Maybe (Either String (List String))
parseAudience (JsonString s) = Just (Left s)
parseAudience (JsonArray xs) =
  let strs = mapMaybe jsonString xs
  in if length strs == length xs
       then Just (Right strs)
       else Nothing
parseAudience _ = Nothing

||| Parse JWT claims from JSON
public export
parseClaims : String -> Result JWTError JWTClaims
parseClaims json =
  case parseJson json of
    Just (JsonObject fields) =>
      let iss = getField "iss" fields >>= jsonString
          sub = getField "sub" fields >>= jsonString
          aud = getField "aud" fields >>= parseAudience
          exp = getField "exp" fields >>= jsonInt
          nbf = getField "nbf" fields >>= jsonInt
          iat = getField "iat" fields >>= jsonInt
          jti = getField "jti" fields >>= jsonString
          -- Custom claims are everything else
          reserved = ["iss", "sub", "aud", "exp", "nbf", "iat", "jti"]
          customFields = filter (\(k, _) => not (k `elem` reserved)) fields
          custom = map (\(k, v) => (k, jsonToClaimValue v)) customFields
      in Ok (MkJWTClaims iss sub aud exp nbf iat jti custom)
    Just _ => Err (JsonError "payload" "Expected JSON object")
    Nothing => Err (JsonError "payload" "Invalid JSON")

--------------------------------------------------------------------------------
-- Full JWT Decoding
--------------------------------------------------------------------------------

||| Decode a JWT token without validating the signature
||| This is useful for inspecting tokens before validation
public export
decode : String -> Result JWTError DecodedJWT
decode token = do
  -- Split into segments
  segs <- splitToken token
  -- Decode and parse header
  headerJson <- base64UrlDecodeString segs.header
  header <- parseHeader headerJson
  -- Decode and parse payload
  payloadJson <- base64UrlDecodeString segs.payload
  claims <- parseClaims payloadJson
  -- Decode signature
  sig <- base64UrlDecode segs.signature
  -- Return decoded JWT
  Ok (MkDecodedJWT token header claims sig segs.header segs.payload)

||| Decode only the header (without parsing full token)
public export
decodeHeader : String -> Result JWTError JWTHeader
decodeHeader token = do
  segs <- splitToken token
  headerJson <- base64UrlDecodeString segs.header
  parseHeader headerJson

||| Decode only the claims/payload
public export
decodeClaims : String -> Result JWTError JWTClaims
decodeClaims token = do
  segs <- splitToken token
  payloadJson <- base64UrlDecodeString segs.payload
  parseClaims payloadJson

||| Get the algorithm from a token without full decoding
public export
getAlgorithm : String -> Result JWTError JWTAlgorithm
getAlgorithm token = map (.alg) (decodeHeader token)

||| Check if token uses a specific algorithm
public export
usesAlgorithm : JWTAlgorithm -> String -> Bool
usesAlgorithm alg token =
  case getAlgorithm token of
    Ok a => a == alg
    Err _ => False

--------------------------------------------------------------------------------
-- Token Inspection Utilities
--------------------------------------------------------------------------------

||| Get claim value by name
public export
getClaim : String -> JWTClaims -> Maybe ClaimValue
getClaim "iss" claims = map ClaimString claims.iss
getClaim "sub" claims = map ClaimString claims.sub
getClaim "aud" claims = map (either ClaimString ClaimArray) claims.aud
getClaim "exp" claims = map ClaimInt claims.exp
getClaim "nbf" claims = map ClaimInt claims.nbf
getClaim "iat" claims = map ClaimInt claims.iat
getClaim "jti" claims = map ClaimString claims.jti
getClaim name claims = lookup name claims.customClaims

||| Get string claim
public export
getStringClaim : String -> JWTClaims -> Maybe String
getStringClaim name claims =
  case getClaim name claims of
    Just (ClaimString s) => Just s
    _ => Nothing

||| Get integer claim
public export
getIntClaim : String -> JWTClaims -> Maybe Integer
getIntClaim name claims =
  case getClaim name claims of
    Just (ClaimInt i) => Just i
    _ => Nothing

||| Get boolean claim
public export
getBoolClaim : String -> JWTClaims -> Maybe Bool
getBoolClaim name claims =
  case getClaim name claims of
    Just (ClaimBool b) => Just b
    _ => Nothing

||| List all claim names
public export
claimNames : JWTClaims -> List String
claimNames claims =
  let registered = catMaybes
        [ map (const "iss") claims.iss
        , map (const "sub") claims.sub
        , map (const "aud") claims.aud
        , map (const "exp") claims.exp
        , map (const "nbf") claims.nbf
        , map (const "iat") claims.iat
        , map (const "jti") claims.jti
        ]
  in registered ++ map fst claims.customClaims
