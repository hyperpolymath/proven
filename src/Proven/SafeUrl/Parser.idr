-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Safe URL Parser
|||
||| A total URL parser that returns Maybe/Result instead of throwing exceptions.
||| Handles malformed URLs gracefully according to RFC 3986.
module Proven.SafeUrl.Parser

import Proven.Core
import Data.Char
import Data.List
import Data.Maybe
import Data.String

%default total

--------------------------------------------------------------------------------
-- Parser Types
--------------------------------------------------------------------------------

||| URL scheme (protocol)
public export
data Scheme = HTTP | HTTPS | FTP | FTPS | File | Mailto | Tel | Custom String

public export
Eq Scheme where
  HTTP == HTTP = True
  HTTPS == HTTPS = True
  FTP == FTP = True
  FTPS == FTPS = True
  File == File = True
  Mailto == Mailto = True
  Tel == Tel = True
  (Custom a) == (Custom b) = a == b
  _ == _ = False

public export
Show Scheme where
  show HTTP = "http"
  show HTTPS = "https"
  show FTP = "ftp"
  show FTPS = "ftps"
  show File = "file"
  show Mailto = "mailto"
  show Tel = "tel"
  show (Custom s) = s

||| Host component
public export
data Host
  = Domain String
  | IPv4 Nat Nat Nat Nat
  | IPv6 String

public export
Show Host where
  show (Domain d) = d
  show (IPv4 a b c d) = show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
  show (IPv6 addr) = "[" ++ addr ++ "]"

public export
Eq Host where
  (Domain a) == (Domain b) = a == b
  (IPv4 a1 b1 c1 d1) == (IPv4 a2 b2 c2 d2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2
  (IPv6 a) == (IPv6 b) = a == b
  _ == _ = False

||| User info component
public export
record UserInfo where
  constructor MkUserInfo
  username : String
  password : Maybe String

public export
Show UserInfo where
  show ui = ui.username ++ maybe "" (\p => ":" ++ p) ui.password

||| Parsed URL structure
public export
record ParsedURL where
  constructor MkParsedURL
  scheme : Maybe Scheme
  userInfo : Maybe UserInfo
  host : Maybe Host
  port : Maybe Nat
  path : List String
  query : List (String, String)
  fragment : Maybe String

--------------------------------------------------------------------------------
-- Parse Error
--------------------------------------------------------------------------------

||| URL parse errors
public export
data URLParseError
  = EmptyInput
  | InvalidScheme String
  | InvalidHost String
  | InvalidPort String
  | InvalidIPv4 String
  | InvalidIPv6 String
  | UnexpectedChar Nat Char
  | UnterminatedBracket

public export
Show URLParseError where
  show EmptyInput = "Empty input"
  show (InvalidScheme s) = "Invalid scheme: " ++ s
  show (InvalidHost h) = "Invalid host: " ++ h
  show (InvalidPort p) = "Invalid port: " ++ p
  show (InvalidIPv4 ip) = "Invalid IPv4 address: " ++ ip
  show (InvalidIPv6 ip) = "Invalid IPv6 address: " ++ ip
  show (UnexpectedChar pos c) = "Unexpected character '" ++ singleton c ++ "' at position " ++ show pos
  show UnterminatedBracket = "Unterminated bracket in IPv6 address"

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

||| Parse scheme from string
parseScheme : String -> Scheme
parseScheme s = case toLower s of
  "http" => HTTP
  "https" => HTTPS
  "ftp" => FTP
  "ftps" => FTPS
  "file" => File
  "mailto" => Mailto
  "tel" => Tel
  other => Custom other

||| Check if character is valid in scheme
isSchemeChar : Char -> Bool
isSchemeChar c = isAlphaNum c || c == '+' || c == '-' || c == '.'

||| Check if character is unreserved (RFC 3986)
isUnreserved : Char -> Bool
isUnreserved c = isAlphaNum c || c == '-' || c == '.' || c == '_' || c == '~'

||| Check if character is sub-delimiter (RFC 3986)
isSubDelim : Char -> Bool
isSubDelim c = c `elem` unpack "!$&'()*+,;="

||| Parse a natural number from digits
public export
parseNat : List Char -> Maybe Nat
parseNat [] = Nothing
parseNat digits =
  if all isDigit digits
    then Just (foldl (\n, c => n * 10 + cast (ord c - ord '0')) 0 digits)
    else Nothing

||| Split string on first occurrence of character
splitOn : Char -> String -> (String, String)
splitOn c s = go (unpack s) []
  where
    go : List Char -> List Char -> (String, String)
    go [] acc = (pack (reverse acc), "")
    go (x :: xs) acc =
      if x == c then (pack (reverse acc), pack xs)
                else go xs (x :: acc)

||| Split string on first occurrence, keeping delimiter in second part
splitOnKeep : Char -> String -> (String, String)
splitOnKeep c s = go (unpack s) []
  where
    go : List Char -> List Char -> (String, String)
    go [] acc = (pack (reverse acc), "")
    go (x :: xs) acc =
      if x == c then (pack (reverse acc), pack (x :: xs))
                else go xs (x :: acc)

urlStrLength : String -> Nat
urlStrLength s = length (unpack s)

public export
strDrop : Nat -> String -> String
strDrop n s = pack (drop n (unpack s))

strIsPrefix : String -> String -> Bool
strIsPrefix pre s = isPrefixOf (unpack pre) (unpack s)

public export
splitChar : Char -> String -> List String
splitChar _ "" = []
splitChar c s = go (unpack s) [] []
  where
    go : List Char -> List Char -> List String -> List String
    go [] current acc = reverse (pack (reverse current) :: acc)
    go (x :: xs) current acc =
      if x == c
        then go xs [] (pack (reverse current) :: acc)
        else go xs (x :: current) acc

parseUserInfo : String -> (Maybe UserInfo, String)
parseUserInfo str =
  case splitOn '@' str of
    (before, "") => (Nothing, str)
    (userPart, hostPart) =>
      let (user, pass) = splitOn ':' userPart
          password = if pass == "" then Nothing else Just pass
      in (Just (MkUserInfo user password), hostPart)

parseIPv4Addr : String -> Maybe (Nat, Nat, Nat, Nat)
parseIPv4Addr str =
  case splitChar '.' str of
    [a, b, c, d] => do
      na <- parseNat (unpack a)
      nb <- parseNat (unpack b)
      nc <- parseNat (unpack c)
      nd <- parseNat (unpack d)
      if na <= 255 && nb <= 255 && nc <= 255 && nd <= 255
        then Just (na, nb, nc, nd)
        else Nothing
    _ => Nothing

parseHost : String -> Maybe Host
parseHost "" = Nothing
parseHost str =
  case parseIPv4Addr str of
    Just (a, b, c, d) => Just (IPv4 a b c d)
    Nothing => Just (Domain str)

parsePort : String -> Maybe Nat
parsePort "" = Nothing
parsePort str = do
  p <- parseNat (unpack str)
  if p <= 65535 then Just p else Nothing

parseBracketedIPv6 : String -> (Maybe Host, Maybe Nat)
parseBracketedIPv6 str =
  case break (== ']') (unpack str) of
    (addr, ']' :: rest) =>
      let host = IPv6 (pack (drop 1 addr))
          port = case rest of
            ':' :: digits => parsePort (pack digits)
            _ => Nothing
      in (Just host, port)
    _ => (Nothing, Nothing)

public export
parseIPv6 : String -> (Maybe Host, Maybe Nat)
parseIPv6 = parseBracketedIPv6

parseHostPort : String -> (Maybe Host, Maybe Nat)
parseHostPort "" = (Nothing, Nothing)
parseHostPort str =
  if strIsPrefix "[" str
    then parseBracketedIPv6 str
    else let (hostStr, portStr) = splitOn ':' str
         in (parseHost hostStr, parsePort portStr)

--------------------------------------------------------------------------------
-- URL Parsing
--------------------------------------------------------------------------------

||| Parse scheme from URL start
extractScheme : String -> (Maybe Scheme, String)
extractScheme s =
  let (before, after) = splitOn ':' s
  in if urlStrLength after >= 2 && strIsPrefix "//" after
       then (Just (parseScheme before), strDrop 2 after)
       else if urlStrLength after > 0
         then (Just (parseScheme before), after)
         else (Nothing, s)

||| Parse authority (userinfo@host:port)
extractAuthority : String -> (Maybe UserInfo, Maybe Host, Maybe Nat, String)
extractAuthority s =
  let (auth, rest) = splitOnKeep '/' s
      (auth', rest') = splitOnKeep '?' auth
      (auth'', rest'') = splitOnKeep '#' auth'
      authStr = if strIsPrefix "/" auth then "" else auth''
      (ui, hostPort) = parseUserInfo authStr
      (h, p) = parseHostPort hostPort
  in (ui, h, p, rest ++ rest' ++ rest'')

||| Parse path from URL
extractPath : String -> (List String, String)
extractPath s =
  let (pathPart, rest) = splitOnKeep '?' s
      (pathPart', rest') = splitOnKeep '#' pathPart
      path = if strIsPrefix "/" pathPart' then strDrop 1 pathPart' else pathPart'
      segments = splitChar '/' path
  in (filter (not . (== "")) segments, rest ++ rest')

||| Parse query string
extractQuery : String -> (List (String, String), String)
extractQuery s =
  if strIsPrefix "?" s
    then let (queryPart, rest) = splitOnKeep '#' (strDrop 1 s)
         in (parseQueryParams queryPart, rest)
    else ([], s)
  where
    parsePair : String -> (String, String)
    parsePair str =
      let (key, val) = splitOn '=' str
      in (key, val)

    parseQueryParams : String -> List (String, String)
    parseQueryParams "" = []
    parseQueryParams str =
      let pairs = splitChar '&' str
      in map parsePair pairs

||| Parse fragment
extractFragment : String -> Maybe String
extractFragment s =
  if strIsPrefix "#" s
    then Just (strDrop 1 s)
    else Nothing

||| Parse a URL string into components
public export
parseURL : String -> Either URLParseError ParsedURL
parseURL "" = Left EmptyInput
parseURL s =
  let (scheme, rest1) = extractScheme s
      (userInfo, host, port, rest2) = extractAuthority rest1
      (path, rest3) = extractPath rest2
      (query, rest4) = extractQuery rest3
      fragment = extractFragment rest4
  in Right (MkParsedURL scheme userInfo host port path query fragment)

||| Parse URL returning Maybe
public export
parseURLMaybe : String -> Maybe ParsedURL
parseURLMaybe s = case parseURL s of
  Right url => Just url
  Left _ => Nothing

||| Check if string is a valid URL
public export
isValidURL : String -> Bool
isValidURL s = isJust (parseURLMaybe s)

--------------------------------------------------------------------------------
-- URL Encoding/Decoding
--------------------------------------------------------------------------------

||| Percent-encode a character
public export
percentEncode : Char -> String
percentEncode c =
  if isUnreserved c
    then singleton c
    else "%" ++ toHex (ord c)
  where
    hexDigit : Int -> Char
    hexDigit d = if d < 10 then chr (ord '0' + d) else chr (ord 'A' + d - 10)

    toHex : Int -> String
    toHex n =
      let hi = n `div` 16
          lo = n `mod` 16
      in pack [hexDigit hi, hexDigit lo]

||| URL-encode a string
public export
urlEncode : String -> String
urlEncode s = concat (map percentEncode (unpack s))

||| Decode a percent-encoded character
decodePercent : Char -> Char -> Maybe Char
decodePercent h1 h2 = do
  v1 <- hexVal h1
  v2 <- hexVal h2
  Just (chr (v1 * 16 + v2))
  where
    hexVal : Char -> Maybe Int
    hexVal c =
      if c >= '0' && c <= '9' then Just (ord c - ord '0')
      else if c >= 'a' && c <= 'f' then Just (ord c - ord 'a' + 10)
      else if c >= 'A' && c <= 'F' then Just (ord c - ord 'A' + 10)
      else Nothing

||| URL-decode a string
public export
urlDecode : String -> Maybe String
urlDecode s = map pack (go (unpack s))
  where
    go : List Char -> Maybe (List Char)
    go [] = Just []
    go ('%' :: h1 :: h2 :: rest) = do
      c <- decodePercent h1 h2
      rest' <- go rest
      Just (c :: rest')
    go ('+' :: rest) = do  -- Handle + as space in query strings
      rest' <- go rest
      Just (' ' :: rest')
    go (c :: rest) = do
      rest' <- go rest
      Just (c :: rest')

||| URL-decode with fallback (returns original on error)
public export
urlDecodeOr : String -> String
urlDecodeOr s = case urlDecode s of
  Just decoded => decoded
  Nothing => s
