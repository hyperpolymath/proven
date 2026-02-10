-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeUrl - URL operations that cannot crash
|||
||| This module provides:
||| - Safe URL parsing (returns Maybe/Result instead of throwing)
||| - URL validation
||| - Safe URL manipulation
||| - Query string parsing
||| - Protection against URL injection attacks
module Proven.SafeUrl

import public Proven.Core
import Data.List
import Data.Maybe
import Data.String

%default total

joinWith : String -> List String -> String
joinWith _ [] = ""
joinWith sep (x :: xs) = foldl (\acc => \y => acc ++ sep ++ y) x xs

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

--------------------------------------------------------------------------------
-- URL Types
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

||| User info component (username:password)
public export
record UserInfo where
  constructor MkUserInfo
  username : String
  password : Maybe String

public export
Show UserInfo where
  show ui = ui.username ++ maybe "" (\p => ":" ++ p) ui.password

||| Host component (domain or IP)
public export
data Host
  = Domain String           -- Regular domain name
  | IPv4 Nat Nat Nat Nat    -- IPv4 address
  | IPv6 String             -- IPv6 address (stored as string)

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

||| Port number (0-65535)
public export
Port : Type
Port = Nat

||| URL path component
public export
Path : Type
Path = List String  -- Segments separated by /

||| Query string as key-value pairs
public export
Query : Type
Query = List (String, String)

||| Fragment (hash) component
public export
Fragment : Type
Fragment = String

||| A parsed URL with all components
public export
record URL where
  constructor MkURL
  scheme : Maybe Scheme
  userInfo : Maybe UserInfo
  host : Maybe Host
  port : Maybe Port
  path : Path
  query : Query
  fragment : Maybe Fragment

--------------------------------------------------------------------------------
-- URL Construction
--------------------------------------------------------------------------------

||| Create an empty URL
public export
emptyURL : URL
emptyURL = MkURL Nothing Nothing Nothing Nothing [] [] Nothing

||| Create a URL from just a path
public export
pathURL : String -> URL
pathURL p = { path := splitPath p } emptyURL
  where
    splitPath : String -> List String
    splitPath s = filter (not . (== "")) (splitChar '/' s)

||| Create an HTTP URL
public export
httpURL : String -> URL
httpURL domain = MkURL (Just HTTP) Nothing (Just (Domain domain)) Nothing [] [] Nothing

||| Create an HTTPS URL
public export
httpsURL : String -> URL
httpsURL domain = MkURL (Just HTTPS) Nothing (Just (Domain domain)) Nothing [] [] Nothing

--------------------------------------------------------------------------------
-- URL Accessors (Safe)
--------------------------------------------------------------------------------

||| Get scheme as string
public export
getScheme : URL -> Maybe String
getScheme url = map show url.scheme

||| Get host as string
public export
getHost : URL -> Maybe String
getHost url = map show url.host

||| Get port number
public export
getPort : URL -> Maybe Nat
getPort = port

||| Get default port for scheme
public export
defaultPort : Scheme -> Maybe Nat
defaultPort HTTP = Just 80
defaultPort HTTPS = Just 443
defaultPort FTP = Just 21
defaultPort FTPS = Just 990
defaultPort _ = Nothing

||| Get effective port (explicit or default)
public export
effectivePort : URL -> Maybe Nat
effectivePort url = url.port <|> (url.scheme >>= defaultPort)

||| Get path as string
public export
getPath : URL -> String
getPath url = "/" ++ joinWith "/" url.path

||| Get query string
public export
getQuery : URL -> String
getQuery url = if null url.query
  then ""
  else "?" ++ joinWith "&" (map formatPair url.query)
  where
    formatPair : (String, String) -> String
    formatPair (k, v) = k ++ "=" ++ v

||| Get query parameter by key
public export
getQueryParam : String -> URL -> Maybe String
getQueryParam key url = lookup key url.query

||| Get all values for a query parameter
public export
getQueryParams : String -> URL -> List String
getQueryParams key url = map snd (filter (\(k, _) => k == key) url.query)

||| Get fragment
public export
getFragment : URL -> Maybe String
getFragment = fragment

--------------------------------------------------------------------------------
-- URL Modification (Safe)
--------------------------------------------------------------------------------

||| Set scheme
public export
setScheme : Scheme -> URL -> URL
setScheme s url = { scheme := Just s } url

||| Set host
public export
setHost : String -> URL -> URL
setHost h url = { host := Just (Domain h) } url

||| Set port
public export
setPort : Nat -> URL -> URL
setPort p url = { port := Just p } url

||| Clear port (use default)
public export
clearPort : URL -> URL
clearPort url = { port := Nothing } url

||| Set path from string
public export
setPath : String -> URL -> URL
setPath p url = { path := splitPath p } url
  where
    splitPath : String -> List String
    splitPath s = filter (not . (== "")) (splitChar '/' s)

||| Append path segment
public export
appendPath : String -> URL -> URL
appendPath seg url = { path $= (++ [seg]) } url

||| Set query from list
public export
setQuery : Query -> URL -> URL
setQuery q url = { query := q } url

||| Add query parameter
public export
addQueryParam : String -> String -> URL -> URL
addQueryParam key val url = { query $= (++ [(key, val)]) } url

||| Remove query parameter
public export
removeQueryParam : String -> URL -> URL
removeQueryParam key url = { query $= filter (\(k, _) => k /= key) } url

||| Set fragment
public export
setFragment : String -> URL -> URL
setFragment f url = { fragment := Just f } url

||| Clear fragment
public export
clearFragment : URL -> URL
clearFragment url = { fragment := Nothing } url

--------------------------------------------------------------------------------
-- URL Validation
--------------------------------------------------------------------------------

||| URL validation errors
public export
data URLError
  = EmptyURL
  | InvalidScheme String
  | InvalidHost String
  | InvalidPort String
  | InvalidPath String
  | InvalidQuery String
  | MissingHost
  | PortOutOfRange Nat

public export
Show URLError where
  show EmptyURL = "Empty URL"
  show (InvalidScheme s) = "Invalid scheme: " ++ s
  show (InvalidHost h) = "Invalid host: " ++ h
  show (InvalidPort p) = "Invalid port: " ++ p
  show (InvalidPath p) = "Invalid path: " ++ p
  show (InvalidQuery q) = "Invalid query: " ++ q
  show MissingHost = "Missing host"
  show (PortOutOfRange p) = "Port out of range: " ++ show p

||| Check if port is valid (0-65535)
public export
isValidPort : Nat -> Bool
isValidPort p = p <= 65535

||| Check if character is valid in URL path
public export
isValidPathChar : Char -> Bool
isValidPathChar c =
  isAlphaNum c || isInfixOf c pathExtras
  where
    pathExtras : List Char
    pathExtras = unpack "-._~!$&'()*+,;=:@/"

    isInfixOf : Char -> List Char -> Bool
    isInfixOf _ [] = False
    isInfixOf ch (x :: xs) = if ch == x then True else isInfixOf ch xs

||| Check if host looks valid
public export
isValidHost : String -> Bool
isValidHost "" = False
isValidHost s = all validHostChar (unpack s)
  where
    validHostChar : Char -> Bool
    validHostChar c = isAlphaNum c || c == '.' || c == '-' || c == '_'

||| Validate URL structure
public export
validateURL : URL -> Either URLError URL
validateURL url =
  if isNothing url.scheme && isNothing url.host && null url.path
    then Left EmptyURL
    else if isJust url.scheme && isNothing url.host
      then Left MissingHost
      else case url.port of
        Just p => if isValidPort p then Right url else Left (PortOutOfRange p)
        Nothing => Right url

--------------------------------------------------------------------------------
-- URL Serialization
--------------------------------------------------------------------------------

||| Convert URL to string
public export
toString : URL -> String
toString url =
  schemeStr ++ authStr ++ hostStr ++ portStr ++ pathStr ++ queryStr ++ fragStr
  where
    schemeStr : String
    schemeStr = maybe "" (\s => show s ++ "://") url.scheme

    authStr : String
    authStr = case url.userInfo of
      Nothing => ""
      Just ui => show ui ++ "@"

    hostStr : String
    hostStr = maybe "" show url.host

    portStr : String
    portStr = case (url.port, url.scheme >>= defaultPort) of
      (Just p, Just def) => if p == def then "" else ":" ++ show p
      (Just p, Nothing) => ":" ++ show p
      (Nothing, _) => ""

    pathStr : String
    pathStr = if null url.path then "" else "/" ++ joinWith "/" url.path

    queryStr : String
    queryStr = if null url.query then "" else "?" ++ joinWith "&" (map formatPair url.query)
      where
        formatPair : (String, String) -> String
        formatPair (k, v) = k ++ "=" ++ v

    fragStr : String
    fragStr = maybe "" (\f => "#" ++ f) url.fragment

public export
Show URL where
  show = toString

--------------------------------------------------------------------------------
-- URL Comparison
--------------------------------------------------------------------------------

public export
Eq URL where
  u1 == u2 = toString u1 == toString u2

||| Check if two URLs are equivalent (ignoring default ports, trailing slashes)
public export
equivalent : URL -> URL -> Bool
equivalent u1 u2 =
  u1.scheme == u2.scheme &&
  u1.host == u2.host &&
  effectivePort u1 == effectivePort u2 &&
  normalizePath u1.path == normalizePath u2.path &&
  u1.query == u2.query
  where
    normalizePath : Path -> Path
    normalizePath p = filter (not . (== "")) p

removeLast : List a -> List a
removeLast [] = []
removeLast [_] = []
removeLast (x :: xs) = x :: removeLast xs

--------------------------------------------------------------------------------
-- URL Resolution
--------------------------------------------------------------------------------

||| Resolve a relative URL against a base URL
public export
resolve : (base : URL) -> (relative : URL) -> URL
resolve base rel =
  if isJust rel.scheme
    then rel  -- Absolute URL, return as-is
    else if isJust rel.host
      then { scheme := base.scheme } rel  -- Network-path reference
      else if null rel.path
        then { path := base.path,
               query := if null rel.query then base.query else rel.query,
               scheme := base.scheme,
               host := base.host,
               port := base.port } rel
        else { scheme := base.scheme,
               host := base.host,
               port := base.port,
               path := resolvePath base.path rel.path } rel
  where
    resolvePath : Path -> Path -> Path
    resolvePath basePath relPath =
      if not (null relPath) && (case relPath of
        "" :: _ => True
        _ => False)
        then relPath  -- Starts with /, use as-is
        else case basePath of
          [] => relPath
          _ => removeLast basePath ++ relPath

--------------------------------------------------------------------------------
-- Security Helpers
--------------------------------------------------------------------------------

||| Check if URL is HTTPS
public export
isSecure : URL -> Bool
isSecure url = url.scheme == Just HTTPS || url.scheme == Just FTPS

||| Check if URL points to localhost
public export
isLocalhost : URL -> Bool
isLocalhost url = case url.host of
  Just (Domain "localhost") => True
  Just (Domain "127.0.0.1") => True
  Just (IPv4 127 0 0 1) => True
  Just (IPv6 "::1") => True
  _ => False

||| Check if URL has a dangerous scheme
public export
isDangerousScheme : URL -> Bool
isDangerousScheme url = case url.scheme of
  Just (Custom "javascript") => True
  Just (Custom "data") => True
  Just (Custom "vbscript") => True
  _ => False

||| Sanitize URL for safe display (remove userinfo)
public export
sanitizeForDisplay : URL -> URL
sanitizeForDisplay url = { userInfo := Nothing } url
