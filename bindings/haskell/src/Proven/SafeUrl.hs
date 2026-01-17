{- SPDX-License-Identifier: PMPL-1.0 -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe URL operations with validation and sanitization.
--
-- Provides secure URL parsing, validation, and manipulation
-- without panicking on malformed input.
module Proven.SafeUrl
  ( -- * Types
    Url(..)
  , UrlScheme(..)
    -- * Parsing
  , parseUrl
  , parseUrlStrict
    -- * Validation
  , isValidUrl
  , isHttps
  , isHttp
  , isSecure
    -- * Components
  , getScheme
  , getHost
  , getPort
  , getPath
  , getQuery
  , getFragment
    -- * Manipulation
  , withScheme
  , withHost
  , withPort
  , withPath
  , withQuery
  , withFragment
    -- * Rendering
  , renderUrl
    -- * Security
  , normalizeUrl
  , stripCredentials
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlphaNum, toLower)
import Proven.Core (ProvenError(..), Result)

-- | URL scheme types.
data UrlScheme
  = Http
  | Https
  | Ftp
  | Ftps
  | File
  | Mailto
  | OtherScheme !Text
  deriving (Eq, Show)

-- | Parsed URL structure.
data Url = Url
  { urlScheme   :: !UrlScheme
  , urlHost     :: !Text
  , urlPort     :: !(Maybe Int)
  , urlPath     :: !Text
  , urlQuery    :: !(Maybe Text)
  , urlFragment :: !(Maybe Text)
  , urlUsername :: !(Maybe Text)
  , urlPassword :: !(Maybe Text)
  } deriving (Eq, Show)

-- | Parse a URL string.
parseUrl :: Text -> Result Url
parseUrl input
  | T.null input = Left (InvalidInput "Empty URL")
  | T.length input > 8192 = Left (TooLong "URL too long")
  | otherwise = parseUrlInternal input

-- | Parse URL with strict validation (no credentials allowed).
parseUrlStrict :: Text -> Result Url
parseUrlStrict input = do
  url <- parseUrl input
  case (urlUsername url, urlPassword url) of
    (Nothing, Nothing) -> Right url
    _ -> Left (InvalidFormat "Credentials not allowed in strict mode")

-- Internal URL parser
parseUrlInternal :: Text -> Result Url
parseUrlInternal input = do
  let trimmed = T.strip input
  -- Extract scheme
  case T.breakOn "://" trimmed of
    (scheme, rest)
      | T.null rest -> Left (InvalidFormat "Missing scheme separator")
      | otherwise -> do
          let schemeType = parseScheme (T.toLower scheme)
          let afterScheme = T.drop 3 rest
          parseAfterScheme schemeType afterScheme

parseScheme :: Text -> UrlScheme
parseScheme "http" = Http
parseScheme "https" = Https
parseScheme "ftp" = Ftp
parseScheme "ftps" = Ftps
parseScheme "file" = File
parseScheme "mailto" = Mailto
parseScheme other = OtherScheme other

parseAfterScheme :: UrlScheme -> Text -> Result Url
parseAfterScheme scheme rest = do
  -- Split off fragment first
  let (beforeFragment, fragment) = case T.breakOn "#" rest of
        (b, f) | T.null f -> (b, Nothing)
               | otherwise -> (b, Just (T.drop 1 f))
  -- Split off query
  let (beforeQuery, query) = case T.breakOn "?" beforeFragment of
        (b, q) | T.null q -> (b, Nothing)
               | otherwise -> (b, Just (T.drop 1 q))
  -- Split host/path
  let (hostPart, pathPart) = case T.breakOn "/" beforeQuery of
        (h, p) | T.null p -> (h, "/")
               | otherwise -> (h, p)
  -- Parse host (possibly with port)
  let (host, port) = parseHostPort hostPart
  if T.null host
    then Left (InvalidFormat "Empty host")
    else Right Url
      { urlScheme = scheme
      , urlHost = host
      , urlPort = port
      , urlPath = pathPart
      , urlQuery = query
      , urlFragment = fragment
      , urlUsername = Nothing
      , urlPassword = Nothing
      }

parseHostPort :: Text -> (Text, Maybe Int)
parseHostPort hp =
  case T.breakOnEnd ":" hp of
    (_, port) | T.null port -> (hp, Nothing)
    (hostColon, port) ->
      case reads (T.unpack port) of
        [(p, "")] | p > 0 && p <= 65535 -> (T.dropEnd 1 hostColon, Just p)
        _ -> (hp, Nothing)

-- | Check if URL string is valid.
isValidUrl :: Text -> Bool
isValidUrl input = case parseUrl input of
  Right _ -> True
  Left _ -> False

-- | Check if URL uses HTTPS.
isHttps :: Url -> Bool
isHttps url = urlScheme url == Https

-- | Check if URL uses HTTP.
isHttp :: Url -> Bool
isHttp url = urlScheme url == Http

-- | Check if URL uses a secure scheme (HTTPS, FTPS).
isSecure :: Url -> Bool
isSecure url = urlScheme url `elem` [Https, Ftps]

-- | Get the scheme from a URL.
getScheme :: Url -> UrlScheme
getScheme = urlScheme

-- | Get the host from a URL.
getHost :: Url -> Text
getHost = urlHost

-- | Get the port from a URL.
getPort :: Url -> Maybe Int
getPort = urlPort

-- | Get the path from a URL.
getPath :: Url -> Text
getPath = urlPath

-- | Get the query string from a URL.
getQuery :: Url -> Maybe Text
getQuery = urlQuery

-- | Get the fragment from a URL.
getFragment :: Url -> Maybe Text
getFragment = urlFragment

-- | Set the scheme.
withScheme :: UrlScheme -> Url -> Url
withScheme s url = url { urlScheme = s }

-- | Set the host.
withHost :: Text -> Url -> Url
withHost h url = url { urlHost = h }

-- | Set the port.
withPort :: Maybe Int -> Url -> Url
withPort p url = url { urlPort = p }

-- | Set the path.
withPath :: Text -> Url -> Url
withPath p url = url { urlPath = p }

-- | Set the query.
withQuery :: Maybe Text -> Url -> Url
withQuery q url = url { urlQuery = q }

-- | Set the fragment.
withFragment :: Maybe Text -> Url -> Url
withFragment f url = url { urlFragment = f }

-- | Render URL to string.
renderUrl :: Url -> Text
renderUrl url =
  schemeText <> "://" <> urlHost url <> portText <> urlPath url <> queryText <> fragText
  where
    schemeText = case urlScheme url of
      Http -> "http"
      Https -> "https"
      Ftp -> "ftp"
      Ftps -> "ftps"
      File -> "file"
      Mailto -> "mailto"
      OtherScheme s -> s
    portText = case urlPort url of
      Nothing -> ""
      Just p -> ":" <> T.pack (show p)
    queryText = case urlQuery url of
      Nothing -> ""
      Just q -> "?" <> q
    fragText = case urlFragment url of
      Nothing -> ""
      Just f -> "#" <> f

-- | Normalize URL (lowercase scheme/host, remove default ports).
normalizeUrl :: Url -> Url
normalizeUrl url = url
  { urlHost = T.toLower (urlHost url)
  , urlPort = case (urlScheme url, urlPort url) of
      (Http, Just 80) -> Nothing
      (Https, Just 443) -> Nothing
      (Ftp, Just 21) -> Nothing
      _ -> urlPort url
  }

-- | Strip credentials from URL.
stripCredentials :: Url -> Url
stripCredentials url = url
  { urlUsername = Nothing
  , urlPassword = Nothing
  }
