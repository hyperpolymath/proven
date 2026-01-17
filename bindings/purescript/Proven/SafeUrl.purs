-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe URL validation and parsing.
-- |
-- | Validates and parses URLs with protection against malformed URLs
-- | and potential security issues.

module Proven.SafeUrl
  ( SafeUrl
  , Url(..)
  , isValidUrl
  , parseUrl
  , requireValidUrl
  , getProtocol
  , getHost
  , getPort
  , getPath
  , getQuery
  , getFragment
  , isHttps
  , isHttp
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), indexOf, take, drop, toLower, trim)
import Proven.Result (Result(..), ProvenError(..))

-- | SafeUrl namespace marker (not instantiated).
data SafeUrl

-- | Parsed URL with validated components.
newtype Url = Url
  { protocol :: String
  , host :: String
  , port :: Maybe Int
  , path :: String
  , query :: Maybe String
  , fragment :: Maybe String
  }

derive instance eqUrl :: Eq Url

instance showUrl :: Show Url where
  show (Url u) =
    u.protocol <> "://" <> u.host <>
    (case u.port of
      Just p -> ":" <> show p
      Nothing -> "") <>
    u.path <>
    (case u.query of
      Just q -> "?" <> q
      Nothing -> "") <>
    (case u.fragment of
      Just f -> "#" <> f
      Nothing -> "")

-- | Check if a string is a valid URL.
isValidUrl :: String -> Boolean
isValidUrl url = isValidUrlImpl (trim url)

foreign import isValidUrlImpl :: String -> Boolean

-- | Parse a URL string into its components.
parseUrl :: String -> Result Url ProvenError
parseUrl url =
  let parsed = parseUrlImpl (trim url)
  in if parsed.valid
     then Ok (Url
       { protocol: parsed.protocol
       , host: parsed.host
       , port: if parsed.port > 0 then Just parsed.port else Nothing
       , path: parsed.path
       , query: if parsed.query == "" then Nothing else Just parsed.query
       , fragment: if parsed.fragment == "" then Nothing else Just parsed.fragment
       })
     else Err InvalidUrl

foreign import parseUrlImpl :: String ->
  { valid :: Boolean
  , protocol :: String
  , host :: String
  , port :: Int
  , path :: String
  , query :: String
  , fragment :: String
  }

-- | Require a valid URL or return error.
requireValidUrl :: String -> Result String ProvenError
requireValidUrl url
  | isValidUrl url = Ok (trim url)
  | otherwise = Err InvalidUrl

-- | Get the protocol (scheme) of a URL.
getProtocol :: Url -> String
getProtocol (Url u) = u.protocol

-- | Get the host of a URL.
getHost :: Url -> String
getHost (Url u) = u.host

-- | Get the port of a URL if specified.
getPort :: Url -> Maybe Int
getPort (Url u) = u.port

-- | Get the path of a URL.
getPath :: Url -> String
getPath (Url u) = u.path

-- | Get the query string of a URL if present.
getQuery :: Url -> Maybe String
getQuery (Url u) = u.query

-- | Get the fragment of a URL if present.
getFragment :: Url -> Maybe String
getFragment (Url u) = u.fragment

-- | Check if URL uses HTTPS protocol.
isHttps :: Url -> Boolean
isHttps (Url u) = toLower u.protocol == "https"

-- | Check if URL uses HTTP protocol.
isHttp :: Url -> Boolean
isHttp (Url u) = toLower u.protocol == "http"
