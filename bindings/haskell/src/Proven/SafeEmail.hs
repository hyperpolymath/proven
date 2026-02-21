{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe email validation and parsing operations.
module Proven.SafeEmail
  ( EmailParts(..)
  , isValid
  , splitEmail
  , getDomain
  , getLocalPart
  , normalize
  ) where

import Data.Char (toLower)
import Data.List (isPrefixOf, isSuffixOf)

-- | Represents the parts of an email address.
data EmailParts = EmailParts
  { localPart :: String
  , domain :: String
  } deriving (Show, Eq)

-- | Check if an email address is valid (basic check).
isValid :: String -> Bool
isValid email = case break (== '@') email of
  (local, '@':dom) ->
    not (null local) &&
    length dom >= 3 &&
    '.' `elem` dom &&
    not ("." `isPrefixOf` dom) &&
    not ("." `isSuffixOf` dom)
  _ -> False

-- | Split an email into local part and domain.
splitEmail :: String -> Maybe EmailParts
splitEmail email
  | not (isValid email) = Nothing
  | otherwise = case break (== '@') email of
      (local, '@':dom) -> Just EmailParts { localPart = local, domain = dom }
      _ -> Nothing

-- | Extract the domain from an email address.
getDomain :: String -> Maybe String
getDomain email = domain <$> splitEmail email

-- | Extract the local part from an email address.
getLocalPart :: String -> Maybe String
getLocalPart email = localPart <$> splitEmail email

-- | Normalize an email address (lowercase domain).
normalize :: String -> Maybe String
normalize email = do
  parts <- splitEmail email
  return $ localPart parts ++ "@" ++ map toLower (domain parts)
