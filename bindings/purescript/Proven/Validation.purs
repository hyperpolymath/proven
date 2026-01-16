-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Validation utilities

module Proven.Validation
  ( Validation
  , Validator
  , isValidPort
  , isValidPercentage
  , isValidEmail
  , isValidUrl
  , isSafePath
  , requireValidPort
  , requireValidPercentage
  , requireValidEmail
  , requireValidUrl
  , requireSafePath
  , validateAll
  , validateFirst
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))
import Data.Array (all, findMap)
import Data.Maybe (Maybe(..), isJust)
import Data.String (contains, length)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)

-- | Validation type class (not instantiated)
data Validation

-- | Validator function type
type Validator a = a -> Maybe ProvenError

-- | Email regex pattern
emailRegex :: Regex
emailRegex = unsafeRegex "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" noFlags

-- | URL regex pattern (simplified)
urlRegex :: Regex
urlRegex = unsafeRegex "^https?://[^\\s/$.?#].[^\\s]*$" noFlags

-- | Validate port number (1-65535)
isValidPort :: Int -> Boolean
isValidPort port = port >= 1 && port <= 65535

-- | Require valid port or return error
requireValidPort :: Int -> Result Int ProvenError
requireValidPort port
  | isValidPort port = Ok port
  | otherwise = Err InvalidPort

-- | Validate percentage (0-100)
isValidPercentage :: Number -> Boolean
isValidPercentage value = value >= 0.0 && value <= 100.0

-- | Require valid percentage or return error
requireValidPercentage :: Number -> Result Number ProvenError
requireValidPercentage value
  | isValidPercentage value = Ok value
  | otherwise = Err InvalidPercentage

-- | Validate email format
isValidEmail :: String -> Boolean
isValidEmail email = test emailRegex email

-- | Require valid email or return error
requireValidEmail :: String -> Result String ProvenError
requireValidEmail email
  | isValidEmail email = Ok email
  | otherwise = Err InvalidEmail

-- | Validate URL format
isValidUrl :: String -> Boolean
isValidUrl url = test urlRegex url

-- | Require valid URL or return error
requireValidUrl :: String -> Result String ProvenError
requireValidUrl url
  | isValidUrl url = Ok url
  | otherwise = Err InvalidUrl

-- | Check if path is safe (no traversal)
isSafePath :: String -> Boolean
isSafePath path = not (contains (Pattern "..") path)

-- | Require safe path or return error
requireSafePath :: String -> Result String ProvenError
requireSafePath path
  | isSafePath path = Ok path
  | otherwise = Err PathTraversal

-- | Run all validators and collect errors
validateAll :: forall a. Array (Validator a) -> a -> Array ProvenError
validateAll validators value = do
  validator <- validators
  case validator value of
    Just err -> [err]
    Nothing -> []

-- | Run validators until first error
validateFirst :: forall a. Array (Validator a) -> a -> Maybe ProvenError
validateFirst validators value = findMap (\v -> v value) validators

-- | Common validators

-- | Required string validator (non-empty)
required :: Validator String
required s
  | length s == 0 = Just (Custom "Required")
  | otherwise = Nothing

-- | Minimum length validator
minLength :: Int -> Validator String
minLength min s
  | length s < min = Just (Custom $ "Minimum " <> show min <> " characters")
  | otherwise = Nothing

-- | Maximum length validator
maxLength :: Int -> Validator String
maxLength max s
  | length s > max = Just (Custom $ "Maximum " <> show max <> " characters")
  | otherwise = Nothing

-- | Port validator
portValidator :: Validator Int
portValidator port
  | isValidPort port = Nothing
  | otherwise = Just InvalidPort

-- | Percentage validator
percentageValidator :: Validator Number
percentageValidator value
  | isValidPercentage value = Nothing
  | otherwise = Just InvalidPercentage

-- | Email validator
emailValidator :: Validator String
emailValidator email
  | isValidEmail email = Nothing
  | otherwise = Just InvalidEmail

-- | Safe path validator
safePathValidator :: Validator String
safePathValidator path
  | isSafePath path = Nothing
  | otherwise = Just PathTraversal
