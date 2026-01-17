{- SPDX-License-Identifier: PMPL-1.0 -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Core types and error handling for Proven library.
--
-- Provides the fundamental error types used across all safe modules.
module Proven.Core
  ( -- * Error Types
    ProvenError(..)
  , Result
    -- * Error Constructors
  , overflow
  , underflow
  , divisionByZero
  , outOfRange
  , invalidInput
  , invalidFormat
  , tooLong
  , tooShort
  , notFound
  , ioError'
    -- * Error Inspection
  , isOverflow
  , isUnderflow
  , isDivisionByZero
  , isOutOfRange
  , isInvalidInput
  , isInvalidFormat
  , isTooLong
  , isTooShort
  , isNotFound
  , isIOError
    -- * Conversion
  , toMaybe
  , fromMaybe'
  ) where

import Data.Text (Text)

-- | Error types for safe operations.
data ProvenError
  = Overflow !Text      -- ^ Arithmetic overflow
  | Underflow !Text     -- ^ Arithmetic underflow
  | DivisionByZero      -- ^ Division by zero attempted
  | OutOfRange !Text    -- ^ Value out of valid range
  | InvalidInput !Text  -- ^ Invalid input provided
  | InvalidFormat !Text -- ^ Invalid format
  | TooLong !Text       -- ^ Input too long
  | TooShort !Text      -- ^ Input too short
  | NotFound !Text      -- ^ Resource not found
  | IOError' !Text      -- ^ IO operation failed
  deriving (Eq, Show)

-- | Result type alias for safe operations.
type Result a = Either ProvenError a

-- | Create an overflow error.
overflow :: Text -> ProvenError
overflow = Overflow

-- | Create an underflow error.
underflow :: Text -> ProvenError
underflow = Underflow

-- | Create a division by zero error.
divisionByZero :: ProvenError
divisionByZero = DivisionByZero

-- | Create an out of range error.
outOfRange :: Text -> ProvenError
outOfRange = OutOfRange

-- | Create an invalid input error.
invalidInput :: Text -> ProvenError
invalidInput = InvalidInput

-- | Create an invalid format error.
invalidFormat :: Text -> ProvenError
invalidFormat = InvalidFormat

-- | Create a too long error.
tooLong :: Text -> ProvenError
tooLong = TooLong

-- | Create a too short error.
tooShort :: Text -> ProvenError
tooShort = TooShort

-- | Create a not found error.
notFound :: Text -> ProvenError
notFound = NotFound

-- | Create an IO error.
ioError' :: Text -> ProvenError
ioError' = IOError'

-- | Check if error is overflow.
isOverflow :: ProvenError -> Bool
isOverflow (Overflow _) = True
isOverflow _ = False

-- | Check if error is underflow.
isUnderflow :: ProvenError -> Bool
isUnderflow (Underflow _) = True
isUnderflow _ = False

-- | Check if error is division by zero.
isDivisionByZero :: ProvenError -> Bool
isDivisionByZero DivisionByZero = True
isDivisionByZero _ = False

-- | Check if error is out of range.
isOutOfRange :: ProvenError -> Bool
isOutOfRange (OutOfRange _) = True
isOutOfRange _ = False

-- | Check if error is invalid input.
isInvalidInput :: ProvenError -> Bool
isInvalidInput (InvalidInput _) = True
isInvalidInput _ = False

-- | Check if error is invalid format.
isInvalidFormat :: ProvenError -> Bool
isInvalidFormat (InvalidFormat _) = True
isInvalidFormat _ = False

-- | Check if error is too long.
isTooLong :: ProvenError -> Bool
isTooLong (TooLong _) = True
isTooLong _ = False

-- | Check if error is too short.
isTooShort :: ProvenError -> Bool
isTooShort (TooShort _) = True
isTooShort _ = False

-- | Check if error is not found.
isNotFound :: ProvenError -> Bool
isNotFound (NotFound _) = True
isNotFound _ = False

-- | Check if error is IO error.
isIOError :: ProvenError -> Bool
isIOError (IOError' _) = True
isIOError _ = False

-- | Convert Result to Maybe, discarding error info.
toMaybe :: Result a -> Maybe a
toMaybe (Right a) = Just a
toMaybe (Left _) = Nothing

-- | Convert Maybe to Result with default error.
fromMaybe' :: ProvenError -> Maybe a -> Result a
fromMaybe' err Nothing = Left err
fromMaybe' _ (Just a) = Right a
