{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe retry/backoff operations via libproven FFI.
--
-- Delay calculation with exponential backoff is performed by the
-- Idris 2 verified core.
module Proven.SafeRetry
  ( RetryConfig(..)
  , retryDelay
  , shouldRetry
  ) where

import Foreign
import Data.Word (Word32, Word64)
import Proven.FFI (c_proven_retry_delay, c_proven_retry_should_retry)
import qualified Proven.FFI.Types as T

-- | Retry configuration.
data RetryConfig = RetryConfig
  { retryMaxAttempts :: !Word32   -- ^ Maximum number of attempts
  , retryBaseDelay   :: !Word64   -- ^ Base delay in milliseconds
  , retryMaxDelay    :: !Word64   -- ^ Maximum delay in milliseconds
  , retryMultiplier  :: !Double   -- ^ Multiplier for exponential backoff
  } deriving (Eq, Show)

-- | Calculate the delay for a given retry attempt (with jitter).
-- Delegates to @proven_retry_delay@ in libproven.
retryDelay :: RetryConfig -> Word32 -> IO Word64
retryDelay cfg attempt =
  alloca $ \ptr -> do
    poke ptr (T.RetryConfig
      (retryMaxAttempts cfg)
      (retryBaseDelay cfg)
      (retryMaxDelay cfg)
      (realToFrac (retryMultiplier cfg)))
    c_proven_retry_delay ptr attempt

-- | Check if a retry should be attempted.
-- Delegates to @proven_retry_should_retry@ in libproven.
shouldRetry :: RetryConfig -> Word32 -> IO Bool
shouldRetry cfg attempt =
  alloca $ \ptr -> do
    poke ptr (T.RetryConfig
      (retryMaxAttempts cfg)
      (retryBaseDelay cfg)
      (retryMaxDelay cfg)
      (realToFrac (retryMultiplier cfg)))
    result <- c_proven_retry_should_retry ptr attempt
    return (result /= 0)
