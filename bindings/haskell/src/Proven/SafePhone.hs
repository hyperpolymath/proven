{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe phone number operations via libproven FFI.
--
-- Phone parsing and E.164 formatting are performed by the Idris 2 verified core.
module Proven.SafePhone
  ( PhoneNumber(..)
  , parsePhone
  , formatE164
  ) where

import Data.Word (Word16, Word64)
import Proven.FFI (c_proven_phone_parse, c_proven_phone_format_e164,
                   c_proven_free_string)
import Proven.FFI.Types (PhoneResult(..))
import Proven.Core (withCStringLen', stringResultToMaybe)

-- | A parsed phone number.
data PhoneNumber = PhoneNumber
  { pnCountryCode    :: !Word16  -- ^ ITU country calling code
  , pnNationalNumber :: !Word64  -- ^ National number
  } deriving (Eq, Show)

-- | Parse a phone number string.
-- Delegates to @proven_phone_parse@ in libproven.
parsePhone :: String -> IO (Maybe PhoneNumber)
parsePhone str = withCStringLen' str $ \ptr len -> do
  result <- c_proven_phone_parse ptr len
  if phoneStatusRaw result == 0 && phoneIsValid result /= 0
    then return (Just (PhoneNumber
           (phoneCountryCodeRaw result)
           (phoneNationalNumber result)))
    else return Nothing

-- | Format a phone number as E.164.
-- Delegates to @proven_phone_format_e164@ in libproven.
formatE164 :: Word16 -> Word64 -> IO (Maybe String)
formatE164 cc nn = do
  sr <- c_proven_phone_format_e164 cc nn
  stringResultToMaybe c_proven_free_string sr
