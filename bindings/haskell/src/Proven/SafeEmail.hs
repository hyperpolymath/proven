{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe email validation via libproven FFI.
--
-- Email validation is performed by the Idris 2 verified core.
module Proven.SafeEmail
  ( isValid
  ) where

import Proven.FFI (c_proven_email_is_valid)
import Proven.Core (withCStringLen', boolResultToBool)

-- | Check if an email address is valid (RFC 5321 simplified).
-- Delegates to @proven_email_is_valid@ in libproven.
isValid :: String -> IO (Maybe Bool)
isValid email = withCStringLen' email $ \ptr len ->
  boolResultToBool <$> c_proven_email_is_valid ptr len
