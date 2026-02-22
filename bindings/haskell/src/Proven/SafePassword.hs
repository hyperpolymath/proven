{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe password validation via libproven FFI.
--
-- Password strength assessment is performed by the Idris 2 verified core.
module Proven.SafePassword
  ( PasswordStrength(..)
  , PasswordInfo(..)
  , validatePassword
  , isCommonPassword
  ) where

import Foreign.C.Types (CInt)
import Proven.FFI (c_proven_password_validate, c_proven_password_is_common)
import Proven.FFI.Types (PasswordResult(..))
import Proven.Core (withCStringLen')

-- | Password strength levels.
data PasswordStrength
  = VeryWeak
  | Weak
  | Fair
  | Strong
  | VeryStrong
  deriving (Eq, Show, Ord)

-- | Information about a validated password.
data PasswordInfo = PasswordInfo
  { piStrength     :: !PasswordStrength
  , piHasLowercase :: !Bool
  , piHasUppercase :: !Bool
  , piHasDigit     :: !Bool
  , piHasSpecial   :: !Bool
  , piLength       :: !Int
  } deriving (Eq, Show)

-- | Validate a password and get strength information.
-- Delegates to @proven_password_validate@ in libproven.
validatePassword :: String -> IO PasswordInfo
validatePassword pwd = withCStringLen' pwd $ \ptr len -> do
  result <- c_proven_password_validate ptr len
  return (PasswordInfo
    (strengthFromCInt (pwStrength result))
    (pwHasLowercase result /= 0)
    (pwHasUppercase result /= 0)
    (pwHasDigit result /= 0)
    (pwHasSpecial result /= 0)
    (fromIntegral (pwLength result)))

-- | Check if a password is in the common passwords list.
-- Delegates to @proven_password_is_common@ in libproven.
isCommonPassword :: String -> IO Bool
isCommonPassword pwd = withCStringLen' pwd $ \ptr len -> do
  result <- c_proven_password_is_common ptr len
  return (result /= 0)

-- | Convert strength CInt to PasswordStrength.
strengthFromCInt :: CInt -> PasswordStrength
strengthFromCInt 0 = VeryWeak
strengthFromCInt 1 = Weak
strengthFromCInt 2 = Fair
strengthFromCInt 3 = Strong
strengthFromCInt 4 = VeryStrong
strengthFromCInt _ = VeryWeak
