-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeEmail - FFI bindings to libproven email validation
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeEmail
  ( isValidEmail
  ) where

import Prelude

-- | Check if a string is a valid email address (delegates to Idris 2).
-- | Uses RFC 5321/5322 compliant validation.
foreign import isValidEmailImpl :: String -> Boolean

isValidEmail :: String -> Boolean
isValidEmail = isValidEmailImpl
