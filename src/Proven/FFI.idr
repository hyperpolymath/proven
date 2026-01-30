-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| Proven FFI aggregation module
|||
||| This module re-exports all FFI functions from individual modules.
||| Each FFI module exports Idris2 functions to the C ABI for consumption by Zig.
module Proven.FFI

import Proven.FFI.SafePath
import Proven.FFI.SafeJson
import Proven.FFI.SafeMath
import Proven.FFI.SafeString
import Proven.FFI.SafeEmail
import Proven.FFI.SafeDateTime
import Proven.FFI.SafeHtml
import Proven.FFI.SafeCookie
import Proven.FFI.SafeHeader
import Proven.FFI.SafeContentType
import Proven.FFI.SafeJWT
import Proven.FFI.SafeSQL
import Proven.FFI.SafeCommand
import Proven.FFI.SafeCrypto
import Proven.FFI.SafePassword
import Proven.FFI.SafeNetwork
import Proven.FFI.SafeUrl

%default total
