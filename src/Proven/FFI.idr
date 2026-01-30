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
import Proven.FFI.SafeRegex
import Proven.FFI.SafeXML
import Proven.FFI.SafeYAML
import Proven.FFI.SafeTOML
import Proven.FFI.SafeNetwork
import Proven.FFI.SafeUrl
import Proven.FFI.SafeEnv
import Proven.FFI.SafeArgs
import Proven.FFI.SafeFile
import Proven.FFI.SafeUUID
import Proven.FFI.SafeCurrency
import Proven.FFI.SafeHex
import Proven.FFI.SafeTree
import Proven.FFI.SafeBuffer
import Proven.FFI.SafePolicy
import Proven.FFI.SafeProvenance
import Proven.FFI.SafeStateMachine
import Proven.FFI.SafeMarkdown
import Proven.FFI.SafeLog
import Proven.FFI.SafeCron
import Proven.FFI.SafeFloat
import Proven.FFI.SafeColor
import Proven.FFI.SafeVersion
import Proven.FFI.SafeChecksum
import Proven.FFI.SafeGeo
import Proven.FFI.SafeAngle
import Proven.FFI.SafeProbability
import Proven.FFI.SafeUnit
import Proven.FFI.SafeQueue
import Proven.FFI.SafeBloom
import Proven.FFI.SafeLRU
import Proven.FFI.SafeRateLimiter
import Proven.FFI.SafeCircuitBreaker
import Proven.FFI.SafeRetry
import Proven.FFI.SafeMonotonic
import Proven.FFI.SafeGraph
import Proven.FFI.SafeResource
import Proven.FFI.SafeTransaction
import Proven.FFI.SafeCapability
import Proven.FFI.SafeConsensus
import Proven.FFI.SafeFiniteField
import Proven.FFI.SafeOrdering
import Proven.FFI.SafeTensor
import Proven.FFI.SafeSchema
import Proven.FFI.SafeUUID
import Proven.FFI.SafeCurrency
import Proven.FFI.SafeHex
import Proven.FFI.SafeTree
import Proven.FFI.SafeBuffer
import Proven.FFI.SafePolicy

%default total
