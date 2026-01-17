# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    Proven

Safety-first utility functions with formal verification guarantees.
Provides safe operations across 38 modules organized into 7 categories:

## Core (11 modules)
SafeMath, SafeString, SafePath, SafeEmail, SafeUrl, SafeNetwork,
SafeCrypto, SafeUUID, SafeCurrency, SafePhone, SafeHex

## Data (7 modules)
SafeJson, SafeDateTime, SafeFloat, SafeVersion, SafeColor, SafeAngle, SafeUnit

## Data Structures (5 modules)
SafeBuffer, SafeQueue, SafeBloom, SafeLRU, SafeGraph

## Resilience (4 modules)
SafeRateLimiter, SafeCircuitBreaker, SafeRetry, SafeMonotonic

## State (2 modules)
SafeStateMachine, SafeCalculator

## Algorithm (4 modules)
SafeGeo, SafeProbability, SafeChecksum, SafeTensor

## Security (2 modules)
SafePassword, SafeMl

## HTTP (3 modules)
SafeHeader, SafeCookie, SafeContentType
"""
module Proven

const VERSION = v"0.4.0"
const MODULE_COUNT = 38

# Core modules (11)
include("SafeMath.jl")
include("SafeString.jl")
include("SafePath.jl")
include("SafeEmail.jl")
include("SafeUrl.jl")
include("SafeNetwork.jl")
include("SafeCrypto.jl")
include("SafeUUID.jl")
include("SafeCurrency.jl")
include("SafePhone.jl")
include("SafeHex.jl")

# Data modules (7)
include("SafeJson.jl")
include("SafeDateTime.jl")
include("SafeFloat.jl")
include("SafeVersion.jl")
include("SafeColor.jl")
include("SafeAngle.jl")
include("SafeUnit.jl")

# Data Structures modules (5)
include("SafeBuffer.jl")
include("SafeQueue.jl")
include("SafeBloom.jl")
include("SafeLRU.jl")
include("SafeGraph.jl")

# Resilience modules (4)
include("SafeRateLimiter.jl")
include("SafeCircuitBreaker.jl")
include("SafeRetry.jl")
include("SafeMonotonic.jl")

# State modules (2)
include("SafeStateMachine.jl")
include("SafeCalculator.jl")

# Algorithm modules (4)
include("SafeGeo.jl")
include("SafeProbability.jl")
include("SafeChecksum.jl")
include("SafeTensor.jl")

# Security modules (2)
include("SafePassword.jl")
include("SafeMl.jl")

# HTTP modules (3)
include("SafeHeader.jl")
include("SafeCookie.jl")
include("SafeContentType.jl")

# Export all modules
# Core
export SafeMath, SafeString, SafePath, SafeEmail, SafeUrl, SafeNetwork
export SafeCrypto, SafeUUID, SafeCurrency, SafePhone, SafeHex

# Data
export SafeJson, SafeDateTime, SafeFloat, SafeVersion, SafeColor, SafeAngle, SafeUnit

# Data Structures
export SafeBuffer, SafeQueue, SafeBloom, SafeLRU, SafeGraph

# Resilience
export SafeRateLimiter, SafeCircuitBreaker, SafeRetry, SafeMonotonic

# State
export SafeStateMachine, SafeCalculator

# Algorithm
export SafeGeo, SafeProbability, SafeChecksum, SafeTensor

# Security
export SafePassword, SafeMl

# HTTP
export SafeHeader, SafeCookie, SafeContentType

end # module
