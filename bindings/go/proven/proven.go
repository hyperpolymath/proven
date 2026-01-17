// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

// Package proven provides safety-first utility functions with formal verification guarantees.
// It offers 38 modules organized into categories: Core, Data, Data Structures, Resilience,
// State, Algorithm, Security, and HTTP.
package proven

// Version is the current version of the proven library.
const Version = "0.4.0"

// ModuleCount is the total number of modules in the proven library.
const ModuleCount = 38

// Module categories and their modules:
//
// Core (11): SafeMath, SafeString, SafePath, SafeEmail, SafeUrl, SafeNetwork,
//            SafeCrypto, SafeUUID, SafeCurrency, SafePhone, SafeHex
//
// Data (7): SafeJson, SafeDateTime, SafeFloat, SafeVersion, SafeColor,
//           SafeAngle, SafeUnit
//
// Data Structures (5): SafeBuffer, SafeQueue, SafeBloom, SafeLRU, SafeGraph
//
// Resilience (4): SafeRateLimiter, SafeCircuitBreaker, SafeRetry, SafeMonotonic
//
// State (2): SafeStateMachine, SafeCalculator
//
// Algorithm (4): SafeGeo, SafeProbability, SafeChecksum, SafeTensor
//
// Security (2): SafePassword, SafeMl
//
// HTTP (3): SafeHeader, SafeCookie, SafeContentType
