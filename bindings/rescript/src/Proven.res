// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

@@warning("-27")

/**
 * Proven - Code that cannot crash
 *
 * ReScript bindings for verified safety functions.
 * Calls Idris 2 verified code via Zig ABI (WASM/WASI).
 *
 * Module Count: 87
 */

// ============================================================================
// Core Modules (18)
// ============================================================================

module SafeMath = Proven_SafeMath
module SafeString = Proven_SafeString
module SafePath = Proven_SafePath
module SafeEmail = Proven_SafeEmail
module SafeUrl = Proven_SafeUrl
module SafeNetwork = Proven_SafeNetwork
module SafeCrypto = Proven_SafeCrypto
module SafeUuid = Proven_SafeUuid
module SafeCurrency = Proven_SafeCurrency
module SafePhone = Proven_SafePhone
module SafeHex = Proven_SafeHex
module SafeEnv = Proven_SafeEnv
module SafeFile = Proven_SafeFile
module SafeFiniteField = Proven_SafeFiniteField
module SafeArgs = Proven_SafeArgs
module SafeDns = Proven_SafeDns
module SafeDocker = Proven_SafeDocker
module SafeDecimal = Proven_SafeDecimal

// ============================================================================
// Encoding Modules (5)
// ============================================================================

module SafeBase64 = Proven_SafeBase64
module SafeHtml = Proven_SafeHtml
module SafeXml = Proven_SafeXml
module SafeJson = Proven_SafeJson
module SafeMarkdown = Proven_SafeMarkdown

// ============================================================================
// Data Format Modules (11)
// ============================================================================

module SafeDateTime = Proven_SafeDateTime
module SafeFloat = Proven_SafeFloat
module SafeVersion = Proven_SafeVersion
module SafeColor = Proven_SafeColor
module SafeAngle = Proven_SafeAngle
module SafeUnit = Proven_SafeUnit
module SafeToml = Proven_SafeToml
module SafeYaml = Proven_SafeYaml
module SafeCsv = Proven_SafeCsv
module SafeCron = Proven_SafeCron
module SafeBibtex = Proven_SafeBibtex

// ============================================================================
// Numeric Modules (5)
// ============================================================================

module SafeComplex = Proven_SafeComplex
module SafeMatrix = Proven_SafeMatrix
module SafeRational = Proven_SafeRational
module SafeInterval = Proven_SafeInterval
module SafeProbability = Proven_SafeProbability

// ============================================================================
// Data Structure Modules (10)
// ============================================================================

module SafeBuffer = Proven_SafeBuffer
module SafeQueue = Proven_SafeQueue
module SafeBloom = Proven_SafeBloom
module SafeLru = Proven_SafeLru
module SafeGraph = Proven_SafeGraph
module SafeBitset = Proven_SafeBitset
module SafeHeap = Proven_SafeHeap
module SafeTree = Proven_SafeTree
module SafeSet = Proven_SafeSet
module SafeUnionFind = Proven_SafeUnionFind

// ============================================================================
// Resilience Modules (6)
// ============================================================================

module SafeRateLimiter = Proven_SafeRateLimiter
module SafeCircuitBreaker = Proven_SafeCircuitBreaker
module SafeRetry = Proven_SafeRetry
module SafeMonotonic = Proven_SafeMonotonic
module SafeConsensus = Proven_SafeConsensus
module SafeSemaphore = Proven_SafeSemaphore

// ============================================================================
// State Modules (3)
// ============================================================================

module SafeStateMachine = Proven_SafeStateMachine
module SafeCalculator = Proven_SafeCalculator
module SafeTransaction = Proven_SafeTransaction

// ============================================================================
// Algorithm Modules (5)
// ============================================================================

module SafeGeo = Proven_SafeGeo
module SafeChecksum = Proven_SafeChecksum
module SafeTensor = Proven_SafeTensor
module SafeOrdering = Proven_SafeOrdering
module SafeLog = Proven_SafeLog

// ============================================================================
// Security Modules (9)
// ============================================================================

module SafePassword = Proven_SafePassword
module SafeMl = Proven_SafeMl
module SafeCert = Proven_SafeCert
module SafeCommand = Proven_SafeCommand
module SafeCapability = Proven_SafeCapability
module SafeOauth = Proven_SafeOauth
module SafeJwt = Proven_SafeJwt
module SafePolicy = Proven_SafePolicy
module SafeRegex = Proven_SafeRegex

// ============================================================================
// Infrastructure Modules (8)
// ============================================================================

module SafeGit = Proven_SafeGit
module SafeSsh = Proven_SafeSsh
module SafeTemplate = Proven_SafeTemplate
module SafeArchive = Proven_SafeArchive
module SafeResource = Proven_SafeResource
module SafeSchema = Proven_SafeSchema
module SafeProvenance = Proven_SafeProvenance
module SafeI18n = Proven_SafeI18n

// ============================================================================
// HTTP/Web Modules (5)
// ============================================================================

module SafeHeader = Proven_SafeHeader
module SafeCookie = Proven_SafeCookie
module SafeContentType = Proven_SafeContentType
module SafeHttp = Proven_SafeHttp
module SafeWebhook = Proven_SafeWebhook

// ============================================================================
// Protocol Modules (2)
// ============================================================================

module SafeMcp = Proven_SafeMcp
module SafeSql = Proven_SafeSql

// ============================================================================
// FFI Layer
// ============================================================================

module FFI = Proven_FFI

let version = "0.5.0"
let moduleCount = 87

/** Initialize the WASM module
 * @param wasmPath Path to the proven.wasm file
 */
let init = Proven_FFI.init

/** Check if initialized */
let isInitialized = Proven_FFI.isInitialized

/** Cleanup */
let deinit = Proven_FFI.deinit
