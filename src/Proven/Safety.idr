-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Proven.Safety - Convenience re-export module for safety infrastructure primitives
|||
||| Groups all safety policy, concurrency, and distributed systems Safe* modules
||| into a single import for applications that require verified safety invariants.
||| Covers security policies, capability-based access control, linear resource
||| management, ACID transactions, counting semaphores, token-bucket rate
||| limiting, circuit breaker patterns, exponential retry with jitter, verified
||| state machines, service registries, Byzantine consensus, trust hierarchies,
||| monotonic counters, cycle detection in dependency graphs, and data provenance
||| tracking.
|||
||| Usage:
|||   import Proven.Safety
|||
||| This single import provides access to all safety infrastructure types,
||| constructors, and verified operations without needing 15 separate imports.
module Proven.Safety

import public Proven.SafePolicy
import public Proven.SafeCapability
import public Proven.SafeResource
import public Proven.SafeTransaction
import public Proven.SafeSemaphore
import public Proven.SafeRateLimiter
import public Proven.SafeCircuitBreaker
import public Proven.SafeRetry
import public Proven.SafeStateMachine
import public Proven.SafeRegistry
import public Proven.SafeConsensus
import public Proven.SafeTrust
import public Proven.SafeMonotonic
import public Proven.SafeCycleDetect
import public Proven.SafeProvenance
