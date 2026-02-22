// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCircuitBreaker - Typed wrapper for the circuit breaker pattern.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * The JS FFI layer provides CircuitBreaker and CircuitBreakerGroup classes
 * backed by native circuit breaker implementations.
 */

/** Circuit breaker states. */
type circuitState =
  | Closed
  | Open
  | HalfOpen

/** Circuit breaker configuration. */
type circuitBreakerConfig = {
  failureThreshold: int,
  successThreshold: int,
  timeout: int,
  halfOpenMaxCalls: option<int>,
}

/** Opaque handle to a JavaScript CircuitBreaker instance. */
type circuitBreaker

/** Opaque handle to a JavaScript CircuitBreakerGroup instance. */
type circuitBreakerGroup

/** JavaScript bindings to the SafeCircuitBreaker FFI wrapper. */
module SafeCircuitBreakerJs = {
  @module("../../javascript/src/safe_circuit_breaker.js")
  @new
  external createBreaker: circuitBreakerConfig => circuitBreaker = "CircuitBreaker"

  @module("../../javascript/src/safe_circuit_breaker.js")
  @new
  external createGroup: circuitBreakerConfig => circuitBreakerGroup = "CircuitBreakerGroup"
}

/**
 * Create a new circuit breaker.
 *
 * @param config Circuit breaker configuration.
 * @returns A new CircuitBreaker handle.
 */
let createBreaker = SafeCircuitBreakerJs.createBreaker

/**
 * Create a new circuit breaker group.
 *
 * @param config Configuration for all breakers in the group.
 * @returns A new CircuitBreakerGroup handle.
 */
let createGroup = SafeCircuitBreakerJs.createGroup

/**
 * Create a circuit breaker with default settings.
 *
 * @returns A CircuitBreaker with threshold=5, successThreshold=2, timeout=30000ms.
 */
let withDefaults = (): circuitBreaker => {
  createBreaker({
    failureThreshold: 5,
    successThreshold: 2,
    timeout: 30000,
    halfOpenMaxCalls: Some(1),
  })
}
