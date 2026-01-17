// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeCircuitBreaker - Fault tolerance pattern implementation.
 *
 * Implements the circuit breaker pattern for fault tolerance:
 * - Closed state: Normal operation, requests flow through
 * - Open state: Failing, reject requests immediately
 * - Half-open state: Testing recovery with limited requests
 *
 * All operations return Result types for error handling.
 */

// ============================================================================
// Types
// ============================================================================

/** Circuit breaker states */
type circuitState =
  | Closed    // Normal operation
  | Open      // Failing, reject requests
  | HalfOpen  // Testing recovery

/** Circuit breaker configuration */
type circuitConfig = {
  failureThreshold: int,      // Number of failures before opening
  successThreshold: int,      // Successes in half-open to close
  timeout: float,             // Time in ms before trying half-open
  halfOpenMaxCalls: int,      // Max calls allowed in half-open state
}

/** Circuit breaker state */
type circuitBreaker = {
  config: circuitConfig,
  mutable state: circuitState,
  mutable failures: int,
  mutable successes: int,
  mutable lastFailureTime: float,
  mutable halfOpenCalls: int,
}

/** Execution result */
type executionResult =
  | Executed({success: bool})
  | Rejected({reason: string, retryAfter: float})

// ============================================================================
// Default Configuration
// ============================================================================

/** Default circuit breaker configuration */
let defaultConfig: circuitConfig = {
  failureThreshold: 5,
  successThreshold: 2,
  timeout: 30000.0, // 30 seconds
  halfOpenMaxCalls: 3,
}

// ============================================================================
// Circuit Breaker Operations
// ============================================================================

/** Create a new circuit breaker with custom configuration */
let make = (config: circuitConfig): result<circuitBreaker, string> => {
  if config.failureThreshold <= 0 {
    Error("Failure threshold must be positive")
  } else if config.successThreshold <= 0 {
    Error("Success threshold must be positive")
  } else if config.timeout <= 0.0 {
    Error("Timeout must be positive")
  } else if config.halfOpenMaxCalls <= 0 {
    Error("Half-open max calls must be positive")
  } else {
    Ok({
      config,
      state: Closed,
      failures: 0,
      successes: 0,
      lastFailureTime: 0.0,
      halfOpenCalls: 0,
    })
  }
}

/** Create a new circuit breaker with default configuration */
let makeDefault = (): circuitBreaker => {
  {
    config: defaultConfig,
    state: Closed,
    failures: 0,
    successes: 0,
    lastFailureTime: 0.0,
    halfOpenCalls: 0,
  }
}

/** Check if circuit should transition to half-open */
let shouldTransitionToHalfOpen = (cb: circuitBreaker, currentTime: float): bool => {
  cb.state == Open && currentTime >= cb.lastFailureTime +. cb.config.timeout
}

/** Update circuit state based on current time */
let updateState = (cb: circuitBreaker, currentTime: float): unit => {
  if shouldTransitionToHalfOpen(cb, currentTime) {
    cb.state = HalfOpen
    cb.successes = 0
    cb.halfOpenCalls = 0
  }
}

/** Check if a request can be executed */
let canExecute = (cb: circuitBreaker, currentTime: float): bool => {
  updateState(cb, currentTime)

  switch cb.state {
  | Closed => true
  | Open => false
  | HalfOpen => cb.halfOpenCalls < cb.config.halfOpenMaxCalls
  }
}

/** Record a successful call */
let recordSuccess = (cb: circuitBreaker): unit => {
  switch cb.state {
  | Closed => {
      cb.failures = 0
    }
  | HalfOpen => {
      cb.successes = cb.successes + 1
      if cb.successes >= cb.config.successThreshold {
        cb.state = Closed
        cb.failures = 0
        cb.successes = 0
      }
    }
  | Open => ()
  }
}

/** Record a failed call */
let recordFailure = (cb: circuitBreaker, currentTime: float): unit => {
  cb.lastFailureTime = currentTime

  switch cb.state {
  | Closed => {
      cb.failures = cb.failures + 1
      if cb.failures >= cb.config.failureThreshold {
        cb.state = Open
      }
    }
  | HalfOpen => {
      cb.state = Open
      cb.failures = cb.failures + 1
    }
  | Open => {
      cb.failures = cb.failures + 1
    }
  }
}

/** Record an attempt (for half-open tracking) */
let recordAttempt = (cb: circuitBreaker): unit => {
  if cb.state == HalfOpen {
    cb.halfOpenCalls = cb.halfOpenCalls + 1
  }
}

/** Execute with circuit breaker logic */
let execute = (cb: circuitBreaker, currentTime: float, success: bool): executionResult => {
  updateState(cb, currentTime)

  if !canExecute(cb, currentTime) {
    let retryAfter = timeUntilRetry(cb, currentTime)
    Rejected({reason: "Circuit is open", retryAfter})
  } else {
    recordAttempt(cb)

    if success {
      recordSuccess(cb)
    } else {
      recordFailure(cb, currentTime)
    }

    Executed({success})
  }
}

/** Execute with circuit breaker, using callback for the actual operation */
and tryExecute = (cb: circuitBreaker, currentTime: float, operation: unit => bool): executionResult => {
  updateState(cb, currentTime)

  if !canExecute(cb, currentTime) {
    let retryAfter = timeUntilRetry(cb, currentTime)
    Rejected({reason: "Circuit is open", retryAfter})
  } else {
    recordAttempt(cb)

    try {
      let success = operation()
      if success {
        recordSuccess(cb)
      } else {
        recordFailure(cb, currentTime)
      }
      Executed({success})
    } catch {
    | _ => {
        recordFailure(cb, currentTime)
        Executed({success: false})
      }
    }
  }
}

/** Check if circuit is healthy (closed) */
and isHealthy = (cb: circuitBreaker): bool => {
  cb.state == Closed
}

/** Time until circuit might close (0 if not open) */
and timeUntilRetry = (cb: circuitBreaker, currentTime: float): float => {
  if cb.state != Open {
    0.0
  } else {
    let retryTime = cb.lastFailureTime +. cb.config.timeout
    if currentTime >= retryTime {
      0.0
    } else {
      retryTime -. currentTime
    }
  }
}

/** Force reset the circuit breaker */
let reset = (cb: circuitBreaker): unit => {
  cb.state = Closed
  cb.failures = 0
  cb.successes = 0
  cb.halfOpenCalls = 0
}

/** Force the circuit to open state */
let forceOpen = (cb: circuitBreaker, currentTime: float): unit => {
  cb.state = Open
  cb.lastFailureTime = currentTime
}

/** Force the circuit to closed state */
let forceClosed = (cb: circuitBreaker): unit => {
  cb.state = Closed
  cb.failures = 0
  cb.successes = 0
  cb.halfOpenCalls = 0
}

// ============================================================================
// State Inspection
// ============================================================================

/** Get current circuit state */
let getState = (cb: circuitBreaker): circuitState => {
  cb.state
}

/** Get current failure count */
let getFailureCount = (cb: circuitBreaker): int => {
  cb.failures
}

/** Get current success count (relevant in half-open) */
let getSuccessCount = (cb: circuitBreaker): int => {
  cb.successes
}

/** Get number of calls made in half-open state */
let getHalfOpenCalls = (cb: circuitBreaker): int => {
  cb.halfOpenCalls
}

/** Convert state to string for logging */
let stateToString = (state: circuitState): string => {
  switch state {
  | Closed => "closed"
  | Open => "open"
  | HalfOpen => "half-open"
  }
}

/** Check if execution result was successful */
let wasExecuted = (result: executionResult): bool => {
  switch result {
  | Executed(_) => true
  | Rejected(_) => false
  }
}

/** Check if execution result was rejected */
let wasRejected = (result: executionResult): bool => {
  switch result {
  | Executed(_) => false
  | Rejected(_) => true
  }
}

/** Get retry after time from execution result */
let getExecutionRetryAfter = (result: executionResult): option<float> => {
  switch result {
  | Executed(_) => None
  | Rejected({retryAfter}) => Some(retryAfter)
  }
}
