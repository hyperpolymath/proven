// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeRetry - Exponential backoff and retry strategies.
 *
 * Provides safe retry algorithms that cannot crash:
 * - Fixed delay retry
 * - Linear backoff retry
 * - Exponential backoff retry
 * - Jittered exponential backoff
 *
 * All operations return Result types for error handling.
 */

// ============================================================================
// Types
// ============================================================================

/** Backoff strategy variants */
type backoffStrategy =
  | Fixed({delay: float})
  | Linear({initial: float, increment: float})
  | Exponential({initial: float, multiplier: float})
  | Jittered({initial: float, multiplier: float, maxJitter: float})

/** Retry configuration */
type retryConfig = {
  maxAttempts: int,
  backoff: backoffStrategy,
  maxDelay: float,
}

/** Retry state */
type retryState = {
  mutable attempt: int,
  mutable totalDelay: float,
  mutable lastError: option<string>,
}

/** Retry executor */
type retryExecutor = {
  config: retryConfig,
  state: retryState,
}

/** Retry result */
type retryResult<'a> =
  | Success('a)
  | Exhausted({attempts: int, lastError: option<string>})
  | NoRetry({reason: string})

// ============================================================================
// Default Configuration
// ============================================================================

/** Default retry configuration with exponential backoff */
let defaultConfig: retryConfig = {
  maxAttempts: 3,
  backoff: Exponential({initial: 100.0, multiplier: 2.0}),
  maxDelay: 30000.0, // 30 seconds
}

/** Create a fixed delay backoff strategy */
let fixedBackoff = (delay: float): backoffStrategy => {
  Fixed({delay: delay})
}

/** Create a linear backoff strategy */
let linearBackoff = (~initial: float, ~increment: float): backoffStrategy => {
  Linear({initial: initial, increment: increment})
}

/** Create an exponential backoff strategy */
let exponentialBackoff = (~initial: float, ~multiplier: float): backoffStrategy => {
  Exponential({initial: initial, multiplier: multiplier})
}

/** Create a jittered exponential backoff strategy */
let jitteredBackoff = (~initial: float, ~multiplier: float, ~maxJitter: float): backoffStrategy => {
  Jittered({initial, multiplier, maxJitter})
}

// ============================================================================
// Delay Calculation
// ============================================================================

/** Calculate delay for a given attempt number */
let calculateDelay = (strategy: backoffStrategy, attempt: int): float => {
  let attemptFloat = Belt.Int.toFloat(attempt)
  switch strategy {
  | Fixed({delay}) => delay
  | Linear({initial, increment}) => initial +. increment *. attemptFloat
  | Exponential({initial, multiplier}) => initial *. Js.Math.pow_float(~base=multiplier, ~exp=attemptFloat)
  | Jittered({initial, multiplier, maxJitter: _}) =>
    initial *. Js.Math.pow_float(~base=multiplier, ~exp=attemptFloat)
  }
}

/** Calculate delay with cap applied */
let calculateDelayWithCap = (config: retryConfig, attempt: int): float => {
  let rawDelay = calculateDelay(config.backoff, attempt)
  Js.Math.min_float(config.maxDelay, rawDelay)
}

/** Add jitter to a delay value */
let addJitter = (delay: float, maxJitter: float): float => {
  let jitter = Js.Math.random() *. maxJitter *. 2.0 -. maxJitter
  Js.Math.max_float(0.0, delay +. jitter)
}

/** Calculate delay with jitter if applicable */
let calculateDelayWithJitter = (config: retryConfig, attempt: int): float => {
  let delay = calculateDelayWithCap(config, attempt)
  switch config.backoff {
  | Jittered({maxJitter, _}) => addJitter(delay, maxJitter)
  | _ => delay
  }
}

// ============================================================================
// Retry Executor Operations
// ============================================================================

/** Create a new retry executor with custom configuration */
let make = (config: retryConfig): result<retryExecutor, string> => {
  if config.maxAttempts <= 0 {
    Error("Max attempts must be positive")
  } else if config.maxDelay <= 0.0 {
    Error("Max delay must be positive")
  } else {
    Ok({
      config,
      state: {
        attempt: 0,
        totalDelay: 0.0,
        lastError: None,
      },
    })
  }
}

/** Create a new retry executor with default configuration */
let makeDefault = (): retryExecutor => {
  {
    config: defaultConfig,
    state: {
      attempt: 0,
      totalDelay: 0.0,
      lastError: None,
    },
  }
}

/** Create a retry executor with specific settings */
let makeWith = (~maxAttempts: int, ~backoff: backoffStrategy, ~maxDelay: float): result<retryExecutor, string> => {
  make({maxAttempts, backoff, maxDelay})
}

/** Check if more retries are allowed */
let canRetry = (executor: retryExecutor): bool => {
  executor.state.attempt < executor.config.maxAttempts
}

/** Get remaining attempts */
let remainingAttempts = (executor: retryExecutor): int => {
  executor.config.maxAttempts - executor.state.attempt
}

/** Advance to next attempt, returning delay */
let nextAttempt = (executor: retryExecutor, errorMessage: option<string>): option<float> => {
  if !canRetry(executor) {
    None
  } else {
    let delay = calculateDelayWithJitter(executor.config, executor.state.attempt)
    executor.state.attempt = executor.state.attempt + 1
    executor.state.totalDelay = executor.state.totalDelay +. delay
    executor.state.lastError = errorMessage
    Some(delay)
  }
}

/** Get current attempt number (1-indexed for display) */
let currentAttempt = (executor: retryExecutor): int => {
  executor.state.attempt
}

/** Get total delay accumulated */
let totalDelay = (executor: retryExecutor): float => {
  executor.state.totalDelay
}

/** Get last error message */
let lastError = (executor: retryExecutor): option<string> => {
  executor.state.lastError
}

/** Reset executor state */
let reset = (executor: retryExecutor): unit => {
  executor.state.attempt = 0
  executor.state.totalDelay = 0.0
  executor.state.lastError = None
}

// ============================================================================
// Retry Execution Helpers
// ============================================================================

/** Execute with retry, using synchronous callback */
let executeSync = (executor: retryExecutor, operation: unit => result<'a, string>): retryResult<'a> => {
  reset(executor)

  let rec loop = () => {
    switch operation() {
    | Ok(value) => Success(value)
    | Error(err) => {
        switch nextAttempt(executor, Some(err)) {
        | None => Exhausted({attempts: executor.state.attempt, lastError: executor.state.lastError})
        | Some(_delay) => loop()
        }
      }
    }
  }

  if executor.config.maxAttempts <= 0 {
    NoRetry({reason: "No attempts configured"})
  } else {
    loop()
  }
}

/** Get delay for next retry without advancing state */
let peekNextDelay = (executor: retryExecutor): option<float> => {
  if !canRetry(executor) {
    None
  } else {
    Some(calculateDelayWithJitter(executor.config, executor.state.attempt))
  }
}

/** Get all delays that would be used for max attempts */
let getAllDelays = (config: retryConfig): array<float> => {
  Belt.Array.makeBy(config.maxAttempts, i => calculateDelayWithCap(config, i))
}

/** Calculate total maximum delay for all retries */
let totalMaxDelay = (config: retryConfig): float => {
  Belt.Array.reduce(getAllDelays(config), 0.0, (acc, delay) => acc +. delay)
}

// ============================================================================
// Utility Functions
// ============================================================================

/** Check if retry result was successful */
let isSuccess = (result: retryResult<'a>): bool => {
  switch result {
  | Success(_) => true
  | Exhausted(_) | NoRetry(_) => false
  }
}

/** Get value from successful retry result */
let getValue = (result: retryResult<'a>): option<'a> => {
  switch result {
  | Success(value) => Some(value)
  | Exhausted(_) | NoRetry(_) => None
  }
}

/** Get attempt count from exhausted result */
let getExhaustedAttempts = (result: retryResult<'a>): option<int> => {
  switch result {
  | Success(_) | NoRetry(_) => None
  | Exhausted({attempts}) => Some(attempts)
  }
}

/** Convert retry result to Result type */
let toResult = (result: retryResult<'a>): result<'a, string> => {
  switch result {
  | Success(value) => Ok(value)
  | Exhausted({attempts, lastError}) => {
      let errorMsg = switch lastError {
      | Some(err) => `Exhausted after ${Belt.Int.toString(attempts)} attempts: ${err}`
      | None => `Exhausted after ${Belt.Int.toString(attempts)} attempts`
      }
      Error(errorMsg)
    }
  | NoRetry({reason}) => Error(reason)
  }
}

/** Sleep for specified duration (returns a promise) */
let sleep = (ms: float): promise<unit> => {
  Js.Promise.make((~resolve, ~reject as _) => {
    let _ = Js.Global.setTimeout(() => resolve(), Belt.Float.toInt(ms))
  })
}
