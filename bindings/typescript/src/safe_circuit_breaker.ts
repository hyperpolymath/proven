// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * Circuit breaker states.
 */
export enum CircuitState {
  Closed = 'closed',
  Open = 'open',
  HalfOpen = 'half_open',
}

/**
 * Circuit breaker configuration.
 */
export interface CircuitBreakerConfig {
  failureThreshold: number;
  successThreshold: number;
  timeout: number; // milliseconds
  halfOpenMaxCalls?: number;
}

/**
 * Circuit breaker statistics.
 */
export interface CircuitStats {
  state: CircuitState;
  failures: number;
  successes: number;
  consecutiveFailures: number;
  consecutiveSuccesses: number;
  totalCalls: number;
  lastFailure?: number;
  lastSuccess?: number;
}

/**
 * CircuitBreaker implements the circuit breaker pattern.
 */
export class CircuitBreaker {
  private state: CircuitState = CircuitState.Closed;
  private failures: number = 0;
  private successes: number = 0;
  private consecutiveFailures: number = 0;
  private consecutiveSuccesses: number = 0;
  private totalCalls: number = 0;
  private lastFailure?: number;
  private lastSuccess?: number;
  private openedAt?: number;
  private halfOpenCalls: number = 0;

  private readonly config: Required<CircuitBreakerConfig>;
  private readonly getNow: () => number;

  constructor(config: CircuitBreakerConfig, getNow: () => number = Date.now) {
    this.config = {
      failureThreshold: Math.max(1, config.failureThreshold),
      successThreshold: Math.max(1, config.successThreshold),
      timeout: Math.max(0, config.timeout),
      halfOpenMaxCalls: config.halfOpenMaxCalls ?? 1,
    };
    this.getNow = getNow;
  }

  /**
   * Create a circuit breaker with default settings.
   */
  static withDefaults(getNow?: () => number): CircuitBreaker {
    return new CircuitBreaker(
      {
        failureThreshold: 5,
        successThreshold: 2,
        timeout: 30000,
        halfOpenMaxCalls: 1,
      },
      getNow
    );
  }

  /**
   * Check if the circuit allows calls.
   */
  canCall(): boolean {
    this.maybeTransitionFromOpen();

    if (this.state === CircuitState.Closed) {
      return true;
    }

    if (this.state === CircuitState.HalfOpen) {
      return this.halfOpenCalls < this.config.halfOpenMaxCalls;
    }

    return false;
  }

  /**
   * Record a successful call.
   */
  recordSuccess(): void {
    this.maybeTransitionFromOpen();
    this.totalCalls++;
    this.successes++;
    this.consecutiveSuccesses++;
    this.consecutiveFailures = 0;
    this.lastSuccess = this.getNow();

    if (this.state === CircuitState.HalfOpen) {
      if (this.consecutiveSuccesses >= this.config.successThreshold) {
        this.transitionToClosed();
      }
    }
  }

  /**
   * Record a failed call.
   */
  recordFailure(): void {
    this.maybeTransitionFromOpen();
    this.totalCalls++;
    this.failures++;
    this.consecutiveFailures++;
    this.consecutiveSuccesses = 0;
    this.lastFailure = this.getNow();

    if (this.state === CircuitState.HalfOpen) {
      this.transitionToOpen();
    } else if (this.state === CircuitState.Closed) {
      if (this.consecutiveFailures >= this.config.failureThreshold) {
        this.transitionToOpen();
      }
    }
  }

  /**
   * Execute a function through the circuit breaker.
   */
  call<T>(fn: () => T): Result<T> {
    if (!this.canCall()) {
      return { ok: false, error: 'Circuit breaker is open' };
    }

    if (this.state === CircuitState.HalfOpen) {
      this.halfOpenCalls++;
    }

    try {
      const result = fn();
      this.recordSuccess();
      return { ok: true, value: result };
    } catch (error) {
      this.recordFailure();
      return { ok: false, error: String(error) };
    }
  }

  /**
   * Execute an async function through the circuit breaker.
   */
  async callAsync<T>(fn: () => Promise<T>): Promise<Result<T>> {
    if (!this.canCall()) {
      return { ok: false, error: 'Circuit breaker is open' };
    }

    if (this.state === CircuitState.HalfOpen) {
      this.halfOpenCalls++;
    }

    try {
      const result = await fn();
      this.recordSuccess();
      return { ok: true, value: result };
    } catch (error) {
      this.recordFailure();
      return { ok: false, error: String(error) };
    }
  }

  /**
   * Get current state.
   */
  getState(): CircuitState {
    this.maybeTransitionFromOpen();
    return this.state;
  }

  /**
   * Get statistics.
   */
  getStats(): CircuitStats {
    this.maybeTransitionFromOpen();
    return {
      state: this.state,
      failures: this.failures,
      successes: this.successes,
      consecutiveFailures: this.consecutiveFailures,
      consecutiveSuccesses: this.consecutiveSuccesses,
      totalCalls: this.totalCalls,
      lastFailure: this.lastFailure,
      lastSuccess: this.lastSuccess,
    };
  }

  /**
   * Reset the circuit breaker.
   */
  reset(): void {
    this.state = CircuitState.Closed;
    this.failures = 0;
    this.successes = 0;
    this.consecutiveFailures = 0;
    this.consecutiveSuccesses = 0;
    this.totalCalls = 0;
    this.lastFailure = undefined;
    this.lastSuccess = undefined;
    this.openedAt = undefined;
    this.halfOpenCalls = 0;
  }

  /**
   * Force the circuit open.
   */
  forceOpen(): void {
    this.transitionToOpen();
  }

  /**
   * Force the circuit closed.
   */
  forceClosed(): void {
    this.transitionToClosed();
  }

  private maybeTransitionFromOpen(): void {
    if (this.state === CircuitState.Open && this.openedAt) {
      const elapsed = this.getNow() - this.openedAt;
      if (elapsed >= this.config.timeout) {
        this.transitionToHalfOpen();
      }
    }
  }

  private transitionToOpen(): void {
    this.state = CircuitState.Open;
    this.openedAt = this.getNow();
    this.halfOpenCalls = 0;
  }

  private transitionToHalfOpen(): void {
    this.state = CircuitState.HalfOpen;
    this.consecutiveSuccesses = 0;
    this.halfOpenCalls = 0;
  }

  private transitionToClosed(): void {
    this.state = CircuitState.Closed;
    this.consecutiveFailures = 0;
    this.openedAt = undefined;
    this.halfOpenCalls = 0;
  }
}

/**
 * CircuitBreakerGroup manages multiple circuit breakers by key.
 */
export class CircuitBreakerGroup {
  private readonly breakers: Map<string, CircuitBreaker> = new Map();
  private readonly config: CircuitBreakerConfig;
  private readonly getNow: () => number;

  constructor(config: CircuitBreakerConfig, getNow: () => number = Date.now) {
    this.config = config;
    this.getNow = getNow;
  }

  /**
   * Get or create a circuit breaker for a key.
   */
  get(key: string): CircuitBreaker {
    let breaker = this.breakers.get(key);
    if (!breaker) {
      breaker = new CircuitBreaker(this.config, this.getNow);
      this.breakers.set(key, breaker);
    }
    return breaker;
  }

  /**
   * Check if a key allows calls.
   */
  canCall(key: string): boolean {
    return this.get(key).canCall();
  }

  /**
   * Record success for a key.
   */
  recordSuccess(key: string): void {
    this.get(key).recordSuccess();
  }

  /**
   * Record failure for a key.
   */
  recordFailure(key: string): void {
    this.get(key).recordFailure();
  }

  /**
   * Get state of all breakers.
   */
  getAllStats(): Map<string, CircuitStats> {
    const stats = new Map<string, CircuitStats>();
    for (const [key, breaker] of this.breakers) {
      stats.set(key, breaker.getStats());
    }
    return stats;
  }

  /**
   * Reset all breakers.
   */
  resetAll(): void {
    for (const breaker of this.breakers.values()) {
      breaker.reset();
    }
  }

  /**
   * Remove a breaker.
   */
  remove(key: string): boolean {
    return this.breakers.delete(key);
  }

  /**
   * Get number of managed breakers.
   */
  get size(): number {
    return this.breakers.size;
  }
}

export const SafeCircuitBreaker = {
  CircuitBreaker,
  CircuitBreakerGroup,
  CircuitState,
};
