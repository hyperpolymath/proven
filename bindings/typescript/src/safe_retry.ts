// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * Retry strategies.
 */
export enum RetryStrategy {
  Fixed = 'fixed',
  Linear = 'linear',
  Exponential = 'exponential',
}

/**
 * Retry configuration.
 */
export interface RetryConfig {
  maxAttempts: number;
  strategy: RetryStrategy;
  baseDelay: number; // milliseconds
  maxDelay?: number; // milliseconds
  jitterFactor?: number; // 0 to 1
  multiplier?: number; // for linear/exponential
}

/**
 * Result of a retry attempt.
 */
export interface RetryResult<T> {
  ok: boolean;
  value?: T;
  error?: string;
  attempts: number;
  totalDelay: number;
}

/**
 * Calculate delay for a given attempt.
 */
export function calculateDelay(config: RetryConfig, attempt: number): number {
  const multiplier = config.multiplier ?? 2;
  let delay: number;

  switch (config.strategy) {
    case RetryStrategy.Fixed:
      delay = config.baseDelay;
      break;
    case RetryStrategy.Linear:
      delay = config.baseDelay * (1 + attempt * multiplier);
      break;
    case RetryStrategy.Exponential:
      delay = config.baseDelay * Math.pow(multiplier, attempt);
      break;
  }

  // Apply max delay cap
  const maxDelay = config.maxDelay ?? Infinity;
  delay = Math.min(delay, maxDelay);

  // Apply jitter
  const jitterFactor = config.jitterFactor ?? 0;
  if (jitterFactor > 0) {
    const jitter = delay * jitterFactor * (Math.random() * 2 - 1);
    delay = Math.max(0, delay + jitter);
  }

  return delay;
}

/**
 * Sleep for a given number of milliseconds.
 */
function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Retry an async operation with backoff.
 */
export async function retryAsync<T>(
  fn: () => Promise<T>,
  config: RetryConfig
): Promise<RetryResult<T>> {
  const maxAttempts = Math.max(1, config.maxAttempts);
  let lastError: string | undefined;
  let totalDelay = 0;

  for (let attempt = 0; attempt < maxAttempts; attempt++) {
    try {
      const value = await fn();
      return {
        ok: true,
        value,
        attempts: attempt + 1,
        totalDelay,
      };
    } catch (error) {
      lastError = String(error);

      if (attempt < maxAttempts - 1) {
        const delay = calculateDelay(config, attempt);
        totalDelay += delay;
        await sleep(delay);
      }
    }
  }

  return {
    ok: false,
    error: lastError,
    attempts: maxAttempts,
    totalDelay,
  };
}

/**
 * Retry an async operation with selective error handling.
 */
export async function retryAsyncSelective<T>(
  fn: () => Promise<T>,
  config: RetryConfig,
  shouldRetry: (error: unknown) => boolean
): Promise<RetryResult<T>> {
  const maxAttempts = Math.max(1, config.maxAttempts);
  let lastError: string | undefined;
  let totalDelay = 0;

  for (let attempt = 0; attempt < maxAttempts; attempt++) {
    try {
      const value = await fn();
      return {
        ok: true,
        value,
        attempts: attempt + 1,
        totalDelay,
      };
    } catch (error) {
      lastError = String(error);

      if (!shouldRetry(error)) {
        return {
          ok: false,
          error: lastError,
          attempts: attempt + 1,
          totalDelay,
        };
      }

      if (attempt < maxAttempts - 1) {
        const delay = calculateDelay(config, attempt);
        totalDelay += delay;
        await sleep(delay);
      }
    }
  }

  return {
    ok: false,
    error: lastError,
    attempts: maxAttempts,
    totalDelay,
  };
}

/**
 * Retry a synchronous operation.
 */
export function retry<T>(fn: () => T, config: RetryConfig): RetryResult<T> {
  const maxAttempts = Math.max(1, config.maxAttempts);
  let lastError: string | undefined;

  for (let attempt = 0; attempt < maxAttempts; attempt++) {
    try {
      const value = fn();
      return {
        ok: true,
        value,
        attempts: attempt + 1,
        totalDelay: 0,
      };
    } catch (error) {
      lastError = String(error);
    }
  }

  return {
    ok: false,
    error: lastError,
    attempts: maxAttempts,
    totalDelay: 0,
  };
}

/**
 * RetryBuilder provides a fluent API for building retry configurations.
 */
export class RetryBuilder {
  private config: RetryConfig;

  constructor() {
    this.config = {
      maxAttempts: 3,
      strategy: RetryStrategy.Exponential,
      baseDelay: 1000,
      maxDelay: 30000,
      jitterFactor: 0.1,
      multiplier: 2,
    };
  }

  /**
   * Set maximum attempts.
   */
  maxAttempts(n: number): RetryBuilder {
    this.config.maxAttempts = Math.max(1, n);
    return this;
  }

  /**
   * Use fixed delay strategy.
   */
  fixed(delayMs: number): RetryBuilder {
    this.config.strategy = RetryStrategy.Fixed;
    this.config.baseDelay = delayMs;
    return this;
  }

  /**
   * Use linear backoff strategy.
   */
  linear(baseDelayMs: number, multiplier: number = 1): RetryBuilder {
    this.config.strategy = RetryStrategy.Linear;
    this.config.baseDelay = baseDelayMs;
    this.config.multiplier = multiplier;
    return this;
  }

  /**
   * Use exponential backoff strategy.
   */
  exponential(baseDelayMs: number, multiplier: number = 2): RetryBuilder {
    this.config.strategy = RetryStrategy.Exponential;
    this.config.baseDelay = baseDelayMs;
    this.config.multiplier = multiplier;
    return this;
  }

  /**
   * Set maximum delay cap.
   */
  maxDelay(ms: number): RetryBuilder {
    this.config.maxDelay = ms;
    return this;
  }

  /**
   * Set jitter factor (0 to 1).
   */
  jitter(factor: number): RetryBuilder {
    this.config.jitterFactor = Math.max(0, Math.min(1, factor));
    return this;
  }

  /**
   * No jitter.
   */
  noJitter(): RetryBuilder {
    this.config.jitterFactor = 0;
    return this;
  }

  /**
   * Build the configuration.
   */
  build(): RetryConfig {
    return { ...this.config };
  }

  /**
   * Execute an async function with this retry configuration.
   */
  async executeAsync<T>(fn: () => Promise<T>): Promise<RetryResult<T>> {
    return retryAsync(fn, this.config);
  }

  /**
   * Execute a sync function with this retry configuration.
   */
  execute<T>(fn: () => T): RetryResult<T> {
    return retry(fn, this.config);
  }
}

/**
 * Create common retry configurations.
 */
export const RetryPresets = {
  /**
   * Quick retries for local operations.
   */
  quick(): RetryConfig {
    return {
      maxAttempts: 3,
      strategy: RetryStrategy.Fixed,
      baseDelay: 100,
      jitterFactor: 0,
    };
  },

  /**
   * Standard exponential backoff for network operations.
   */
  standard(): RetryConfig {
    return {
      maxAttempts: 5,
      strategy: RetryStrategy.Exponential,
      baseDelay: 1000,
      maxDelay: 30000,
      jitterFactor: 0.1,
      multiplier: 2,
    };
  },

  /**
   * Aggressive retries for critical operations.
   */
  aggressive(): RetryConfig {
    return {
      maxAttempts: 10,
      strategy: RetryStrategy.Exponential,
      baseDelay: 500,
      maxDelay: 60000,
      jitterFactor: 0.2,
      multiplier: 2,
    };
  },

  /**
   * Gentle retries for rate-limited APIs.
   */
  gentle(): RetryConfig {
    return {
      maxAttempts: 5,
      strategy: RetryStrategy.Linear,
      baseDelay: 5000,
      maxDelay: 60000,
      jitterFactor: 0.1,
      multiplier: 1,
    };
  },
};

export const SafeRetry = {
  RetryStrategy,
  RetryBuilder,
  RetryPresets,
  calculateDelay,
  retry,
  retryAsync,
  retryAsyncSelective,
};
