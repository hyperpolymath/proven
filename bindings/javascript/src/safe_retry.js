// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeRetry - Retry strategies with exponential backoff.
 *
 * Provides configurable retry logic with jitter and limits.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Retry strategy types.
 * @readonly
 * @enum {string}
 */
export const RetryStrategy = {
  IMMEDIATE: 'immediate',
  FIXED: 'fixed',
  LINEAR: 'linear',
  EXPONENTIAL: 'exponential',
};

/**
 * Retry configuration.
 */
export class RetryConfig {
  /** @type {number} */
  #maxAttempts;
  /** @type {RetryStrategy} */
  #strategy;
  /** @type {number} */
  #baseDelay;
  /** @type {number} */
  #maxDelay;
  /** @type {number} */
  #jitter;
  /** @type {number} */
  #multiplier;

  /**
   * Create retry configuration.
   *
   * @param {Object} options - Options
   * @param {number} [options.maxAttempts=3] - Maximum attempts
   * @param {RetryStrategy} [options.strategy='exponential'] - Backoff strategy
   * @param {number} [options.baseDelay=1000] - Base delay in ms
   * @param {number} [options.maxDelay=30000] - Maximum delay in ms
   * @param {number} [options.jitter=0.1] - Jitter factor (0-1)
   * @param {number} [options.multiplier=2] - Exponential multiplier
   */
  constructor(options = {}) {
    const {
      maxAttempts = 3,
      strategy = RetryStrategy.EXPONENTIAL,
      baseDelay = 1000,
      maxDelay = 30000,
      jitter = 0.1,
      multiplier = 2,
    } = options;

    if (maxAttempts < 1) {
      throw new Error('Max attempts must be at least 1');
    }
    if (baseDelay < 0) {
      throw new Error('Base delay must be non-negative');
    }
    if (maxDelay < baseDelay) {
      throw new Error('Max delay must be >= base delay');
    }
    if (jitter < 0 || jitter > 1) {
      throw new Error('Jitter must be between 0 and 1');
    }
    if (multiplier < 1) {
      throw new Error('Multiplier must be at least 1');
    }

    this.#maxAttempts = maxAttempts;
    this.#strategy = strategy;
    this.#baseDelay = baseDelay;
    this.#maxDelay = maxDelay;
    this.#jitter = jitter;
    this.#multiplier = multiplier;
  }

  get maxAttempts() {
    return this.#maxAttempts;
  }
  get strategy() {
    return this.#strategy;
  }
  get baseDelay() {
    return this.#baseDelay;
  }
  get maxDelay() {
    return this.#maxDelay;
  }
  get jitter() {
    return this.#jitter;
  }
  get multiplier() {
    return this.#multiplier;
  }

  /**
   * Calculate delay for an attempt.
   *
   * @param {number} attempt - Attempt number (1-based)
   * @returns {number} Delay in milliseconds
   */
  getDelay(attempt) {
    let delay;

    switch (this.#strategy) {
      case RetryStrategy.IMMEDIATE:
        delay = 0;
        break;
      case RetryStrategy.FIXED:
        delay = this.#baseDelay;
        break;
      case RetryStrategy.LINEAR:
        delay = this.#baseDelay * attempt;
        break;
      case RetryStrategy.EXPONENTIAL:
      default:
        delay = this.#baseDelay * Math.pow(this.#multiplier, attempt - 1);
        break;
    }

    // Apply max delay cap
    delay = Math.min(delay, this.#maxDelay);

    // Apply jitter
    if (this.#jitter > 0) {
      const jitterAmount = delay * this.#jitter;
      delay += (Math.random() * 2 - 1) * jitterAmount;
      delay = Math.max(0, delay);
    }

    return Math.round(delay);
  }
}

/**
 * Retry result with attempt details.
 *
 * @template T
 */
export class RetryResult {
  /** @type {boolean} */
  #success;
  /** @type {T | undefined} */
  #value;
  /** @type {string | undefined} */
  #error;
  /** @type {number} */
  #attempts;
  /** @type {number} */
  #totalDelay;
  /** @type {string[]} */
  #errors;

  /**
   * Create a retry result.
   *
   * @param {Object} data - Result data
   * @param {boolean} data.success - Whether succeeded
   * @param {T} [data.value] - Success value
   * @param {string} [data.error] - Final error
   * @param {number} data.attempts - Number of attempts
   * @param {number} data.totalDelay - Total delay in ms
   * @param {string[]} data.errors - All errors encountered
   */
  constructor(data) {
    this.#success = data.success;
    this.#value = data.value;
    this.#error = data.error;
    this.#attempts = data.attempts;
    this.#totalDelay = data.totalDelay;
    this.#errors = data.errors || [];
  }

  get success() {
    return this.#success;
  }
  get value() {
    return this.#value;
  }
  get error() {
    return this.#error;
  }
  get attempts() {
    return this.#attempts;
  }
  get totalDelay() {
    return this.#totalDelay;
  }
  get errors() {
    return [...this.#errors];
  }

  /**
   * Convert to Result type.
   *
   * @returns {{ ok: true, value: T } | { ok: false, error: string }}
   */
  toResult() {
    if (this.#success) {
      return ok(this.#value);
    }
    return err(this.#error || 'Unknown error');
  }
}

/**
 * Retry executor.
 */
export class Retry {
  /** @type {RetryConfig} */
  #config;

  /**
   * Create a retry executor.
   *
   * @param {RetryConfig | Object} config - Configuration
   */
  constructor(config = {}) {
    this.#config = config instanceof RetryConfig ? config : new RetryConfig(config);
  }

  /**
   * Get configuration.
   *
   * @returns {RetryConfig}
   */
  get config() {
    return this.#config;
  }

  /**
   * Execute a function with retry logic (sync).
   *
   * @template T
   * @param {() => T} fn - Function to execute
   * @returns {RetryResult<T>}
   */
  execute(fn) {
    const errors = [];
    let totalDelay = 0;

    for (let attempt = 1; attempt <= this.#config.maxAttempts; attempt++) {
      try {
        const value = fn();
        return new RetryResult({
          success: true,
          value,
          attempts: attempt,
          totalDelay,
          errors,
        });
      } catch (error) {
        const errorMsg = error instanceof Error ? error.message : String(error);
        errors.push(errorMsg);

        if (attempt < this.#config.maxAttempts) {
          const delay = this.#config.getDelay(attempt);
          totalDelay += delay;
          // Sync sleep (blocking - not recommended for production)
          this.#syncSleep(delay);
        }
      }
    }

    return new RetryResult({
      success: false,
      error: errors[errors.length - 1],
      attempts: this.#config.maxAttempts,
      totalDelay,
      errors,
    });
  }

  /**
   * Execute an async function with retry logic.
   *
   * @template T
   * @param {() => Promise<T>} fn - Async function to execute
   * @returns {Promise<RetryResult<T>>}
   */
  async executeAsync(fn) {
    const errors = [];
    let totalDelay = 0;

    for (let attempt = 1; attempt <= this.#config.maxAttempts; attempt++) {
      try {
        const value = await fn();
        return new RetryResult({
          success: true,
          value,
          attempts: attempt,
          totalDelay,
          errors,
        });
      } catch (error) {
        const errorMsg = error instanceof Error ? error.message : String(error);
        errors.push(errorMsg);

        if (attempt < this.#config.maxAttempts) {
          const delay = this.#config.getDelay(attempt);
          totalDelay += delay;
          await this.#asyncSleep(delay);
        }
      }
    }

    return new RetryResult({
      success: false,
      error: errors[errors.length - 1],
      attempts: this.#config.maxAttempts,
      totalDelay,
      errors,
    });
  }

  /**
   * Execute with custom retry condition.
   *
   * @template T
   * @param {() => Promise<T>} fn - Async function to execute
   * @param {(error: Error) => boolean} shouldRetry - Predicate to check if should retry
   * @returns {Promise<RetryResult<T>>}
   */
  async executeWithCondition(fn, shouldRetry) {
    const errors = [];
    let totalDelay = 0;

    for (let attempt = 1; attempt <= this.#config.maxAttempts; attempt++) {
      try {
        const value = await fn();
        return new RetryResult({
          success: true,
          value,
          attempts: attempt,
          totalDelay,
          errors,
        });
      } catch (error) {
        const err = error instanceof Error ? error : new Error(String(error));
        errors.push(err.message);

        if (attempt < this.#config.maxAttempts && shouldRetry(err)) {
          const delay = this.#config.getDelay(attempt);
          totalDelay += delay;
          await this.#asyncSleep(delay);
        } else if (!shouldRetry(err)) {
          break;
        }
      }
    }

    return new RetryResult({
      success: false,
      error: errors[errors.length - 1],
      attempts: errors.length,
      totalDelay,
      errors,
    });
  }

  /**
   * Sync sleep (blocking).
   *
   * @param {number} ms - Milliseconds
   */
  #syncSleep(ms) {
    const end = Date.now() + ms;
    while (Date.now() < end) {
      // Busy wait
    }
  }

  /**
   * Async sleep.
   *
   * @param {number} ms - Milliseconds
   * @returns {Promise<void>}
   */
  #asyncSleep(ms) {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }
}

/**
 * Safe retry utilities.
 */
export class SafeRetry {
  /**
   * Create a retry executor with default config.
   *
   * @returns {Retry}
   */
  static create() {
    return new Retry();
  }

  /**
   * Create a retry executor with custom config.
   *
   * @param {Object} options - Configuration options
   * @returns {Retry}
   */
  static withConfig(options) {
    return new Retry(new RetryConfig(options));
  }

  /**
   * Create a retry executor with exponential backoff.
   *
   * @param {number} maxAttempts - Maximum attempts
   * @param {number} baseDelay - Base delay in ms
   * @returns {Retry}
   */
  static exponential(maxAttempts, baseDelay) {
    return new Retry(
      new RetryConfig({
        maxAttempts,
        baseDelay,
        strategy: RetryStrategy.EXPONENTIAL,
      }),
    );
  }

  /**
   * Create a retry executor with fixed delay.
   *
   * @param {number} maxAttempts - Maximum attempts
   * @param {number} delay - Fixed delay in ms
   * @returns {Retry}
   */
  static fixed(maxAttempts, delay) {
    return new Retry(
      new RetryConfig({
        maxAttempts,
        baseDelay: delay,
        strategy: RetryStrategy.FIXED,
      }),
    );
  }

  /**
   * Create a retry executor with immediate retry (no delay).
   *
   * @param {number} maxAttempts - Maximum attempts
   * @returns {Retry}
   */
  static immediate(maxAttempts) {
    return new Retry(
      new RetryConfig({
        maxAttempts,
        strategy: RetryStrategy.IMMEDIATE,
      }),
    );
  }

  /**
   * Simple retry helper.
   *
   * @template T
   * @param {() => Promise<T>} fn - Function to retry
   * @param {number} [maxAttempts=3] - Maximum attempts
   * @returns {Promise<RetryResult<T>>}
   */
  static async retry(fn, maxAttempts = 3) {
    return new Retry({ maxAttempts }).executeAsync(fn);
  }
}
