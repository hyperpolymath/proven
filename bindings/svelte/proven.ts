// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Proven Safety Library for Svelte
 *
 * Formally verified safety primitives as Svelte stores and utilities.
 * Provides reactive, type-safe patterns for Svelte applications.
 *
 * @example
 * ```svelte
 * <script>
 *   import { safeCounter, safeMath } from 'proven-svelte';
 *   const counter = safeCounter(0, 0, 100);
 * </script>
 *
 * <button on:click={() => counter.increment(1)}>
 *   Count: {$counter}
 * </button>
 * ```
 *
 * @version 0.9.0
 */

import { writable, derived, readable, type Writable, type Readable } from 'svelte/store';

// ============================================================================
// RESULT TYPE
// ============================================================================

/** Result type for operations that can fail */
export type Result<T, E = string> =
  | { ok: true; value: T }
  | { ok: false; error: E };

/** Create a success result */
export function ok<T>(value: T): Result<T, never> {
  return { ok: true, value };
}

/** Create an error result */
export function err<E>(error: E): Result<never, E> {
  return { ok: false, error };
}

/** Check if result is ok */
export function isOk<T, E>(result: Result<T, E>): result is { ok: true; value: T } {
  return result.ok;
}

/** Check if result is error */
export function isErr<T, E>(result: Result<T, E>): result is { ok: false; error: E } {
  return !result.ok;
}

/** Unwrap result or throw */
export function unwrap<T, E>(result: Result<T, E>): T {
  if (result.ok) return result.value;
  throw new Error(String(result.error));
}

/** Unwrap result with default */
export function unwrapOr<T, E>(result: Result<T, E>, defaultValue: T): T {
  return result.ok ? result.value : defaultValue;
}

/** Map over successful result */
export function mapResult<T, U, E>(result: Result<T, E>, fn: (value: T) => U): Result<U, E> {
  return result.ok ? ok(fn(result.value)) : result;
}

// ============================================================================
// ERROR TYPES
// ============================================================================

export type ProvenError =
  | 'OVERFLOW'
  | 'UNDERFLOW'
  | 'DIVISION_BY_ZERO'
  | 'OUT_OF_BOUNDS'
  | 'INVALID_PORT'
  | 'INVALID_PERCENTAGE'
  | 'INVALID_EMAIL'
  | 'INVALID_URL'
  | 'PATH_TRAVERSAL';

// ============================================================================
// SAFE MATH
// ============================================================================

/** Safe math operations with overflow protection */
export const safeMath = {
  /** Safe addition */
  add(a: number, b: number): Result<number, ProvenError> {
    const result = a + b;
    if (!Number.isFinite(result)) return err('OVERFLOW');
    return ok(result);
  },

  /** Safe subtraction */
  sub(a: number, b: number): Result<number, ProvenError> {
    const result = a - b;
    if (!Number.isFinite(result)) return err('UNDERFLOW');
    return ok(result);
  },

  /** Safe multiplication */
  mul(a: number, b: number): Result<number, ProvenError> {
    const result = a * b;
    if (!Number.isFinite(result)) return err('OVERFLOW');
    return ok(result);
  },

  /** Safe division */
  div(a: number, b: number): Result<number, ProvenError> {
    if (b === 0) return err('DIVISION_BY_ZERO');
    const result = a / b;
    if (!Number.isFinite(result)) return err('OVERFLOW');
    return ok(result);
  },

  /** Safe modulo */
  mod(a: number, b: number): Result<number, ProvenError> {
    if (b === 0) return err('DIVISION_BY_ZERO');
    return ok(a % b);
  },

  /** Safe integer addition (53-bit) */
  safeIntAdd(a: number, b: number): Result<number, ProvenError> {
    if (!Number.isSafeInteger(a) || !Number.isSafeInteger(b)) {
      return err('OVERFLOW');
    }
    const result = a + b;
    if (!Number.isSafeInteger(result)) return err('OVERFLOW');
    return ok(result);
  },

  /** Safe integer subtraction (53-bit) */
  safeIntSub(a: number, b: number): Result<number, ProvenError> {
    if (!Number.isSafeInteger(a) || !Number.isSafeInteger(b)) {
      return err('OVERFLOW');
    }
    const result = a - b;
    if (!Number.isSafeInteger(result)) return err('UNDERFLOW');
    return ok(result);
  },

  /** Clamp value to range */
  clamp(value: number, min: number, max: number): number {
    return Math.min(Math.max(value, min), max);
  },
};

// ============================================================================
// BOUNDED STORES
// ============================================================================

export interface BoundedStore<T> extends Writable<T> {
  /** Get current bounds */
  getBounds(): { min: T; max: T };
  /** Check if value is at minimum */
  isAtMin(): boolean;
  /** Check if value is at maximum */
  isAtMax(): boolean;
}

/** Create a bounded number store */
export function boundedNumber(
  initial: number,
  min: number,
  max: number
): BoundedStore<number> & {
  increment: (delta?: number) => Result<number, ProvenError>;
  decrement: (delta?: number) => Result<number, ProvenError>;
} {
  const clampedInitial = safeMath.clamp(initial, min, max);
  const { subscribe, set, update } = writable(clampedInitial);

  let currentValue = clampedInitial;

  return {
    subscribe,
    set: (value: number) => {
      currentValue = safeMath.clamp(value, min, max);
      set(currentValue);
    },
    update: (fn: (value: number) => number) => {
      update((v) => {
        currentValue = safeMath.clamp(fn(v), min, max);
        return currentValue;
      });
    },
    getBounds: () => ({ min, max }),
    isAtMin: () => currentValue <= min,
    isAtMax: () => currentValue >= max,
    increment: (delta = 1) => {
      const result = safeMath.add(currentValue, delta);
      if (isOk(result)) {
        currentValue = safeMath.clamp(result.value, min, max);
        set(currentValue);
        return ok(currentValue);
      }
      return result;
    },
    decrement: (delta = 1) => {
      const result = safeMath.sub(currentValue, delta);
      if (isOk(result)) {
        currentValue = safeMath.clamp(result.value, min, max);
        set(currentValue);
        return ok(currentValue);
      }
      return result;
    },
  };
}

/** Create a safe counter store (0 to max) */
export function safeCounter(initial = 0, min = 0, max = Number.MAX_SAFE_INTEGER) {
  return boundedNumber(initial, min, max);
}

/** Create a percentage store (0-100) */
export function percentageStore(initial = 0) {
  return boundedNumber(initial, 0, 100);
}

/** Create a normalized store (0-1) */
export function normalizedStore(initial = 0) {
  return boundedNumber(initial, 0, 1);
}

// ============================================================================
// RESULT STORES
// ============================================================================

export interface ResultStore<T, E = string> extends Readable<Result<T, E>> {
  /** Get the current value if ok */
  getValue(): T | undefined;
  /** Check if current state is ok */
  isOk(): boolean;
  /** Map over the value */
  map<U>(fn: (value: T) => U): ResultStore<U, E>;
}

/** Create a writable result store */
export function resultStore<T, E = string>(
  initial: Result<T, E>
): ResultStore<T, E> & Writable<Result<T, E>> {
  const { subscribe, set, update } = writable(initial);
  let current = initial;

  const store = {
    subscribe,
    set: (value: Result<T, E>) => {
      current = value;
      set(value);
    },
    update: (fn: (value: Result<T, E>) => Result<T, E>) => {
      update((v) => {
        current = fn(v);
        return current;
      });
    },
    getValue: () => (current.ok ? current.value : undefined),
    isOk: () => current.ok,
    map: <U>(fn: (value: T) => U): ResultStore<U, E> => {
      return derivedResultStore(store, fn);
    },
  };

  return store;
}

/** Create a derived result store */
function derivedResultStore<T, U, E>(
  source: Readable<Result<T, E>>,
  fn: (value: T) => U
): ResultStore<U, E> {
  let current: Result<U, E> = err('uninitialized' as E);

  const { subscribe } = derived(source, ($source) => {
    current = mapResult($source, fn);
    return current;
  });

  return {
    subscribe,
    getValue: () => (current.ok ? current.value : undefined),
    isOk: () => current.ok,
    map: <V>(fn2: (value: U) => V): ResultStore<V, E> => {
      return derivedResultStore({ subscribe }, fn2);
    },
  };
}

// ============================================================================
// ASYNC RESULT STORES
// ============================================================================

export interface AsyncResultStore<T, E = string> extends Readable<{
  loading: boolean;
  result: Result<T, E> | null;
}> {
  /** Execute the async operation */
  execute: (...args: unknown[]) => Promise<Result<T, E>>;
  /** Reset to initial state */
  reset: () => void;
}

/** Create an async result store */
export function asyncResultStore<T, E = string>(
  asyncFn: (...args: unknown[]) => Promise<T>
): AsyncResultStore<T, E> {
  const { subscribe, set } = writable<{
    loading: boolean;
    result: Result<T, E> | null;
  }>({
    loading: false,
    result: null,
  });

  return {
    subscribe,
    execute: async (...args: unknown[]) => {
      set({ loading: true, result: null });
      try {
        const value = await asyncFn(...args);
        const result = ok(value) as Result<T, E>;
        set({ loading: false, result });
        return result;
      } catch (e) {
        const result = err(e instanceof Error ? e.message : String(e)) as Result<T, E>;
        set({ loading: false, result });
        return result;
      }
    },
    reset: () => {
      set({ loading: false, result: null });
    },
  };
}

// ============================================================================
// VALIDATION STORES
// ============================================================================

export interface ValidatedStore<T> extends Writable<T> {
  /** Whether current value is valid */
  readonly valid: Readable<boolean>;
  /** Current validation errors */
  readonly errors: Readable<string[]>;
  /** Validate current value */
  validate(): boolean;
}

type Validator<T> = (value: T) => string | null;

/** Create a validated store */
export function validatedStore<T>(
  initial: T,
  validators: Validator<T>[]
): ValidatedStore<T> {
  const { subscribe, set, update } = writable(initial);
  const errorsStore = writable<string[]>([]);
  const validStore = derived(errorsStore, ($errors) => $errors.length === 0);

  let currentValue = initial;

  const runValidation = (value: T): string[] => {
    return validators
      .map((v) => v(value))
      .filter((e): e is string => e !== null);
  };

  // Initial validation
  errorsStore.set(runValidation(initial));

  return {
    subscribe,
    set: (value: T) => {
      currentValue = value;
      errorsStore.set(runValidation(value));
      set(value);
    },
    update: (fn: (value: T) => T) => {
      update((v) => {
        currentValue = fn(v);
        errorsStore.set(runValidation(currentValue));
        return currentValue;
      });
    },
    valid: validStore,
    errors: { subscribe: errorsStore.subscribe },
    validate: () => {
      const errors = runValidation(currentValue);
      errorsStore.set(errors);
      return errors.length === 0;
    },
  };
}

// ============================================================================
// COMMON VALIDATORS
// ============================================================================

export const validators = {
  /** Require non-empty string */
  required: (message = 'Required'): Validator<string> => (value) =>
    value.trim().length === 0 ? message : null,

  /** Minimum length */
  minLength: (min: number, message?: string): Validator<string> => (value) =>
    value.length < min ? (message ?? `Minimum ${min} characters`) : null,

  /** Maximum length */
  maxLength: (max: number, message?: string): Validator<string> => (value) =>
    value.length > max ? (message ?? `Maximum ${max} characters`) : null,

  /** Valid email format */
  email: (message = 'Invalid email'): Validator<string> => (value) => {
    const emailPattern = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/;
    return emailPattern.test(value) ? null : message;
  },

  /** Valid URL format */
  url: (message = 'Invalid URL'): Validator<string> => (value) => {
    try {
      new URL(value);
      return null;
    } catch {
      return message;
    }
  },

  /** Valid port number */
  port: (message = 'Invalid port'): Validator<number> => (value) =>
    value >= 1 && value <= 65535 && Number.isInteger(value) ? null : message,

  /** Value in range */
  range: (min: number, max: number, message?: string): Validator<number> => (value) =>
    value >= min && value <= max ? null : (message ?? `Must be between ${min} and ${max}`),

  /** Percentage (0-100) */
  percentage: (message = 'Must be 0-100'): Validator<number> => (value) =>
    value >= 0 && value <= 100 ? null : message,

  /** Safe path (no traversal) */
  safePath: (message = 'Invalid path'): Validator<string> => (value) =>
    value.includes('..') ? message : null,

  /** Pattern match */
  pattern: (regex: RegExp, message = 'Invalid format'): Validator<string> => (value) =>
    regex.test(value) ? null : message,

  /** Custom validator */
  custom: <T>(fn: (value: T) => boolean, message: string): Validator<T> => (value) =>
    fn(value) ? null : message,
};

// ============================================================================
// FORM STORES
// ============================================================================

export interface FormField<T> {
  value: Writable<T>;
  valid: Readable<boolean>;
  errors: Readable<string[]>;
  touched: Writable<boolean>;
  validate: () => boolean;
}

export interface FormStore<T extends Record<string, unknown>> {
  fields: { [K in keyof T]: FormField<T[K]> };
  values: Readable<T>;
  valid: Readable<boolean>;
  touched: Readable<boolean>;
  submit: (handler: (values: T) => void | Promise<void>) => Promise<void>;
  reset: () => void;
}

/** Create a form field */
export function formField<T>(
  initial: T,
  fieldValidators: Validator<T>[] = []
): FormField<T> {
  const store = validatedStore(initial, fieldValidators);
  const touched = writable(false);

  return {
    value: store,
    valid: store.valid,
    errors: store.errors,
    touched,
    validate: () => {
      touched.set(true);
      return store.validate();
    },
  };
}

// ============================================================================
// SAFE FETCH
// ============================================================================

export interface FetchOptions extends RequestInit {
  timeout?: number;
  retries?: number;
  retryDelay?: number;
}

/** Safe fetch with timeout and retry support */
export async function safeFetch<T>(
  url: string,
  options: FetchOptions = {}
): Promise<Result<T, string>> {
  const { timeout = 30000, retries = 0, retryDelay = 1000, ...fetchOptions } = options;

  const attemptFetch = async (): Promise<Result<T, string>> => {
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), timeout);

    try {
      const response = await fetch(url, {
        ...fetchOptions,
        signal: controller.signal,
      });

      clearTimeout(timeoutId);

      if (!response.ok) {
        return err(`HTTP ${response.status}: ${response.statusText}`);
      }

      const data = await response.json();
      return ok(data as T);
    } catch (e) {
      clearTimeout(timeoutId);
      if (e instanceof Error) {
        if (e.name === 'AbortError') {
          return err('Request timeout');
        }
        return err(e.message);
      }
      return err('Unknown error');
    }
  };

  let lastError: Result<T, string> = err('No attempts made');

  for (let attempt = 0; attempt <= retries; attempt++) {
    const result = await attemptFetch();
    if (result.ok) return result;
    lastError = result;

    if (attempt < retries) {
      await new Promise((resolve) => setTimeout(resolve, retryDelay));
    }
  }

  return lastError;
}

/** Create a fetch store */
export function fetchStore<T>(url: string, options: FetchOptions = {}) {
  return asyncResultStore<T>(() =>
    safeFetch<T>(url, options).then((r) => {
      if (r.ok) return r.value;
      throw new Error(r.error);
    })
  );
}

// ============================================================================
// DEBOUNCED STORES
// ============================================================================

/** Create a debounced store */
export function debouncedStore<T>(
  source: Readable<T>,
  delay: number
): Readable<T> {
  let timeout: ReturnType<typeof setTimeout>;
  let initialValue: T;

  const unsubscribe = source.subscribe((value) => {
    initialValue = value;
  });
  unsubscribe();

  return readable(initialValue!, (set) => {
    return source.subscribe((value) => {
      clearTimeout(timeout);
      timeout = setTimeout(() => set(value), delay);
    });
  });
}

/** Create a throttled store */
export function throttledStore<T>(
  source: Readable<T>,
  delay: number
): Readable<T> {
  let lastUpdate = 0;
  let initialValue: T;

  const unsubscribe = source.subscribe((value) => {
    initialValue = value;
  });
  unsubscribe();

  return readable(initialValue!, (set) => {
    return source.subscribe((value) => {
      const now = Date.now();
      if (now - lastUpdate >= delay) {
        lastUpdate = now;
        set(value);
      }
    });
  });
}

// ============================================================================
// HISTORY STORE
// ============================================================================

export interface HistoryStore<T> extends Writable<T> {
  /** Undo last change */
  undo: () => boolean;
  /** Redo last undone change */
  redo: () => boolean;
  /** Whether undo is available */
  canUndo: Readable<boolean>;
  /** Whether redo is available */
  canRedo: Readable<boolean>;
  /** Clear history */
  clearHistory: () => void;
}

/** Create a store with undo/redo history */
export function historyStore<T>(
  initial: T,
  maxHistory = 50
): HistoryStore<T> {
  const history: T[] = [initial];
  let position = 0;

  const { subscribe, set } = writable(initial);
  const canUndoStore = writable(false);
  const canRedoStore = writable(false);

  const updateCanFlags = () => {
    canUndoStore.set(position > 0);
    canRedoStore.set(position < history.length - 1);
  };

  return {
    subscribe,
    set: (value: T) => {
      // Remove any redo history
      history.splice(position + 1);
      // Add new value
      history.push(value);
      // Trim if over max
      if (history.length > maxHistory) {
        history.shift();
      } else {
        position++;
      }
      updateCanFlags();
      set(value);
    },
    update: (fn: (value: T) => T) => {
      const newValue = fn(history[position]);
      history.splice(position + 1);
      history.push(newValue);
      if (history.length > maxHistory) {
        history.shift();
      } else {
        position++;
      }
      updateCanFlags();
      set(newValue);
    },
    undo: () => {
      if (position > 0) {
        position--;
        updateCanFlags();
        set(history[position]);
        return true;
      }
      return false;
    },
    redo: () => {
      if (position < history.length - 1) {
        position++;
        updateCanFlags();
        set(history[position]);
        return true;
      }
      return false;
    },
    canUndo: { subscribe: canUndoStore.subscribe },
    canRedo: { subscribe: canRedoStore.subscribe },
    clearHistory: () => {
      const current = history[position];
      history.length = 0;
      history.push(current);
      position = 0;
      updateCanFlags();
    },
  };
}

// ============================================================================
// VERSION
// ============================================================================

export const VERSION = '0.9.0';

export const version = readable(VERSION);
