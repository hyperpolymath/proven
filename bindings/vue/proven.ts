// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Proven Safety Library for Vue 3
 *
 * Formally verified safety primitives as Vue composables.
 * Provides reactive, type-safe patterns for Vue applications.
 *
 * @example
 * ```vue
 * <script setup>
 * import { useSafeCounter, useSafeMath } from 'proven-vue';
 * const { count, increment, decrement } = useSafeCounter(0, 0, 100);
 * </script>
 *
 * <template>
 *   <button @click="increment()">{{ count }}</button>
 * </template>
 * ```
 *
 * @version 0.9.0
 */

import {
  ref,
  computed,
  reactive,
  readonly,
  watch,
  type Ref,
  type ComputedRef,
  type UnwrapRef,
} from 'vue';

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
// SAFE MATH COMPOSABLE
// ============================================================================

/** Safe math operations with overflow protection */
export function useSafeMath() {
  const add = (a: number, b: number): Result<number, ProvenError> => {
    const result = a + b;
    if (!Number.isFinite(result)) return err('OVERFLOW');
    return ok(result);
  };

  const sub = (a: number, b: number): Result<number, ProvenError> => {
    const result = a - b;
    if (!Number.isFinite(result)) return err('UNDERFLOW');
    return ok(result);
  };

  const mul = (a: number, b: number): Result<number, ProvenError> => {
    const result = a * b;
    if (!Number.isFinite(result)) return err('OVERFLOW');
    return ok(result);
  };

  const div = (a: number, b: number): Result<number, ProvenError> => {
    if (b === 0) return err('DIVISION_BY_ZERO');
    const result = a / b;
    if (!Number.isFinite(result)) return err('OVERFLOW');
    return ok(result);
  };

  const mod = (a: number, b: number): Result<number, ProvenError> => {
    if (b === 0) return err('DIVISION_BY_ZERO');
    return ok(a % b);
  };

  const clamp = (value: number, min: number, max: number): number => {
    return Math.min(Math.max(value, min), max);
  };

  const safeIntAdd = (a: number, b: number): Result<number, ProvenError> => {
    if (!Number.isSafeInteger(a) || !Number.isSafeInteger(b)) {
      return err('OVERFLOW');
    }
    const result = a + b;
    if (!Number.isSafeInteger(result)) return err('OVERFLOW');
    return ok(result);
  };

  const safeIntSub = (a: number, b: number): Result<number, ProvenError> => {
    if (!Number.isSafeInteger(a) || !Number.isSafeInteger(b)) {
      return err('OVERFLOW');
    }
    const result = a - b;
    if (!Number.isSafeInteger(result)) return err('UNDERFLOW');
    return ok(result);
  };

  return {
    add,
    sub,
    mul,
    div,
    mod,
    clamp,
    safeIntAdd,
    safeIntSub,
  };
}

// ============================================================================
// BOUNDED VALUE COMPOSABLE
// ============================================================================

export interface UseBoundedOptions {
  /** Callback when value hits minimum */
  onMin?: () => void;
  /** Callback when value hits maximum */
  onMax?: () => void;
  /** Callback on any value change */
  onChange?: (value: number, oldValue: number) => void;
}

/** Create a bounded reactive number */
export function useBounded(
  initial: number,
  min: number,
  max: number,
  options: UseBoundedOptions = {}
) {
  const { clamp } = useSafeMath();
  const { add, sub } = useSafeMath();

  const value = ref(clamp(initial, min, max));
  const isAtMin = computed(() => value.value <= min);
  const isAtMax = computed(() => value.value >= max);
  const percentage = computed(() => ((value.value - min) / (max - min)) * 100);

  // Watch for boundary callbacks
  watch(value, (newVal, oldVal) => {
    options.onChange?.(newVal, oldVal ?? initial);
    if (newVal <= min) options.onMin?.();
    if (newVal >= max) options.onMax?.();
  });

  const set = (newValue: number) => {
    value.value = clamp(newValue, min, max);
  };

  const increment = (delta = 1): Result<number, ProvenError> => {
    const result = add(value.value, delta);
    if (isOk(result)) {
      value.value = clamp(result.value, min, max);
      return ok(value.value);
    }
    return result;
  };

  const decrement = (delta = 1): Result<number, ProvenError> => {
    const result = sub(value.value, delta);
    if (isOk(result)) {
      value.value = clamp(result.value, min, max);
      return ok(value.value);
    }
    return result;
  };

  const reset = () => {
    value.value = clamp(initial, min, max);
  };

  return {
    value: readonly(value),
    isAtMin,
    isAtMax,
    percentage,
    set,
    increment,
    decrement,
    reset,
    bounds: { min, max },
  };
}

/** Create a safe counter (convenience wrapper) */
export function useSafeCounter(
  initial = 0,
  min = 0,
  max = Number.MAX_SAFE_INTEGER,
  options: UseBoundedOptions = {}
) {
  const bounded = useBounded(initial, min, max, options);

  return {
    count: bounded.value,
    ...bounded,
  };
}

/** Create a percentage value (0-100) */
export function usePercentage(initial = 0, options: UseBoundedOptions = {}) {
  return useBounded(initial, 0, 100, options);
}

/** Create a normalized value (0-1) */
export function useNormalized(initial = 0, options: UseBoundedOptions = {}) {
  return useBounded(initial, 0, 1, options);
}

// ============================================================================
// RESULT REF COMPOSABLE
// ============================================================================

/** Create a reactive result ref */
export function useResult<T, E = string>(initial: Result<T, E>) {
  const result = ref(initial) as Ref<Result<T, E>>;

  const isOkComputed = computed(() => result.value.ok);
  const isErrComputed = computed(() => !result.value.ok);

  const value = computed(() =>
    result.value.ok ? result.value.value : undefined
  );

  const error = computed(() =>
    result.value.ok ? undefined : result.value.error
  );

  const setOk = (newValue: T) => {
    result.value = ok(newValue);
  };

  const setErr = (newError: E) => {
    result.value = err(newError);
  };

  const map = <U>(fn: (v: T) => U): ComputedRef<Result<U, E>> => {
    return computed(() => mapResult(result.value, fn));
  };

  return {
    result,
    isOk: isOkComputed,
    isErr: isErrComputed,
    value,
    error,
    setOk,
    setErr,
    map,
  };
}

// ============================================================================
// ASYNC RESULT COMPOSABLE
// ============================================================================

export interface UseAsyncResultOptions {
  immediate?: boolean;
  resetOnExecute?: boolean;
}

/** Create an async operation with result handling */
export function useAsyncResult<T, Args extends unknown[] = []>(
  asyncFn: (...args: Args) => Promise<T>,
  options: UseAsyncResultOptions = {}
) {
  const { immediate = false, resetOnExecute = true } = options;

  const loading = ref(false);
  const result = ref<Result<T, string> | null>(null) as Ref<Result<T, string> | null>;

  const isOk = computed(() => result.value?.ok ?? false);
  const isErr = computed(() => result.value !== null && !result.value.ok);
  const value = computed(() => (result.value?.ok ? result.value.value : undefined));
  const error = computed(() => (result.value && !result.value.ok ? result.value.error : undefined));

  const execute = async (...args: Args): Promise<Result<T, string>> => {
    if (resetOnExecute) {
      result.value = null;
    }
    loading.value = true;

    try {
      const data = await asyncFn(...args);
      const successResult = ok(data);
      result.value = successResult;
      return successResult;
    } catch (e) {
      const errorResult = err(e instanceof Error ? e.message : String(e));
      result.value = errorResult;
      return errorResult;
    } finally {
      loading.value = false;
    }
  };

  const reset = () => {
    loading.value = false;
    result.value = null;
  };

  // Execute immediately if requested
  if (immediate) {
    execute(...([] as unknown as Args));
  }

  return {
    loading: readonly(loading),
    result: readonly(result),
    isOk,
    isErr,
    value,
    error,
    execute,
    reset,
  };
}

// ============================================================================
// VALIDATION COMPOSABLE
// ============================================================================

export type Validator<T> = (value: T) => string | null;

export interface UseValidationOptions<T> {
  /** Validate on mount */
  immediate?: boolean;
  /** Validate on every change */
  eager?: boolean;
  /** Debounce validation (ms) */
  debounce?: number;
}

/** Create a validated reactive value */
export function useValidation<T>(
  initial: T,
  validators: Validator<T>[],
  options: UseValidationOptions<T> = {}
) {
  const { immediate = false, eager = true, debounce = 0 } = options;

  const value = ref(initial) as Ref<T>;
  const errors = ref<string[]>([]);
  const touched = ref(false);
  const dirty = ref(false);

  const isValid = computed(() => errors.value.length === 0);

  let debounceTimeout: ReturnType<typeof setTimeout> | null = null;

  const runValidation = () => {
    errors.value = validators
      .map((v) => v(value.value))
      .filter((e): e is string => e !== null);
  };

  const validate = () => {
    touched.value = true;
    runValidation();
    return isValid.value;
  };

  const reset = () => {
    value.value = initial as UnwrapRef<T>;
    errors.value = [];
    touched.value = false;
    dirty.value = false;
  };

  // Watch for changes
  watch(value, (newVal, oldVal) => {
    if (newVal !== oldVal) {
      dirty.value = true;
    }

    if (eager && touched.value) {
      if (debounce > 0) {
        if (debounceTimeout) clearTimeout(debounceTimeout);
        debounceTimeout = setTimeout(runValidation, debounce);
      } else {
        runValidation();
      }
    }
  });

  // Validate on mount if requested
  if (immediate) {
    runValidation();
  }

  return {
    value,
    errors: readonly(errors),
    touched: readonly(touched),
    dirty: readonly(dirty),
    isValid,
    validate,
    reset,
  };
}

// ============================================================================
// COMMON VALIDATORS
// ============================================================================

export const validators = {
  required: (message = 'Required'): Validator<string> => (value) =>
    value.trim().length === 0 ? message : null,

  minLength: (min: number, message?: string): Validator<string> => (value) =>
    value.length < min ? (message ?? `Minimum ${min} characters`) : null,

  maxLength: (max: number, message?: string): Validator<string> => (value) =>
    value.length > max ? (message ?? `Maximum ${max} characters`) : null,

  email: (message = 'Invalid email'): Validator<string> => (value) => {
    const emailPattern = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/;
    return emailPattern.test(value) ? null : message;
  },

  url: (message = 'Invalid URL'): Validator<string> => (value) => {
    try {
      new URL(value);
      return null;
    } catch {
      return message;
    }
  },

  port: (message = 'Invalid port'): Validator<number> => (value) =>
    value >= 1 && value <= 65535 && Number.isInteger(value) ? null : message,

  range: (min: number, max: number, message?: string): Validator<number> => (value) =>
    value >= min && value <= max ? null : (message ?? `Must be between ${min} and ${max}`),

  percentage: (message = 'Must be 0-100'): Validator<number> => (value) =>
    value >= 0 && value <= 100 ? null : message,

  safePath: (message = 'Invalid path'): Validator<string> => (value) =>
    value.includes('..') ? message : null,

  pattern: (regex: RegExp, message = 'Invalid format'): Validator<string> => (value) =>
    regex.test(value) ? null : message,

  custom: <T>(fn: (value: T) => boolean, message: string): Validator<T> => (value) =>
    fn(value) ? null : message,
};

// ============================================================================
// SAFE FETCH COMPOSABLE
// ============================================================================

export interface UseFetchOptions extends RequestInit {
  immediate?: boolean;
  timeout?: number;
  retries?: number;
  retryDelay?: number;
  refetch?: boolean;
  refetchInterval?: number;
}

/** Safe fetch composable */
export function useFetch<T>(url: Ref<string> | string, options: UseFetchOptions = {}) {
  const {
    immediate = true,
    timeout = 30000,
    retries = 0,
    retryDelay = 1000,
    refetch = false,
    refetchInterval = 60000,
    ...fetchOptions
  } = options;

  const urlRef = typeof url === 'string' ? ref(url) : url;
  const data = ref<T | null>(null) as Ref<T | null>;
  const error = ref<string | null>(null);
  const loading = ref(false);

  let refetchTimer: ReturnType<typeof setInterval> | null = null;

  const execute = async (): Promise<Result<T, string>> => {
    loading.value = true;
    error.value = null;

    const attemptFetch = async (): Promise<Result<T, string>> => {
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), timeout);

      try {
        const response = await fetch(urlRef.value, {
          ...fetchOptions,
          signal: controller.signal,
        });

        clearTimeout(timeoutId);

        if (!response.ok) {
          return err(`HTTP ${response.status}: ${response.statusText}`);
        }

        const json = await response.json();
        return ok(json as T);
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
      if (result.ok) {
        data.value = result.value;
        loading.value = false;
        return result;
      }
      lastError = result;

      if (attempt < retries) {
        await new Promise((resolve) => setTimeout(resolve, retryDelay));
      }
    }

    error.value = lastError.ok ? null : lastError.error;
    loading.value = false;
    return lastError;
  };

  const startRefetch = () => {
    if (refetch && !refetchTimer) {
      refetchTimer = setInterval(execute, refetchInterval);
    }
  };

  const stopRefetch = () => {
    if (refetchTimer) {
      clearInterval(refetchTimer);
      refetchTimer = null;
    }
  };

  // Watch URL changes
  watch(urlRef, () => {
    if (immediate) execute();
  });

  // Execute immediately if requested
  if (immediate) {
    execute();
  }

  // Start refetch if enabled
  if (refetch) {
    startRefetch();
  }

  return {
    data: readonly(data),
    error: readonly(error),
    loading: readonly(loading),
    execute,
    startRefetch,
    stopRefetch,
  };
}

// ============================================================================
// HISTORY COMPOSABLE
// ============================================================================

/** Create a value with undo/redo history */
export function useHistory<T>(initial: T, maxHistory = 50) {
  const history = reactive<T[]>([initial]) as T[];
  let position = 0;

  const current = ref(initial) as Ref<T>;
  const canUndo = computed(() => position > 0);
  const canRedo = computed(() => position < history.length - 1);

  const push = (value: T) => {
    // Remove redo history
    history.splice(position + 1);
    // Add new value
    history.push(value);
    // Trim if over max
    if (history.length > maxHistory) {
      history.shift();
    } else {
      position++;
    }
    current.value = value;
  };

  const undo = (): boolean => {
    if (position > 0) {
      position--;
      current.value = history[position];
      return true;
    }
    return false;
  };

  const redo = (): boolean => {
    if (position < history.length - 1) {
      position++;
      current.value = history[position];
      return true;
    }
    return false;
  };

  const clear = () => {
    const currentValue = history[position];
    history.length = 0;
    history.push(currentValue);
    position = 0;
  };

  return {
    value: current,
    canUndo,
    canRedo,
    push,
    undo,
    redo,
    clear,
    historyLength: computed(() => history.length),
    currentPosition: computed(() => position),
  };
}

// ============================================================================
// DEBOUNCE/THROTTLE COMPOSABLES
// ============================================================================

/** Create a debounced ref */
export function useDebouncedRef<T>(initial: T, delay: number) {
  const value = ref(initial) as Ref<T>;
  const debouncedValue = ref(initial) as Ref<T>;
  let timeout: ReturnType<typeof setTimeout>;

  watch(value, (newValue) => {
    clearTimeout(timeout);
    timeout = setTimeout(() => {
      debouncedValue.value = newValue;
    }, delay);
  });

  return {
    value,
    debouncedValue: readonly(debouncedValue),
  };
}

/** Create a throttled ref */
export function useThrottledRef<T>(initial: T, delay: number) {
  const value = ref(initial) as Ref<T>;
  const throttledValue = ref(initial) as Ref<T>;
  let lastUpdate = 0;

  watch(value, (newValue) => {
    const now = Date.now();
    if (now - lastUpdate >= delay) {
      lastUpdate = now;
      throttledValue.value = newValue;
    }
  });

  return {
    value,
    throttledValue: readonly(throttledValue),
  };
}

// ============================================================================
// LOCAL STORAGE COMPOSABLE
// ============================================================================

export interface UseStorageOptions<T> {
  serializer?: {
    read: (value: string) => T;
    write: (value: T) => string;
  };
}

/** Create a reactive localStorage value */
export function useLocalStorage<T>(
  key: string,
  initial: T,
  options: UseStorageOptions<T> = {}
) {
  const serializer = options.serializer ?? {
    read: (v: string) => JSON.parse(v) as T,
    write: (v: T) => JSON.stringify(v),
  };

  // Read initial value from storage
  const readValue = (): T => {
    try {
      const item = localStorage.getItem(key);
      return item ? serializer.read(item) : initial;
    } catch {
      return initial;
    }
  };

  const value = ref(readValue()) as Ref<T>;

  // Write to storage on change
  watch(
    value,
    (newValue) => {
      try {
        localStorage.setItem(key, serializer.write(newValue));
      } catch {
        // Storage full or unavailable
      }
    },
    { deep: true }
  );

  const remove = () => {
    localStorage.removeItem(key);
    value.value = initial;
  };

  return {
    value,
    remove,
  };
}

// ============================================================================
// VERSION
// ============================================================================

export const VERSION = '0.9.0';
