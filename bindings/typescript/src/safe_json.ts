// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export type JsonValue = null | boolean | number | string | JsonValue[] | { [key: string]: JsonValue };

export interface ParseResult<T> {
  ok: true;
  value: T;
}

export interface ParseError {
  ok: false;
  error: string;
}

export type Result<T> = ParseResult<T> | ParseError;

/**
 * Safely parse JSON string.
 */
export function parseJson(input: string): Result<JsonValue> {
  try {
    const value = JSON.parse(input);
    return { ok: true, value };
  } catch (e) {
    return { ok: false, error: e instanceof Error ? e.message : 'Parse error' };
  }
}

/**
 * Safely stringify a value to JSON.
 */
export function stringifyJson(value: unknown, pretty: boolean = false): Result<string> {
  try {
    const result = pretty ? JSON.stringify(value, null, 2) : JSON.stringify(value);
    if (result === undefined) {
      return { ok: false, error: 'Value cannot be stringified' };
    }
    return { ok: true, value: result };
  } catch (e) {
    return { ok: false, error: e instanceof Error ? e.message : 'Stringify error' };
  }
}

/**
 * Get value at path (dot-separated or bracket notation).
 */
export function getPath(obj: JsonValue, path: string): JsonValue | undefined {
  if (path === '' || path === '.') {
    return obj;
  }

  const parts = path
    .replace(/\[(\d+)\]/g, '.$1')
    .split('.')
    .filter((p) => p !== '');

  let current: JsonValue = obj;

  for (const part of parts) {
    if (current === null || typeof current !== 'object') {
      return undefined;
    }

    if (Array.isArray(current)) {
      const index = parseInt(part, 10);
      if (isNaN(index) || index < 0 || index >= current.length) {
        return undefined;
      }
      current = current[index];
    } else {
      if (!(part in current)) {
        return undefined;
      }
      current = current[part];
    }
  }

  return current;
}

/**
 * Set value at path, creating intermediate objects/arrays as needed.
 */
export function setPath(obj: JsonValue, path: string, value: JsonValue): Result<JsonValue> {
  if (path === '' || path === '.') {
    return { ok: true, value };
  }

  if (obj === null || typeof obj !== 'object') {
    return { ok: false, error: 'Cannot set path on non-object' };
  }

  const parts = path
    .replace(/\[(\d+)\]/g, '.$1')
    .split('.')
    .filter((p) => p !== '');

  // Deep clone to avoid mutation
  const result: JsonValue = Array.isArray(obj) ? [...obj] : { ...obj };
  let current: JsonValue = result;

  for (let i = 0; i < parts.length - 1; i++) {
    const part = parts[i];
    const nextPart = parts[i + 1];
    const nextIsArray = /^\d+$/.test(nextPart);

    if (Array.isArray(current)) {
      const index = parseInt(part, 10);
      if (current[index] === undefined || current[index] === null) {
        current[index] = nextIsArray ? [] : {};
      }
      current = current[index];
    } else if (typeof current === 'object' && current !== null) {
      if ((current as Record<string, JsonValue>)[part] === undefined) {
        (current as Record<string, JsonValue>)[part] = nextIsArray ? [] : {};
      }
      current = (current as Record<string, JsonValue>)[part];
    }
  }

  const lastPart = parts[parts.length - 1];
  if (Array.isArray(current)) {
    const index = parseInt(lastPart, 10);
    current[index] = value;
  } else if (typeof current === 'object' && current !== null) {
    (current as Record<string, JsonValue>)[lastPart] = value;
  }

  return { ok: true, value: result };
}

/**
 * Check if value is valid JSON type.
 */
export function isJsonValue(value: unknown): value is JsonValue {
  if (value === null) return true;
  if (typeof value === 'boolean') return true;
  if (typeof value === 'number') return !isNaN(value) && isFinite(value);
  if (typeof value === 'string') return true;
  if (Array.isArray(value)) return value.every(isJsonValue);
  if (typeof value === 'object') {
    return Object.values(value as Record<string, unknown>).every(isJsonValue);
  }
  return false;
}

/**
 * Deep equality check for JSON values.
 */
export function jsonEquals(a: JsonValue, b: JsonValue): boolean {
  if (a === b) return true;
  if (a === null || b === null) return a === b;
  if (typeof a !== typeof b) return false;

  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    return a.every((val, i) => jsonEquals(val, b[i]));
  }

  if (typeof a === 'object' && typeof b === 'object') {
    const keysA = Object.keys(a);
    const keysB = Object.keys(b);
    if (keysA.length !== keysB.length) return false;
    return keysA.every((key) =>
      Object.prototype.hasOwnProperty.call(b, key) &&
      jsonEquals(a[key], (b as Record<string, JsonValue>)[key])
    );
  }

  return a === b;
}

/**
 * Deep merge two JSON objects.
 */
export function mergeJson(base: JsonValue, override: JsonValue): JsonValue {
  if (override === null || typeof override !== 'object' || Array.isArray(override)) {
    return override;
  }

  if (base === null || typeof base !== 'object' || Array.isArray(base)) {
    return override;
  }

  const result: Record<string, JsonValue> = { ...base };

  for (const key of Object.keys(override)) {
    if (
      key in result &&
      typeof result[key] === 'object' &&
      result[key] !== null &&
      !Array.isArray(result[key])
    ) {
      result[key] = mergeJson(result[key], (override as Record<string, JsonValue>)[key]);
    } else {
      result[key] = (override as Record<string, JsonValue>)[key];
    }
  }

  return result;
}

export const SafeJson = {
  parse: parseJson,
  stringify: stringifyJson,
  getPath,
  setPath,
  isJsonValue,
  equals: jsonEquals,
  merge: mergeJson,
};
