// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * WASM module loader and FFI interface.
 */

let wasmInstance: WebAssembly.Instance | null = null;
let wasmMemory: WebAssembly.Memory | null = null;

/**
 * Initialize the Proven WASM module.
 *
 * @param wasmSource - Path to WASM file, URL, or ArrayBuffer
 */
export async function init(
  wasmSource?: string | URL | ArrayBuffer
): Promise<void> {
  if (wasmInstance) {
    return; // Already initialized
  }

  let wasmBytes: ArrayBuffer;

  if (wasmSource instanceof ArrayBuffer) {
    wasmBytes = wasmSource;
  } else if (typeof wasmSource === 'string' || wasmSource instanceof URL) {
    const response = await fetch(wasmSource);
    wasmBytes = await response.arrayBuffer();
  } else {
    // Try default location
    const defaultPath = new URL('../wasm/proven.wasm', import.meta.url);
    const response = await fetch(defaultPath);
    wasmBytes = await response.arrayBuffer();
  }

  const module = await WebAssembly.compile(wasmBytes);

  wasmMemory = new WebAssembly.Memory({ initial: 256, maximum: 65536 });

  const importObject: WebAssembly.Imports = {
    env: {
      memory: wasmMemory,
    },
  };

  wasmInstance = await WebAssembly.instantiate(module, importObject);

  // Initialize the Proven runtime
  const initFn = wasmInstance.exports['proven_init'] as () => number;
  if (initFn) {
    const status = initFn();
    if (status !== 0) {
      throw new Error(`Failed to initialize Proven runtime: ${status}`);
    }
  }
}

/**
 * Check if the WASM module is initialized.
 */
export function isInitialized(): boolean {
  return wasmInstance !== null;
}

/**
 * Get the WASM exports.
 */
export function getExports(): WebAssembly.Exports {
  if (!wasmInstance) {
    throw new Error(
      'Proven WASM not initialized. Call init() first.'
    );
  }
  return wasmInstance.exports;
}

/**
 * Get the WASM memory.
 */
export function getMemory(): WebAssembly.Memory {
  if (!wasmMemory) {
    throw new Error(
      'Proven WASM not initialized. Call init() first.'
    );
  }
  return wasmMemory;
}

/**
 * Encode a string to WASM memory.
 */
export function encodeString(str: string): { ptr: number; len: number } {
  const encoder = new TextEncoder();
  const bytes = encoder.encode(str);
  const memory = getMemory();
  const exports = getExports();

  // Allocate memory
  const allocFn = exports['proven_alloc'] as (size: number) => number;
  const ptr = allocFn(bytes.length);

  // Copy bytes
  const view = new Uint8Array(memory.buffer, ptr, bytes.length);
  view.set(bytes);

  return { ptr, len: bytes.length };
}

/**
 * Decode a string from WASM memory.
 */
export function decodeString(ptr: number, len: number): string {
  const memory = getMemory();
  const bytes = new Uint8Array(memory.buffer, ptr, len);
  const decoder = new TextDecoder();
  return decoder.decode(bytes);
}

/**
 * Free allocated memory.
 */
export function freePtr(ptr: number): void {
  const exports = getExports();
  const freeFn = exports['proven_free'] as (ptr: number) => void;
  if (freeFn) {
    freeFn(ptr);
  }
}
