;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven Safety Primitives for WebAssembly Text Format (WAT)
;;
;; Low-level safe operations that can be imported into any WASM module.
;; These provide the foundation for safe computation in WebAssembly.

(module $proven
  ;; ============================================================================
  ;; MEMORY
  ;; ============================================================================

  ;; 1 page = 64KB, we allocate 1 page for safe operations
  (memory (export "memory") 1)

  ;; ============================================================================
  ;; RESULT TYPE ENCODING
  ;; ============================================================================
  ;; We encode Result<T, E> as (i32, i64):
  ;;   - First i32: 0 = Ok, non-zero = error code
  ;;   - Second i64: value (for Ok) or error details (for Err)

  ;; ============================================================================
  ;; SAFE INTEGER ARITHMETIC
  ;; ============================================================================

  ;; Safe addition with overflow detection
  ;; Returns (error_code, result) where error_code = 0 means success
  (func $safe_add_i32 (export "safe_add_i32")
    (param $a i32) (param $b i32)
    (result i32 i64)
    (local $result i64)
    (local $sum i64)

    ;; Compute sum as i64 to detect overflow
    (local.set $sum
      (i64.add
        (i64.extend_i32_s (local.get $a))
        (i64.extend_i32_s (local.get $b))))

    ;; Check if result fits in i32 range
    (if (result i32 i64)
      (i32.and
        (i64.ge_s (local.get $sum) (i64.const -2147483648))
        (i64.le_s (local.get $sum) (i64.const 2147483647)))
      (then
        ;; Success: return (0, sum)
        (i32.const 0)
        (local.get $sum))
      (else
        ;; Overflow: return (1, 0)
        (i32.const 1)
        (i64.const 0))))

  ;; Safe addition for i64
  (func $safe_add_i64 (export "safe_add_i64")
    (param $a i64) (param $b i64)
    (result i32 i64)
    (local $result i64)

    ;; For i64, we check signs to detect overflow
    (local.set $result (i64.add (local.get $a) (local.get $b)))

    ;; Overflow if: same signs for a,b but different sign for result
    (if (result i32 i64)
      (i32.and
        (i64.eq
          (i64.shr_s (local.get $a) (i64.const 63))
          (i64.shr_s (local.get $b) (i64.const 63)))
        (i64.ne
          (i64.shr_s (local.get $a) (i64.const 63))
          (i64.shr_s (local.get $result) (i64.const 63))))
      (then
        ;; Overflow
        (i32.const 1)
        (i64.const 0))
      (else
        ;; Success
        (i32.const 0)
        (local.get $result))))

  ;; Safe subtraction with overflow detection
  (func $safe_sub_i32 (export "safe_sub_i32")
    (param $a i32) (param $b i32)
    (result i32 i64)
    (local $diff i64)

    (local.set $diff
      (i64.sub
        (i64.extend_i32_s (local.get $a))
        (i64.extend_i32_s (local.get $b))))

    (if (result i32 i64)
      (i32.and
        (i64.ge_s (local.get $diff) (i64.const -2147483648))
        (i64.le_s (local.get $diff) (i64.const 2147483647)))
      (then
        (i32.const 0)
        (local.get $diff))
      (else
        (i32.const 1)
        (i64.const 0))))

  ;; Safe multiplication with overflow detection
  (func $safe_mul_i32 (export "safe_mul_i32")
    (param $a i32) (param $b i32)
    (result i32 i64)
    (local $product i64)

    (local.set $product
      (i64.mul
        (i64.extend_i32_s (local.get $a))
        (i64.extend_i32_s (local.get $b))))

    (if (result i32 i64)
      (i32.and
        (i64.ge_s (local.get $product) (i64.const -2147483648))
        (i64.le_s (local.get $product) (i64.const 2147483647)))
      (then
        (i32.const 0)
        (local.get $product))
      (else
        (i32.const 2)  ;; Error code 2 = multiplication overflow
        (i64.const 0))))

  ;; Safe division (no divide by zero)
  (func $safe_div_i32 (export "safe_div_i32")
    (param $a i32) (param $b i32)
    (result i32 i64)

    (if (result i32 i64) (i32.eqz (local.get $b))
      (then
        ;; Division by zero
        (i32.const 3)  ;; Error code 3 = division by zero
        (i64.const 0))
      (else
        ;; Check for MIN_INT / -1 overflow
        (if (result i32 i64)
          (i32.and
            (i32.eq (local.get $a) (i32.const -2147483648))
            (i32.eq (local.get $b) (i32.const -1)))
          (then
            (i32.const 1)  ;; Overflow
            (i64.const 0))
          (else
            (i32.const 0)
            (i64.extend_i32_s (i32.div_s (local.get $a) (local.get $b))))))))

  ;; Safe modulo (no divide by zero)
  (func $safe_mod_i32 (export "safe_mod_i32")
    (param $a i32) (param $b i32)
    (result i32 i64)

    (if (result i32 i64) (i32.eqz (local.get $b))
      (then
        (i32.const 3)
        (i64.const 0))
      (else
        (i32.const 0)
        (i64.extend_i32_s (i32.rem_s (local.get $a) (local.get $b))))))

  ;; ============================================================================
  ;; SAFE BOUNDS CHECKING
  ;; ============================================================================

  ;; Clamp value to range [min, max]
  (func $clamp_i32 (export "clamp_i32")
    (param $value i32) (param $min i32) (param $max i32)
    (result i32)

    (select
      (local.get $min)
      (select
        (local.get $max)
        (local.get $value)
        (i32.gt_s (local.get $value) (local.get $max)))
      (i32.lt_s (local.get $value) (local.get $min))))

  ;; Check if value is in range [min, max]
  (func $in_range_i32 (export "in_range_i32")
    (param $value i32) (param $min i32) (param $max i32)
    (result i32)

    (i32.and
      (i32.ge_s (local.get $value) (local.get $min))
      (i32.le_s (local.get $value) (local.get $max))))

  ;; ============================================================================
  ;; SAFE PORT VALIDATION
  ;; ============================================================================

  ;; Validate port number (1-65535)
  (func $is_valid_port (export "is_valid_port")
    (param $port i32)
    (result i32)

    (i32.and
      (i32.ge_u (local.get $port) (i32.const 1))
      (i32.le_u (local.get $port) (i32.const 65535))))

  ;; ============================================================================
  ;; SAFE MEMORY OPERATIONS
  ;; ============================================================================

  ;; Bounds-checked memory read (i32)
  ;; Returns (error_code, value)
  (func $safe_load_i32 (export "safe_load_i32")
    (param $offset i32)
    (result i32 i64)

    ;; Check bounds (memory is 64KB = 65536 bytes)
    (if (result i32 i64)
      (i32.gt_u (i32.add (local.get $offset) (i32.const 4)) (i32.const 65536))
      (then
        (i32.const 4)  ;; Error code 4 = out of bounds
        (i64.const 0))
      (else
        (i32.const 0)
        (i64.extend_i32_s (i32.load (local.get $offset))))))

  ;; Bounds-checked memory write
  ;; Returns error_code (0 = success)
  (func $safe_store_i32 (export "safe_store_i32")
    (param $offset i32) (param $value i32)
    (result i32)

    (if (result i32)
      (i32.gt_u (i32.add (local.get $offset) (i32.const 4)) (i32.const 65536))
      (then
        (i32.const 4))  ;; Out of bounds
      (else
        (i32.store (local.get $offset) (local.get $value))
        (i32.const 0))))

  ;; ============================================================================
  ;; UTILITY FUNCTIONS
  ;; ============================================================================

  ;; Absolute value (handles MIN_INT)
  (func $safe_abs_i32 (export "safe_abs_i32")
    (param $value i32)
    (result i32 i64)

    (if (result i32 i64)
      (i32.eq (local.get $value) (i32.const -2147483648))
      (then
        ;; MIN_INT has no positive representation
        (i32.const 1)
        (i64.const 0))
      (else
        (i32.const 0)
        (i64.extend_i32_s
          (select
            (i32.sub (i32.const 0) (local.get $value))
            (local.get $value)
            (i32.lt_s (local.get $value) (i32.const 0)))))))

  ;; Version info
  (func $version (export "version")
    (result i32)
    ;; Returns version as packed: major << 16 | minor << 8 | patch
    ;; 0.9.0 = 0x000900
    (i32.const 0x000900))
)
