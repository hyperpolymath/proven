; SPDX-License-Identifier: PMPL-1.0-or-later
; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; SafeMath - Arithmetic operations that cannot crash.
;;
;; All functions delegate to libproven via FFI. Returns None on error
;; (overflow, underflow, division by zero). NEVER reimplements logic.

(import proven.ffi [get-lib ok? PROVEN-OK])


(defn safe-add [a b]
  "Checked addition. Returns None on overflow.

  Example:
    (safe-add 5 3)    ; => 8
    (safe-add (** 2 62) (** 2 62))  ; => None (overflow)
  "
  (setv result (.proven_math_add_checked (get-lib) a b))
  (when (ok? result.status)
    (return result.value))
  None)


(defn safe-sub [a b]
  "Checked subtraction. Returns None on underflow.

  Example:
    (safe-sub 10 3)  ; => 7
  "
  (setv result (.proven_math_sub_checked (get-lib) a b))
  (when (ok? result.status)
    (return result.value))
  None)


(defn safe-mul [a b]
  "Checked multiplication. Returns None on overflow.

  Example:
    (safe-mul 6 7)  ; => 42
  "
  (setv result (.proven_math_mul_checked (get-lib) a b))
  (when (ok? result.status)
    (return result.value))
  None)


(defn safe-div [numerator denominator]
  "Safe integer division. Returns None on division by zero or overflow.

  Example:
    (safe-div 10 2)  ; => 5
    (safe-div 10 0)  ; => None
  "
  (setv result (.proven_math_div (get-lib) numerator denominator))
  (when (ok? result.status)
    (return result.value))
  None)


(defn safe-mod [numerator denominator]
  "Safe modulo. Returns None on division by zero.

  Example:
    (safe-mod 10 3)  ; => 1
    (safe-mod 10 0)  ; => None
  "
  (setv result (.proven_math_mod (get-lib) numerator denominator))
  (when (ok? result.status)
    (return result.value))
  None)


(defn safe-abs [n]
  "Safe absolute value. Returns None for INT64_MIN.

  Example:
    (safe-abs -42)  ; => 42
  "
  (setv result (.proven_math_abs_safe (get-lib) n))
  (when (ok? result.status)
    (return result.value))
  None)


(defn safe-negate [n]
  "Safe negation via checked subtraction from zero. Returns None on overflow.

  Example:
    (safe-negate 5)  ; => -5
  "
  (safe-sub 0 n))


(defn clamp [lo hi value]
  "Clamp value to [lo, hi] range.

  Example:
    (clamp 0 100 50)   ; => 50
    (clamp 0 100 150)  ; => 100
  "
  (.proven_math_clamp (get-lib) lo hi value))


(defn safe-pow [base exp]
  "Checked integer exponentiation. Returns None on overflow.

  Example:
    (safe-pow 2 10)  ; => 1024
  "
  (when (< exp 0)
    (return None))
  (setv result (.proven_math_pow_checked (get-lib) base exp))
  (when (ok? result.status)
    (return result.value))
  None)
