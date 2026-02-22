(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(* safe_math.sml -- Safe integer and arithmetic operations.
 *
 * Thin wrapper around libproven's SafeMath module.  Every function delegates
 * to the formally verified Idris 2 core via the C FFI -- no arithmetic logic
 * is reimplemented in SML.
 *
 * All operations return SOME on success and NONE on error (overflow, underflow,
 * division by zero, etc.).
 *)

signature SAFE_MATH =
sig
  (** Checked addition: returns NONE on overflow. *)
  val add : Int64.int * Int64.int -> Int64.int option

  (** Checked subtraction: returns NONE on underflow. *)
  val sub : Int64.int * Int64.int -> Int64.int option

  (** Checked multiplication: returns NONE on overflow. *)
  val mul : Int64.int * Int64.int -> Int64.int option

  (** Safe division: returns NONE on division by zero or INT64_MIN / -1. *)
  val div' : Int64.int * Int64.int -> Int64.int option

  (** Safe modulo: returns NONE on division by zero. *)
  val mod' : Int64.int * Int64.int -> Int64.int option

  (** Safe absolute value: returns NONE for INT64_MIN. *)
  val abs : Int64.int -> Int64.int option

  (** Clamp value to [lo, hi].  Always succeeds. *)
  val clamp : { lo : Int64.int, hi : Int64.int, value : Int64.int } -> Int64.int

  (** Checked exponentiation: returns NONE on overflow. *)
  val pow : Int64.int * Word32.word -> Int64.int option
end

structure SafeMath :> SAFE_MATH =
struct

  fun add (a, b) =
    ProvenMarshal.unwrapInt (LibProven.mathAddChecked (a, b))

  fun sub (a, b) =
    ProvenMarshal.unwrapInt (LibProven.mathSubChecked (a, b))

  fun mul (a, b) =
    ProvenMarshal.unwrapInt (LibProven.mathMulChecked (a, b))

  fun div' (a, b) =
    ProvenMarshal.unwrapInt (LibProven.mathDiv (a, b))

  fun mod' (a, b) =
    ProvenMarshal.unwrapInt (LibProven.mathMod (a, b))

  fun abs n =
    ProvenMarshal.unwrapInt (LibProven.mathAbsSafe n)

  fun clamp { lo, hi, value } =
    LibProven.mathClamp (lo, hi, value)

  fun pow (base, exp) =
    ProvenMarshal.unwrapInt (LibProven.mathPowChecked (base, exp))

end
