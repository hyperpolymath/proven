(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(* safe_float.sml -- Safe floating-point operations.
 *
 * Thin wrapper around libproven's SafeFloat module.  Provides division,
 * square root, natural logarithm, and NaN/infinity checks.  Also includes
 * the safe expression calculator, angle conversions, probability functions,
 * and ML activation functions.
 *
 * All computation is performed in the formally verified Idris 2 core.
 *)

signature SAFE_FLOAT =
sig
  (** Safe floating-point division.
   *  Returns NONE on division by zero or NaN input. *)
  val div' : Real64.real * Real64.real -> Real64.real option

  (** Check if a float is finite (not NaN or Inf). *)
  val isFinite : Real64.real -> bool

  (** Check if a float is NaN. *)
  val isNan : Real64.real -> bool

  (** Safe square root.  Returns NONE for negative or NaN input. *)
  val sqrt : Real64.real -> Real64.real option

  (** Safe natural logarithm.  Returns NONE for x <= 0 or NaN. *)
  val ln : Real64.real -> Real64.real option

  (** Evaluate a safe arithmetic expression string.
   *  Supports +, -, *, /, parentheses, decimals, and negatives.
   *  Returns NONE for invalid expressions or division by zero. *)
  val eval : string -> Real64.real option

  (* ---- Angle Conversions ------------------------------------------------- *)

  (** Convert degrees to radians. *)
  val degToRad : Real64.real -> Real64.real

  (** Convert radians to degrees. *)
  val radToDeg : Real64.real -> Real64.real

  (** Normalize angle to [0, 360) degrees. *)
  val normalizeDegrees : Real64.real -> Real64.real

  (** Normalize angle to [0, 2*pi) radians. *)
  val normalizeRadians : Real64.real -> Real64.real

  (* ---- Probability ------------------------------------------------------- *)

  (** Create a probability value clamped to [0, 1]. *)
  val probability : Real64.real -> Real64.real

  (** P(A and B) = P(A) * P(B) for independent events. *)
  val probabilityAnd : Real64.real * Real64.real -> Real64.real

  (** P(A or B) = P(A) + P(B) for mutually exclusive events (clamped). *)
  val probabilityOr : Real64.real * Real64.real -> Real64.real

  (** P(not A) = 1 - P(A). *)
  val probabilityNot : Real64.real -> Real64.real

  (* ---- ML Activation Functions ------------------------------------------- *)

  (** Sigmoid: 1 / (1 + exp(-x)). *)
  val sigmoid : Real64.real -> Real64.real

  (** ReLU: max(0, x). *)
  val relu : Real64.real -> Real64.real

  (** Leaky ReLU: x >= 0 ? x : alpha * x. *)
  val leakyRelu : Real64.real * Real64.real -> Real64.real

  (** Clamp value to [minVal, maxVal]. *)
  val clamp : Real64.real * Real64.real * Real64.real -> Real64.real

  (* ---- Unit Conversions -------------------------------------------------- *)

  (** Length units. *)
  datatype length_unit =
      METERS | KILOMETERS | CENTIMETERS | MILLIMETERS
    | FEET | INCHES | MILES | YARDS

  (** Temperature units. *)
  datatype temp_unit = CELSIUS | FAHRENHEIT | KELVIN

  (** Convert length between units.  Returns NONE for NaN. *)
  val convertLength : Real64.real * length_unit * length_unit -> Real64.real option

  (** Convert temperature between units.  Returns NONE for below absolute zero. *)
  val convertTemp : Real64.real * temp_unit * temp_unit -> Real64.real option
end

structure SafeFloat :> SAFE_FLOAT =
struct

  (* ---- Core Float Operations --------------------------------------------- *)

  fun div' (a, b) =
    ProvenMarshal.unwrapFloat (LibProven.floatDiv (a, b))

  fun isFinite x = LibProven.floatIsFinite x

  fun isNan x = LibProven.floatIsNan x

  fun sqrt x =
    ProvenMarshal.unwrapFloat (LibProven.floatSqrt x)

  fun ln x =
    ProvenMarshal.unwrapFloat (LibProven.floatLn x)

  fun eval (expr : string) : Real64.real option =
    let
      val bytes = ProvenMarshal.stringToBytes expr
      val len = ProvenMarshal.stringLen expr
    in
      ProvenMarshal.unwrapFloat (LibProven.calculatorEval (bytes, len))
    end

  (* ---- Angle Conversions ------------------------------------------------- *)

  fun degToRad d = LibProven.angleDegToRad d
  fun radToDeg r = LibProven.angleRadToDeg r
  fun normalizeDegrees d = LibProven.angleNormalizeDegrees d
  fun normalizeRadians r = LibProven.angleNormalizeRadians r

  (* ---- Probability ------------------------------------------------------- *)

  fun probability p = LibProven.probabilityCreate p
  fun probabilityAnd (a, b) = LibProven.probabilityAnd (a, b)
  fun probabilityOr (a, b) = LibProven.probabilityOrExclusive (a, b)
  fun probabilityNot p = LibProven.probabilityNot p

  (* ---- ML Activation Functions ------------------------------------------- *)

  fun sigmoid x = LibProven.mlSigmoid x
  fun relu x = LibProven.mlRelu x
  fun leakyRelu (x, alpha) = LibProven.mlLeakyRelu (x, alpha)
  fun clamp (x, minVal, maxVal) = LibProven.mlClamp (x, minVal, maxVal)

  (* ---- Unit Conversions -------------------------------------------------- *)

  datatype length_unit =
      METERS | KILOMETERS | CENTIMETERS | MILLIMETERS
    | FEET | INCHES | MILES | YARDS

  datatype temp_unit = CELSIUS | FAHRENHEIT | KELVIN

  fun lengthUnitToInt METERS      = 0 : Int32.int
    | lengthUnitToInt KILOMETERS  = 1
    | lengthUnitToInt CENTIMETERS = 2
    | lengthUnitToInt MILLIMETERS = 3
    | lengthUnitToInt FEET        = 4
    | lengthUnitToInt INCHES      = 5
    | lengthUnitToInt MILES       = 6
    | lengthUnitToInt YARDS       = 7

  fun tempUnitToInt CELSIUS    = 0 : Int32.int
    | tempUnitToInt FAHRENHEIT = 1
    | tempUnitToInt KELVIN     = 2

  fun convertLength (value, from, to) =
    ProvenMarshal.unwrapFloat (
      LibProven.unitConvertLength (value, lengthUnitToInt from, lengthUnitToInt to))

  fun convertTemp (value, from, to) =
    ProvenMarshal.unwrapFloat (
      LibProven.unitConvertTemp (value, tempUnitToInt from, tempUnitToInt to))

end
