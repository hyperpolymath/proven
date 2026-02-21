(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe floating-point operations with NaN/infinity handling. *)

type error =
  | Is_nan
  | Is_infinity
  | Division_by_zero
  | Overflow
  | Underflow

(** Check if value is a valid finite number. *)
let is_finite x =
  not (Float.is_nan x) && not (Float.is_inf x)

(** Safe add with overflow/NaN checking. *)
let safe_add a b =
  if Float.is_nan a || Float.is_nan b then Error Is_nan
  else
    let result = a +. b in
    if Float.is_nan result then Error Is_nan
    else if Float.is_inf result then Error Overflow
    else Ok result

(** Safe subtract with overflow/NaN checking. *)
let safe_sub a b =
  if Float.is_nan a || Float.is_nan b then Error Is_nan
  else
    let result = a -. b in
    if Float.is_nan result then Error Is_nan
    else if Float.is_inf result then Error Overflow
    else Ok result

(** Safe multiply with overflow/NaN checking. *)
let safe_mul a b =
  if Float.is_nan a || Float.is_nan b then Error Is_nan
  else
    let result = a *. b in
    if Float.is_nan result then Error Is_nan
    else if Float.is_inf result then Error Overflow
    else Ok result

(** Safe divide with zero/NaN checking. *)
let safe_div a b =
  if Float.is_nan a || Float.is_nan b then Error Is_nan
  else if b = 0.0 then Error Division_by_zero
  else
    let result = a /. b in
    if Float.is_nan result then Error Is_nan
    else if Float.is_inf result then Error Overflow
    else Ok result

(** Safe square root. *)
let safe_sqrt x =
  if Float.is_nan x then Error Is_nan
  else if x < 0.0 then Error Is_nan
  else Ok (Float.sqrt x)

(** Safe power. *)
let safe_pow base exp =
  if Float.is_nan base || Float.is_nan exp then Error Is_nan
  else
    let result = Float.pow base exp in
    if Float.is_nan result then Error Is_nan
    else if Float.is_inf result then Error Overflow
    else Ok result

(** Safe logarithm (natural). *)
let safe_log x =
  if Float.is_nan x then Error Is_nan
  else if x <= 0.0 then Error Is_nan
  else Ok (Float.log x)

(** Safe log base 10. *)
let safe_log10 x =
  if Float.is_nan x then Error Is_nan
  else if x <= 0.0 then Error Is_nan
  else Ok (Float.log10 x)

(** Safe exponential. *)
let safe_exp x =
  if Float.is_nan x then Error Is_nan
  else
    let result = Float.exp x in
    if Float.is_inf result then Error Overflow
    else Ok result

(** Clamp value to range. *)
let clamp ~min ~max x =
  if Float.is_nan x then Error Is_nan
  else if Float.is_nan min || Float.is_nan max then Error Is_nan
  else Ok (Float.max min (Float.min max x))

(** Check if two floats are approximately equal. *)
let approx_equal ?(epsilon=1e-10) a b =
  if Float.is_nan a || Float.is_nan b then false
  else
    let diff = Float.abs (a -. b) in
    diff <= epsilon || diff <= epsilon *. Float.max (Float.abs a) (Float.abs b)

(** Round to n decimal places. *)
let round_to_decimals n x =
  if Float.is_nan x then Error Is_nan
  else if Float.is_inf x then Error Is_infinity
  else
    let factor = Float.pow 10.0 (float_of_int n) in
    Ok (Float.round (x *. factor) /. factor)

(** Truncate to n decimal places. *)
let truncate_to_decimals n x =
  if Float.is_nan x then Error Is_nan
  else if Float.is_inf x then Error Is_infinity
  else
    let factor = Float.pow 10.0 (float_of_int n) in
    Ok (Float.trunc (x *. factor) /. factor)

(** Ceiling to n decimal places. *)
let ceil_to_decimals n x =
  if Float.is_nan x then Error Is_nan
  else if Float.is_inf x then Error Is_infinity
  else
    let factor = Float.pow 10.0 (float_of_int n) in
    Ok (Float.ceil (x *. factor) /. factor)

(** Floor to n decimal places. *)
let floor_to_decimals n x =
  if Float.is_nan x then Error Is_nan
  else if Float.is_inf x then Error Is_infinity
  else
    let factor = Float.pow 10.0 (float_of_int n) in
    Ok (Float.floor (x *. factor) /. factor)

(** Linear interpolation. *)
let lerp a b t =
  if Float.is_nan a || Float.is_nan b || Float.is_nan t then Error Is_nan
  else
    let result = a +. (b -. a) *. t in
    if Float.is_inf result then Error Overflow
    else Ok result

(** Map value from one range to another. *)
let map_range ~from_min ~from_max ~to_min ~to_max x =
  if Float.is_nan x || Float.is_nan from_min || Float.is_nan from_max ||
     Float.is_nan to_min || Float.is_nan to_max then
    Error Is_nan
  else if from_max = from_min then
    Error Division_by_zero
  else
    let normalized = (x -. from_min) /. (from_max -. from_min) in
    let result = to_min +. normalized *. (to_max -. to_min) in
    if Float.is_inf result then Error Overflow
    else Ok result

(** Safe modulo. *)
let safe_mod a b =
  if Float.is_nan a || Float.is_nan b then Error Is_nan
  else if b = 0.0 then Error Division_by_zero
  else Ok (Float.rem a b)

(** Convert to integer if it's a whole number. *)
let to_int_exact x =
  if Float.is_nan x then Error Is_nan
  else if Float.is_inf x then Error Is_infinity
  else if not (Float.is_integer x) then Error Is_nan
  else if x > float_of_int max_int || x < float_of_int min_int then Error Overflow
  else Ok (int_of_float x)

(** Format float with specific precision. *)
let format_precision n x =
  if Float.is_nan x then "NaN"
  else if Float.is_inf x then (if x > 0.0 then "Infinity" else "-Infinity")
  else Printf.sprintf "%.*f" n x

(** Parse float with validation. *)
let parse str =
  try
    let x = float_of_string str in
    if Float.is_nan x then Error Is_nan
    else if Float.is_inf x then Error Is_infinity
    else Ok x
  with _ -> Error Is_nan

(** Safe reciprocal (1/x). *)
let safe_reciprocal x =
  safe_div 1.0 x

(** Sign of a number (-1, 0, or 1). *)
let sign x =
  if Float.is_nan x then Error Is_nan
  else if x > 0.0 then Ok 1
  else if x < 0.0 then Ok (-1)
  else Ok 0

(** Hypotenuse (sqrt(a^2 + b^2)) with overflow protection. *)
let safe_hypot a b =
  if Float.is_nan a || Float.is_nan b then Error Is_nan
  else
    let result = Float.hypot a b in
    if Float.is_inf result then Error Overflow
    else Ok result

(** Safe arc tangent of y/x. *)
let safe_atan2 y x =
  if Float.is_nan y || Float.is_nan x then Error Is_nan
  else Ok (Float.atan2 y x)

(** Safe sine. *)
let safe_sin x =
  if Float.is_nan x then Error Is_nan
  else if Float.is_inf x then Error Is_infinity
  else Ok (Float.sin x)

(** Safe cosine. *)
let safe_cos x =
  if Float.is_nan x then Error Is_nan
  else if Float.is_inf x then Error Is_infinity
  else Ok (Float.cos x)

(** Safe tangent. *)
let safe_tan x =
  if Float.is_nan x then Error Is_nan
  else if Float.is_inf x then Error Is_infinity
  else
    let result = Float.tan x in
    if Float.is_inf result then Error Overflow
    else Ok result

(** Constants. *)
let pi = Float.pi
let e = Float.exp 1.0
let epsilon = Float.epsilon
let max_finite = Float.max_float
let min_positive = Float.min_float
