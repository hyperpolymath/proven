(* SPDX-License-Identifier: Apache-2.0 *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe mathematical operations with overflow detection. *)

let safe_div numerator denominator =
  if denominator = 0 then None
  else Some (numerator / denominator)

let safe_mod numerator denominator =
  if denominator = 0 then None
  else Some (numerator mod denominator)

let safe_add a b =
  (* Check for overflow using Int module bounds *)
  if b > 0 && a > max_int - b then None
  else if b < 0 && a < min_int - b then None
  else Some (a + b)

let safe_sub a b =
  (* Check for overflow *)
  if b < 0 && a > max_int + b then None
  else if b > 0 && a < min_int + b then None
  else Some (a - b)

let safe_mul a b =
  if a = 0 || b = 0 then Some 0
  else
    (* Check for overflow using division *)
    let would_overflow =
      if a > 0 then
        if b > 0 then a > max_int / b
        else b < min_int / a
      else
        if b > 0 then a < min_int / b
        else a <> 0 && b < max_int / a
    in
    if would_overflow then None
    else Some (a * b)
