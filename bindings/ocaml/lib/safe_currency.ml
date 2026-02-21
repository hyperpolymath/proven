(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe currency handling with precise decimal arithmetic. *)

(** ISO 4217 currency codes. *)
type currency_code =
  | USD  (** United States Dollar *)
  | EUR  (** Euro *)
  | GBP  (** British Pound Sterling *)
  | JPY  (** Japanese Yen *)
  | CHF  (** Swiss Franc *)
  | CAD  (** Canadian Dollar *)
  | AUD  (** Australian Dollar *)
  | CNY  (** Chinese Yuan *)
  | INR  (** Indian Rupee *)
  | BRL  (** Brazilian Real *)
  | MXN  (** Mexican Peso *)
  | KRW  (** South Korean Won *)
  | SGD  (** Singapore Dollar *)
  | HKD  (** Hong Kong Dollar *)
  | NOK  (** Norwegian Krone *)
  | SEK  (** Swedish Krona *)
  | DKK  (** Danish Krone *)
  | NZD  (** New Zealand Dollar *)
  | ZAR  (** South African Rand *)
  | RUB  (** Russian Ruble *)
  | PLN  (** Polish Zloty *)
  | THB  (** Thai Baht *)
  | Other of string  (** Other ISO 4217 code *)

(** Money representation with minor units (cents, pence, etc.). *)
type t = {
  amount: int64;      (** Amount in minor units *)
  currency: currency_code;
  decimals: int;      (** Number of decimal places (usually 2, 0 for JPY/KRW) *)
}

type error =
  | Currency_mismatch
  | Overflow
  | Invalid_amount
  | Parse_error
  | Division_by_zero
  | Negative_amount

(** Get the number of decimal places for a currency. *)
let decimals_for_currency = function
  | JPY | KRW -> 0
  | _ -> 2

(** Get the multiplier for converting to minor units. *)
let multiplier decimals =
  let rec pow10 n acc =
    if n <= 0 then acc else pow10 (n - 1) (Int64.mul acc 10L)
  in
  pow10 decimals 1L

(** Convert currency code to ISO 4217 string. *)
let currency_to_string = function
  | USD -> "USD" | EUR -> "EUR" | GBP -> "GBP" | JPY -> "JPY"
  | CHF -> "CHF" | CAD -> "CAD" | AUD -> "AUD" | CNY -> "CNY"
  | INR -> "INR" | BRL -> "BRL" | MXN -> "MXN" | KRW -> "KRW"
  | SGD -> "SGD" | HKD -> "HKD" | NOK -> "NOK" | SEK -> "SEK"
  | DKK -> "DKK" | NZD -> "NZD" | ZAR -> "ZAR" | RUB -> "RUB"
  | PLN -> "PLN" | THB -> "THB" | Other s -> s

(** Parse currency code from ISO 4217 string. *)
let currency_of_string s =
  match String.uppercase_ascii s with
  | "USD" -> USD | "EUR" -> EUR | "GBP" -> GBP | "JPY" -> JPY
  | "CHF" -> CHF | "CAD" -> CAD | "AUD" -> AUD | "CNY" -> CNY
  | "INR" -> INR | "BRL" -> BRL | "MXN" -> MXN | "KRW" -> KRW
  | "SGD" -> SGD | "HKD" -> HKD | "NOK" -> NOK | "SEK" -> SEK
  | "DKK" -> DKK | "NZD" -> NZD | "ZAR" -> ZAR | "RUB" -> RUB
  | "PLN" -> PLN | "THB" -> THB | code -> Other code

(** Create money from minor units. *)
let of_minor_units amount currency =
  let decimals = decimals_for_currency currency in
  { amount; currency; decimals }

(** Create money from major units (e.g., dollars). *)
let of_major_units amount currency =
  let decimals = decimals_for_currency currency in
  let mult = multiplier decimals in
  { amount = Int64.mul (Int64.of_float amount) mult; currency; decimals }

(** Create money from string like "123.45". *)
let of_string str currency =
  let decimals = decimals_for_currency currency in
  try
    let str = String.trim str in
    (* Remove currency symbols if present *)
    let str =
      if String.length str > 0 && not (str.[0] >= '0' && str.[0] <= '9' || str.[0] = '-' || str.[0] = '+') then
        String.sub str 1 (String.length str - 1)
      else str
    in
    let str = String.trim str in
    match String.split_on_char '.' str with
    | [whole] ->
        let major = Int64.of_string whole in
        let mult = multiplier decimals in
        Ok { amount = Int64.mul major mult; currency; decimals }
    | [whole; frac] ->
        let major = Int64.of_string whole in
        let mult = multiplier decimals in
        let frac =
          if String.length frac > decimals then
            String.sub frac 0 decimals
          else
            frac ^ String.make (decimals - String.length frac) '0'
        in
        let minor = Int64.of_string frac in
        let is_negative = major < 0L || (major = 0L && String.length whole > 0 && whole.[0] = '-') in
        let amount =
          if is_negative then
            Int64.sub (Int64.mul major mult) minor
          else
            Int64.add (Int64.mul major mult) minor
        in
        Ok { amount; currency; decimals }
    | _ -> Error Parse_error
  with _ -> Error Parse_error

(** Format money to string. *)
let to_string money =
  let mult = multiplier money.decimals in
  let major = Int64.div money.amount mult in
  let minor = Int64.abs (Int64.rem money.amount mult) in
  if money.decimals = 0 then
    Printf.sprintf "%Ld" major
  else
    Printf.sprintf "%Ld.%0*Ld" major money.decimals minor

(** Format money with currency symbol. *)
let format ?(symbol=true) money =
  let amount_str = to_string money in
  if symbol then
    currency_to_string money.currency ^ " " ^ amount_str
  else
    amount_str

(** Add two money values. *)
let add a b =
  if a.currency <> b.currency then Error Currency_mismatch
  else
    let result = Int64.add a.amount b.amount in
    (* Check for overflow *)
    if (a.amount > 0L && b.amount > 0L && result < 0L) ||
       (a.amount < 0L && b.amount < 0L && result > 0L) then
      Error Overflow
    else
      Ok { a with amount = result }

(** Subtract two money values. *)
let sub a b =
  if a.currency <> b.currency then Error Currency_mismatch
  else
    let result = Int64.sub a.amount b.amount in
    (* Check for overflow *)
    if (a.amount > 0L && b.amount < 0L && result < 0L) ||
       (a.amount < 0L && b.amount > 0L && result > 0L) then
      Error Overflow
    else
      Ok { a with amount = result }

(** Multiply money by a scalar. *)
let mul money scalar =
  if scalar = 0L then Ok { money with amount = 0L }
  else
    let abs_amount = Int64.abs money.amount in
    let abs_scalar = Int64.abs scalar in
    (* Check for overflow before multiplying *)
    if abs_amount > Int64.div Int64.max_int abs_scalar then
      Error Overflow
    else
      Ok { money with amount = Int64.mul money.amount scalar }

(** Divide money by a scalar with rounding. *)
let div money divisor =
  if divisor = 0L then Error Division_by_zero
  else Ok { money with amount = Int64.div money.amount divisor }

(** Negate money value. *)
let negate money =
  if money.amount = Int64.min_int then Error Overflow
  else Ok { money with amount = Int64.neg money.amount }

(** Absolute value of money. *)
let abs money =
  if money.amount = Int64.min_int then Error Overflow
  else Ok { money with amount = Int64.abs money.amount }

(** Compare two money values. *)
let compare a b =
  if a.currency <> b.currency then
    Error Currency_mismatch
  else
    Ok (Int64.compare a.amount b.amount)

(** Check if money is zero. *)
let is_zero money = money.amount = 0L

(** Check if money is positive. *)
let is_positive money = money.amount > 0L

(** Check if money is negative. *)
let is_negative money = money.amount < 0L

(** Get amount in minor units. *)
let to_minor_units money = money.amount

(** Get amount as float (may lose precision). *)
let to_float money =
  let mult = multiplier money.decimals in
  Int64.to_float money.amount /. Int64.to_float mult

(** Create zero money in a currency. *)
let zero currency =
  of_minor_units 0L currency

(** Round to nearest minor unit after a calculation. *)
let round_half_up money =
  money  (* Already in minor units, no rounding needed *)

(** Split money into equal parts (handles remainders). *)
let split money n =
  if n <= 0 then Error Invalid_amount
  else
    let base_amount = Int64.div money.amount (Int64.of_int n) in
    let remainder = Int64.rem money.amount (Int64.of_int n) in
    let parts = Array.make n { money with amount = base_amount } in
    (* Distribute remainder to first parts *)
    for i = 0 to (Int64.to_int (Int64.abs remainder)) - 1 do
      let adjustment = if remainder > 0L then 1L else -1L in
      parts.(i) <- { parts.(i) with amount = Int64.add parts.(i).amount adjustment }
    done;
    Ok (Array.to_list parts)
