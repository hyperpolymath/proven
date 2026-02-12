(* SPDX-License-Identifier: Apache-2.0 *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe phone number validation and formatting. *)

(** Country calling codes per ITU-T E.164. *)
type country_code =
  | US  (** +1 United States *)
  | CA  (** +1 Canada *)
  | GB  (** +44 United Kingdom *)
  | DE  (** +49 Germany *)
  | FR  (** +33 France *)
  | IT  (** +39 Italy *)
  | ES  (** +34 Spain *)
  | JP  (** +81 Japan *)
  | CN  (** +86 China *)
  | IN  (** +91 India *)
  | AU  (** +61 Australia *)
  | BR  (** +55 Brazil *)
  | MX  (** +52 Mexico *)
  | KR  (** +82 South Korea *)
  | RU  (** +7 Russia *)
  | NL  (** +31 Netherlands *)
  | SE  (** +46 Sweden *)
  | CH  (** +41 Switzerland *)
  | BE  (** +32 Belgium *)
  | AT  (** +43 Austria *)
  | PL  (** +48 Poland *)
  | Other of int  (** Other country code *)

(** Phone number representation. *)
type t = {
  country: country_code;
  national_number: string;  (** National significant number (digits only) *)
}

type error =
  | Invalid_format
  | Invalid_country_code
  | Invalid_length
  | Invalid_characters

(** Get numeric calling code for country. *)
let calling_code_of_country = function
  | US | CA -> 1
  | GB -> 44
  | DE -> 49
  | FR -> 33
  | IT -> 39
  | ES -> 34
  | JP -> 81
  | CN -> 86
  | IN -> 91
  | AU -> 61
  | BR -> 55
  | MX -> 52
  | KR -> 82
  | RU -> 7
  | NL -> 31
  | SE -> 46
  | CH -> 41
  | BE -> 32
  | AT -> 43
  | PL -> 48
  | Other n -> n

(** Get country from numeric calling code. *)
let country_of_calling_code = function
  | 1 -> US  (* Note: Both US and CA use +1, defaults to US *)
  | 44 -> GB
  | 49 -> DE
  | 33 -> FR
  | 39 -> IT
  | 34 -> ES
  | 81 -> JP
  | 86 -> CN
  | 91 -> IN
  | 61 -> AU
  | 55 -> BR
  | 52 -> MX
  | 82 -> KR
  | 7 -> RU
  | 31 -> NL
  | 46 -> SE
  | 41 -> CH
  | 32 -> BE
  | 43 -> AT
  | 48 -> PL
  | n -> Other n

(** Country code to ISO 3166-1 alpha-2 string. *)
let country_to_string = function
  | US -> "US" | CA -> "CA" | GB -> "GB" | DE -> "DE" | FR -> "FR"
  | IT -> "IT" | ES -> "ES" | JP -> "JP" | CN -> "CN" | IN -> "IN"
  | AU -> "AU" | BR -> "BR" | MX -> "MX" | KR -> "KR" | RU -> "RU"
  | NL -> "NL" | SE -> "SE" | CH -> "CH" | BE -> "BE" | AT -> "AT"
  | PL -> "PL" | Other n -> Printf.sprintf "+%d" n

(** Extract only digit characters from a string. *)
let extract_digits str =
  let buf = Buffer.create (String.length str) in
  String.iter (fun c ->
    if c >= '0' && c <= '9' then Buffer.add_char buf c
  ) str;
  Buffer.contents buf

(** Validate that string contains only valid phone characters. *)
let has_valid_chars str =
  String.for_all (fun c ->
    (c >= '0' && c <= '9') || c = '+' || c = '-' || c = ' ' ||
    c = '(' || c = ')' || c = '.'
  ) str

(** Get expected national number length range for country. *)
let national_number_length_range = function
  | US | CA -> (10, 10)
  | GB -> (10, 11)
  | DE -> (6, 13)
  | FR -> (9, 9)
  | IT -> (6, 11)
  | ES -> (9, 9)
  | JP -> (9, 10)
  | CN -> (11, 11)
  | IN -> (10, 10)
  | AU -> (9, 9)
  | BR -> (10, 11)
  | MX -> (10, 10)
  | KR -> (8, 11)
  | RU -> (10, 10)
  | NL -> (9, 9)
  | SE -> (7, 13)
  | CH -> (9, 9)
  | BE -> (8, 9)
  | AT -> (4, 13)
  | PL -> (9, 9)
  | Other _ -> (4, 15)  (* E.164 allows 4-15 digits for national number *)

(** Parse phone number from E.164 format (+1234567890). *)
let parse_e164 str =
  if String.length str < 2 then Error Invalid_format
  else if str.[0] <> '+' then Error Invalid_format
  else
    let digits = extract_digits str in
    if String.length digits < 7 || String.length digits > 15 then
      Error Invalid_length
    else
      (* Try to match country codes (1-3 digits) *)
      let try_code len =
        if len > String.length digits then None
        else
          let code_str = String.sub digits 0 len in
          try
            let code = int_of_string code_str in
            let country = country_of_calling_code code in
            let national = String.sub digits len (String.length digits - len) in
            let min_len, max_len = national_number_length_range country in
            if String.length national >= min_len && String.length national <= max_len then
              Some { country; national_number = national }
            else
              None
          with _ -> None
      in
      (* Try 1, 2, then 3 digit codes *)
      match try_code 1 with
      | Some phone -> Ok phone
      | None ->
          match try_code 2 with
          | Some phone -> Ok phone
          | None ->
              match try_code 3 with
              | Some phone -> Ok phone
              | None -> Error Invalid_country_code

(** Parse phone number with explicit country. *)
let parse ?(country=US) str =
  if not (has_valid_chars str) then Error Invalid_characters
  else
    let digits = extract_digits str in
    let min_len, max_len = national_number_length_range country in
    if String.length digits < min_len || String.length digits > max_len then
      Error Invalid_length
    else
      Ok { country; national_number = digits }

(** Format to E.164 international format. *)
let format_e164 phone =
  Printf.sprintf "+%d%s"
    (calling_code_of_country phone.country)
    phone.national_number

(** Format for display (country-specific). *)
let format phone =
  let n = phone.national_number in
  match phone.country with
  | US | CA when String.length n = 10 ->
      Printf.sprintf "(%s) %s-%s"
        (String.sub n 0 3) (String.sub n 3 3) (String.sub n 6 4)
  | GB when String.length n >= 10 ->
      Printf.sprintf "+44 %s %s"
        (String.sub n 0 4) (String.sub n 4 (String.length n - 4))
  | DE ->
      Printf.sprintf "+49 %s" n
  | FR when String.length n = 9 ->
      Printf.sprintf "+33 %s %s %s %s %s"
        (String.sub n 0 1) (String.sub n 1 2) (String.sub n 3 2)
        (String.sub n 5 2) (String.sub n 7 2)
  | _ ->
      format_e164 phone

(** Format as national number only. *)
let format_national phone =
  phone.national_number

(** Check if phone number is valid. *)
let is_valid phone =
  let min_len, max_len = national_number_length_range phone.country in
  let len = String.length phone.national_number in
  len >= min_len && len <= max_len &&
  String.for_all (fun c -> c >= '0' && c <= '9') phone.national_number

(** Get the country code. *)
let get_country phone = phone.country

(** Get the calling code as integer. *)
let get_calling_code phone = calling_code_of_country phone.country

(** Get the national number. *)
let get_national_number phone = phone.national_number

(** Compare two phone numbers. *)
let compare a b =
  let cmp = compare (calling_code_of_country a.country) (calling_code_of_country b.country) in
  if cmp <> 0 then cmp
  else String.compare a.national_number b.national_number

(** Check equality of two phone numbers. *)
let equal a b =
  compare a b = 0

(** Normalize phone number (strip formatting, standardize). *)
let normalize phone =
  { phone with national_number = extract_digits phone.national_number }

(** Check if number is a mobile number (heuristic, country-specific). *)
let is_mobile phone =
  let n = phone.national_number in
  if String.length n = 0 then false
  else
    match phone.country with
    | US | CA ->
        (* US/CA: All 10-digit numbers can be mobile *)
        String.length n = 10
    | GB ->
        (* UK mobile: starts with 7 *)
        n.[0] = '7'
    | DE ->
        (* German mobile: starts with 15, 16, 17 *)
        String.length n >= 2 && n.[0] = '1' &&
        (n.[1] = '5' || n.[1] = '6' || n.[1] = '7')
    | FR ->
        (* French mobile: starts with 6 or 7 *)
        n.[0] = '6' || n.[0] = '7'
    | _ ->
        (* Default: cannot determine *)
        false
