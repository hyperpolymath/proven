(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe hexadecimal encoding and decoding with constant-time operations. *)

type error =
  | Invalid_length
  | Invalid_character of char

(** Encode bytes to lowercase hexadecimal string. *)
let encode bytes =
  let len = String.length bytes in
  let buf = Buffer.create (len * 2) in
  for i = 0 to len - 1 do
    Printf.bprintf buf "%02x" (Char.code bytes.[i])
  done;
  Buffer.contents buf

(** Encode bytes to uppercase hexadecimal string. *)
let encode_upper bytes =
  let len = String.length bytes in
  let buf = Buffer.create (len * 2) in
  for i = 0 to len - 1 do
    Printf.bprintf buf "%02X" (Char.code bytes.[i])
  done;
  Buffer.contents buf

(** Convert a hex character to its integer value. *)
let hex_char_to_int c =
  match c with
  | '0'..'9' -> Ok (Char.code c - Char.code '0')
  | 'a'..'f' -> Ok (Char.code c - Char.code 'a' + 10)
  | 'A'..'F' -> Ok (Char.code c - Char.code 'A' + 10)
  | _ -> Error (Invalid_character c)

(** Convert an integer (0-15) to a lowercase hex character. *)
let int_to_hex_char n =
  if n < 10 then Char.chr (n + Char.code '0')
  else Char.chr (n - 10 + Char.code 'a')

(** Decode hexadecimal string to bytes. *)
let decode hex =
  let len = String.length hex in
  if len mod 2 <> 0 then Error Invalid_length
  else
    let buf = Buffer.create (len / 2) in
    let rec decode_pairs i =
      if i >= len then Ok (Buffer.contents buf)
      else
        match hex_char_to_int hex.[i], hex_char_to_int hex.[i + 1] with
        | Ok h, Ok l ->
            Buffer.add_char buf (Char.chr ((h lsl 4) lor l));
            decode_pairs (i + 2)
        | Error e, _ | _, Error e -> Error e
    in
    decode_pairs 0

(** Check if a string is valid hexadecimal. *)
let is_valid_hex str =
  String.length str mod 2 = 0 &&
  String.for_all (fun c ->
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  ) str

(** Constant-time comparison of two byte strings.
    Returns true if equal, false otherwise.
    Comparison time is independent of where differences occur. *)
let constant_time_equal a b =
  if String.length a <> String.length b then false
  else if String.length a = 0 then true
  else
    let result = ref 0 in
    for i = 0 to String.length a - 1 do
      result := !result lor (Char.code a.[i] lxor Char.code b.[i])
    done;
    !result = 0

(** Constant-time comparison of two hex strings.
    Decodes both and compares the underlying bytes. *)
let constant_time_equal_hex a b =
  match decode a, decode b with
  | Ok a_bytes, Ok b_bytes -> constant_time_equal a_bytes b_bytes
  | _ -> false

(** XOR two byte strings of equal length. *)
let xor a b =
  if String.length a <> String.length b then Error Invalid_length
  else
    let buf = Bytes.create (String.length a) in
    for i = 0 to String.length a - 1 do
      Bytes.set buf i (Char.chr (Char.code a.[i] lxor Char.code b.[i]))
    done;
    Ok (Bytes.to_string buf)

(** XOR two hex strings of equal length. *)
let xor_hex a b =
  match decode a, decode b with
  | Ok a_bytes, Ok b_bytes ->
      (match xor a_bytes b_bytes with
       | Ok result -> Ok (encode result)
       | Error e -> Error e)
  | Error e, _ | _, Error e -> Error e

(** Create a byte string of zeros. *)
let zero_bytes n =
  String.make n '\000'

(** Create a hex string of zeros. *)
let zero_hex n =
  String.make (n * 2) '0'

(** Reverse the bytes in a string. *)
let reverse_bytes bytes =
  let len = String.length bytes in
  let buf = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set buf i bytes.[len - 1 - i]
  done;
  Bytes.to_string buf

(** Pad bytes to a minimum length with leading zeros. *)
let pad_left bytes min_len =
  let len = String.length bytes in
  if len >= min_len then bytes
  else String.make (min_len - len) '\000' ^ bytes

(** Pad hex string to a minimum length with leading zeros. *)
let pad_left_hex hex min_chars =
  let len = String.length hex in
  if len >= min_chars then hex
  else String.make (min_chars - len) '0' ^ hex

(** Convert integer to big-endian bytes. *)
let int_to_bytes_be n num_bytes =
  let buf = Bytes.create num_bytes in
  for i = 0 to num_bytes - 1 do
    Bytes.set buf (num_bytes - 1 - i) (Char.chr ((n lsr (i * 8)) land 0xFF))
  done;
  Bytes.to_string buf

(** Convert big-endian bytes to integer. *)
let bytes_to_int_be bytes =
  let len = String.length bytes in
  let result = ref 0 in
  for i = 0 to len - 1 do
    result := (!result lsl 8) lor (Char.code bytes.[i])
  done;
  !result

(** Convert integer to little-endian bytes. *)
let int_to_bytes_le n num_bytes =
  let buf = Bytes.create num_bytes in
  for i = 0 to num_bytes - 1 do
    Bytes.set buf i (Char.chr ((n lsr (i * 8)) land 0xFF))
  done;
  Bytes.to_string buf

(** Convert little-endian bytes to integer. *)
let bytes_to_int_le bytes =
  let len = String.length bytes in
  let result = ref 0 in
  for i = len - 1 downto 0 do
    result := (!result lsl 8) lor (Char.code bytes.[i])
  done;
  !result

(** Pretty-print hex with grouping (e.g., "00 11 22 33"). *)
let format_hex ?(group_size=2) ?(separator=" ") hex =
  if group_size <= 0 then hex
  else
    let len = String.length hex in
    let buf = Buffer.create (len + (len / group_size)) in
    for i = 0 to len - 1 do
      if i > 0 && i mod group_size = 0 then
        Buffer.add_string buf separator;
      Buffer.add_char buf hex.[i]
    done;
    Buffer.contents buf
