(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe UUID validation, parsing, and generation. *)

(** UUID version types. *)
type version =
  | V1  (** Time-based UUID *)
  | V2  (** DCE Security UUID *)
  | V3  (** MD5 hash namespace UUID *)
  | V4  (** Random UUID *)
  | V5  (** SHA-1 hash namespace UUID *)
  | V6  (** Reordered time-based UUID *)
  | V7  (** Unix epoch time-based UUID *)
  | V8  (** Custom UUID *)
  | Unknown of int

(** UUID variant types per RFC 4122. *)
type variant =
  | Ncs       (** Reserved for NCS backward compatibility *)
  | Rfc4122   (** Standard UUID variant *)
  | Microsoft (** Reserved for Microsoft backward compatibility *)
  | Future    (** Reserved for future definition *)

(** Opaque UUID type. *)
type t = {
  bytes: string;  (** 16 bytes raw representation *)
}

type error =
  | Invalid_format
  | Invalid_length
  | Invalid_hex_char

(** Parse a hex character to its integer value. *)
let hex_char_to_int c =
  match c with
  | '0'..'9' -> Ok (Char.code c - Char.code '0')
  | 'a'..'f' -> Ok (Char.code c - Char.code 'a' + 10)
  | 'A'..'F' -> Ok (Char.code c - Char.code 'A' + 10)
  | _ -> Error Invalid_hex_char

(** Convert two hex characters to a byte. *)
let hex_pair_to_byte h l =
  match hex_char_to_int h, hex_char_to_int l with
  | Ok hv, Ok lv -> Ok (Char.chr ((hv lsl 4) lor lv))
  | Error e, _ | _, Error e -> Error e

(** Parse a UUID from standard 8-4-4-4-12 format. *)
let parse str =
  let str = String.trim str in
  (* Remove optional braces/parens *)
  let str =
    if String.length str >= 2 then
      match str.[0], str.[String.length str - 1] with
      | '{', '}' | '(', ')' -> String.sub str 1 (String.length str - 2)
      | _ -> str
    else str
  in
  if String.length str <> 36 then Error Invalid_length
  else if str.[8] <> '-' || str.[13] <> '-' || str.[18] <> '-' || str.[23] <> '-' then
    Error Invalid_format
  else
    let hex_chars =
      String.sub str 0 8 ^
      String.sub str 9 4 ^
      String.sub str 14 4 ^
      String.sub str 19 4 ^
      String.sub str 24 12
    in
    if String.length hex_chars <> 32 then Error Invalid_format
    else
      let buf = Buffer.create 16 in
      let rec convert i =
        if i >= 32 then Ok { bytes = Buffer.contents buf }
        else
          match hex_pair_to_byte hex_chars.[i] hex_chars.[i + 1] with
          | Ok c -> Buffer.add_char buf c; convert (i + 2)
          | Error e -> Error e
      in
      convert 0

(** Format a UUID to standard 8-4-4-4-12 lowercase format. *)
let format uuid =
  let hex_of_byte b =
    Printf.sprintf "%02x" (Char.code b)
  in
  let b = uuid.bytes in
  Printf.sprintf "%s%s%s%s-%s%s-%s%s-%s%s-%s%s%s%s%s%s"
    (hex_of_byte b.[0]) (hex_of_byte b.[1]) (hex_of_byte b.[2]) (hex_of_byte b.[3])
    (hex_of_byte b.[4]) (hex_of_byte b.[5])
    (hex_of_byte b.[6]) (hex_of_byte b.[7])
    (hex_of_byte b.[8]) (hex_of_byte b.[9])
    (hex_of_byte b.[10]) (hex_of_byte b.[11]) (hex_of_byte b.[12])
    (hex_of_byte b.[13]) (hex_of_byte b.[14]) (hex_of_byte b.[15])

(** Format a UUID to uppercase. *)
let format_upper uuid =
  String.uppercase_ascii (format uuid)

(** Get the UUID version. *)
let get_version uuid =
  let version_byte = Char.code uuid.bytes.[6] in
  match (version_byte lsr 4) land 0x0F with
  | 1 -> V1
  | 2 -> V2
  | 3 -> V3
  | 4 -> V4
  | 5 -> V5
  | 6 -> V6
  | 7 -> V7
  | 8 -> V8
  | n -> Unknown n

(** Get the UUID variant. *)
let get_variant uuid =
  let variant_byte = Char.code uuid.bytes.[8] in
  if (variant_byte land 0x80) = 0 then Ncs
  else if (variant_byte land 0xC0) = 0x80 then Rfc4122
  else if (variant_byte land 0xE0) = 0xC0 then Microsoft
  else Future

(** Check if a string is a valid UUID format. *)
let is_valid str =
  match parse str with
  | Ok _ -> true
  | Error _ -> false

(** Generate a random (v4) UUID. *)
let generate_v4 () =
  let buf = Bytes.create 16 in
  let ic = open_in_bin "/dev/urandom" in
  really_input ic buf 0 16;
  close_in ic;
  (* Set version to 4 *)
  Bytes.set buf 6 (Char.chr ((Char.code (Bytes.get buf 6) land 0x0F) lor 0x40));
  (* Set variant to RFC 4122 *)
  Bytes.set buf 8 (Char.chr ((Char.code (Bytes.get buf 8) land 0x3F) lor 0x80));
  { bytes = Bytes.to_string buf }

(** The nil UUID (all zeros). *)
let nil = { bytes = String.make 16 '\000' }

(** Check if a UUID is nil. *)
let is_nil uuid =
  uuid.bytes = nil.bytes

(** Compare two UUIDs. *)
let compare a b =
  String.compare a.bytes b.bytes

(** Check equality of two UUIDs. *)
let equal a b =
  a.bytes = b.bytes

(** Get raw bytes of UUID. *)
let to_bytes uuid = uuid.bytes

(** Create UUID from raw bytes. *)
let of_bytes bytes =
  if String.length bytes <> 16 then Error Invalid_length
  else Ok { bytes }
