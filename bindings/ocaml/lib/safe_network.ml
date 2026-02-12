(* SPDX-License-Identifier: Apache-2.0 *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe network operations for IP address validation and classification. *)

type ipv4 = {
  a: int;
  b: int;
  c: int;
  d: int;
}

let ipv4_to_string ip =
  Printf.sprintf "%d.%d.%d.%d" ip.a ip.b ip.c ip.d

let parse_ipv4 address =
  match String.split_on_char '.' address with
  | [a; b; c; d] ->
      (try
        let a = int_of_string a in
        let b = int_of_string b in
        let c = int_of_string c in
        let d = int_of_string d in
        if a >= 0 && a <= 255 &&
           b >= 0 && b <= 255 &&
           c >= 0 && c <= 255 &&
           d >= 0 && d <= 255
        then Some { a; b; c; d }
        else None
      with Failure _ -> None)
  | _ -> None

let is_valid_ipv4 address =
  match parse_ipv4 address with
  | Some _ -> true
  | None -> false

let is_private address =
  match parse_ipv4 address with
  | None -> false
  | Some ip ->
      (* 10.0.0.0/8 *)
      ip.a = 10 ||
      (* 172.16.0.0/12 *)
      (ip.a = 172 && ip.b >= 16 && ip.b <= 31) ||
      (* 192.168.0.0/16 *)
      (ip.a = 192 && ip.b = 168)

let is_loopback address =
  match parse_ipv4 address with
  | None -> false
  | Some ip -> ip.a = 127

let is_public address =
  is_valid_ipv4 address && not (is_private address) && not (is_loopback address)

let format_ipv4 = ipv4_to_string
