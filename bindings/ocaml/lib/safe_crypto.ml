(* SPDX-License-Identifier: Apache-2.0 *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Cryptographic safety operations with constant-time guarantees. *)

let constant_time_compare a b =
  if String.length a <> String.length b then false
  else if String.length a = 0 then true
  else
    let result = ref 0 in
    for i = 0 to String.length a - 1 do
      result := !result lor (Char.code a.[i] lxor Char.code b.[i])
    done;
    !result = 0

let secure_zero length =
  String.make length '\000'

let random_bytes n =
  let buf = Bytes.create n in
  let ic = open_in_bin "/dev/urandom" in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.to_string buf

let random_hex n =
  let bytes = random_bytes n in
  let buf = Buffer.create (n * 2) in
  String.iter (fun c ->
    Printf.bprintf buf "%02x" (Char.code c)
  ) bytes;
  Buffer.contents buf
