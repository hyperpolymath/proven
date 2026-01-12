(* SPDX-License-Identifier: PMPL-1.0 *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe email validation and parsing operations. *)

type email_parts = {
  local_part: string;
  domain: string;
}

let is_valid email =
  match String.split_on_char '@' email with
  | [local_part; domain] ->
      String.length local_part > 0 &&
      String.length domain >= 3 &&
      String.contains domain '.' &&
      domain.[0] <> '.' &&
      domain.[String.length domain - 1] <> '.'
  | _ -> false

let split email =
  if not (is_valid email) then None
  else
    match String.split_on_char '@' email with
    | [local_part; domain] -> Some { local_part; domain }
    | _ -> None

let get_domain email =
  match split email with
  | Some parts -> Some parts.domain
  | None -> None

let get_local_part email =
  match split email with
  | Some parts -> Some parts.local_part
  | None -> None

let normalize email =
  match split email with
  | Some parts -> Some (parts.local_part ^ "@" ^ String.lowercase_ascii parts.domain)
  | None -> None
