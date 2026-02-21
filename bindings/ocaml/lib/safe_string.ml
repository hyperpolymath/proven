(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe string operations for security-sensitive contexts. *)

let escape_html value =
  let buf = Buffer.create (String.length value) in
  String.iter (fun c ->
    match c with
    | '&' -> Buffer.add_string buf "&amp;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '"' -> Buffer.add_string buf "&quot;"
    | '\'' -> Buffer.add_string buf "&#x27;"
    | c -> Buffer.add_char buf c
  ) value;
  Buffer.contents buf

let escape_sql value =
  let buf = Buffer.create (String.length value) in
  String.iter (fun c ->
    match c with
    | '\'' -> Buffer.add_string buf "''"
    | c -> Buffer.add_char buf c
  ) value;
  Buffer.contents buf

let escape_js value =
  let buf = Buffer.create (String.length value) in
  String.iter (fun c ->
    match c with
    | '\\' -> Buffer.add_string buf "\\\\"
    | '"' -> Buffer.add_string buf "\\\""
    | '\'' -> Buffer.add_string buf "\\'"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c
  ) value;
  Buffer.contents buf

let escape_url value =
  let buf = Buffer.create (String.length value * 3) in
  String.iter (fun c ->
    match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' ->
        Buffer.add_char buf c
    | c ->
        Printf.bprintf buf "%%%02X" (Char.code c)
  ) value;
  Buffer.contents buf

let truncate_safe ?(suffix="...") value max_len =
  if String.length value <= max_len then value
  else
    let suffix_len = String.length suffix in
    if max_len <= suffix_len then String.sub suffix 0 max_len
    else String.sub value 0 (max_len - suffix_len) ^ suffix
