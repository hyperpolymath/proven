(* SPDX-License-Identifier: Apache-2.0 *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe filesystem path operations with traversal attack prevention. *)

let has_traversal path =
  String.length path >= 2 &&
  (let rec check i =
    if i > String.length path - 2 then false
    else if path.[i] = '.' && path.[i+1] = '.' then true
    else check (i + 1)
  in check 0) ||
  String.contains path '~'

let is_safe path = not (has_traversal path)

let sanitize_filename filename =
  let buf = Buffer.create (String.length filename) in
  let prev_dot = ref false in
  String.iter (fun c ->
    match c with
    | '.' when !prev_dot ->
        Buffer.add_char buf '_';
        prev_dot := false
    | '.' ->
        prev_dot := true;
        Buffer.add_char buf '.'
    | '/' | '\\' | '<' | '>' | ':' | '"' | '|' | '?' | '*' | '\000' ->
        if !prev_dot then prev_dot := false;
        Buffer.add_char buf '_'
    | c ->
        if !prev_dot then prev_dot := false;
        Buffer.add_char buf c
  ) filename;
  (* Handle trailing .. *)
  let result = Buffer.contents buf in
  let len = String.length result in
  if len >= 2 && result.[len-2] = '.' && result.[len-1] = '.' then
    String.sub result 0 (len - 2) ^ "__"
  else result

let safe_join base parts =
  if List.exists has_traversal parts then None
  else
    let sanitized = List.map sanitize_filename parts in
    Some (List.fold_left Filename.concat base sanitized)
