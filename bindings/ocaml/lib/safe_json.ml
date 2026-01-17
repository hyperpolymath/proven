(* SPDX-License-Identifier: PMPL-1.0 *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe JSON validation and parsing with size limits. *)

(** JSON value types. *)
type t =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of t list
  | Object of (string * t) list

type error =
  | Invalid_syntax of string
  | Max_depth_exceeded
  | Max_size_exceeded
  | Invalid_number
  | Unterminated_string
  | Invalid_escape_sequence

(** Default limits. *)
let default_max_depth = 100
let default_max_size = 10_000_000  (* 10 MB *)

(** Check if a string is valid JSON (basic check). *)
let is_valid_json str =
  let len = String.length str in
  if len = 0 then false
  else
    let first = String.get (String.trim str) 0 in
    first = '{' || first = '[' || first = '"' ||
    first = 't' || first = 'f' || first = 'n' ||
    (first >= '0' && first <= '9') || first = '-'

(** Escape a string for JSON. *)
let escape_string str =
  let buf = Buffer.create (String.length str) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c when Char.code c < 32 ->
        Printf.bprintf buf "\\u%04x" (Char.code c)
    | c -> Buffer.add_char buf c
  ) str;
  Buffer.contents buf

(** Unescape a JSON string. *)
let unescape_string str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec process i =
    if i >= len then Ok (Buffer.contents buf)
    else if str.[i] = '\\' then
      if i + 1 >= len then Error Invalid_escape_sequence
      else
        match str.[i + 1] with
        | '"' -> Buffer.add_char buf '"'; process (i + 2)
        | '\\' -> Buffer.add_char buf '\\'; process (i + 2)
        | '/' -> Buffer.add_char buf '/'; process (i + 2)
        | 'b' -> Buffer.add_char buf '\b'; process (i + 2)
        | 'f' -> Buffer.add_char buf '\012'; process (i + 2)
        | 'n' -> Buffer.add_char buf '\n'; process (i + 2)
        | 'r' -> Buffer.add_char buf '\r'; process (i + 2)
        | 't' -> Buffer.add_char buf '\t'; process (i + 2)
        | 'u' when i + 5 < len ->
            (try
              let hex = String.sub str (i + 2) 4 in
              let code = int_of_string ("0x" ^ hex) in
              if code < 128 then
                Buffer.add_char buf (Char.chr code)
              else if code < 2048 then begin
                Buffer.add_char buf (Char.chr (0xC0 lor (code lsr 6)));
                Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
              end else begin
                Buffer.add_char buf (Char.chr (0xE0 lor (code lsr 12)));
                Buffer.add_char buf (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
                Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
              end;
              process (i + 6)
            with _ -> Error Invalid_escape_sequence)
        | _ -> Error Invalid_escape_sequence
    else begin
      Buffer.add_char buf str.[i];
      process (i + 1)
    end
  in
  process 0

(** Format JSON value to string. *)
let rec to_string ?(indent=0) ?(pretty=false) value =
  let spaces n = if pretty then String.make n ' ' else "" in
  let newline = if pretty then "\n" else "" in
  let sep = if pretty then ": " else ":" in
  match value with
  | Null -> "null"
  | Bool true -> "true"
  | Bool false -> "false"
  | Number n ->
      if Float.is_nan n || Float.is_inf n then "null"
      else
        let s = Printf.sprintf "%.17g" n in
        (* Ensure integers don't have unnecessary decimals *)
        if Float.is_integer n && not (String.contains s '.') && not (String.contains s 'e') then
          Printf.sprintf "%.0f" n
        else s
  | String s -> "\"" ^ escape_string s ^ "\""
  | Array [] -> "[]"
  | Array items ->
      let inner = String.concat ("," ^ newline ^ spaces (indent + 2))
        (List.map (to_string ~indent:(indent + 2) ~pretty) items) in
      "[" ^ newline ^ spaces (indent + 2) ^ inner ^ newline ^ spaces indent ^ "]"
  | Object [] -> "{}"
  | Object pairs ->
      let format_pair (k, v) =
        "\"" ^ escape_string k ^ "\"" ^ sep ^ to_string ~indent:(indent + 2) ~pretty v
      in
      let inner = String.concat ("," ^ newline ^ spaces (indent + 2))
        (List.map format_pair pairs) in
      "{" ^ newline ^ spaces (indent + 2) ^ inner ^ newline ^ spaces indent ^ "}"

(** Format JSON with pretty printing. *)
let to_string_pretty value =
  to_string ~pretty:true value

(** Get a value by key from an object. *)
let get key = function
  | Object pairs ->
      (try Some (List.assoc key pairs)
       with Not_found -> None)
  | _ -> None

(** Get a value by index from an array. *)
let get_index idx = function
  | Array items ->
      if idx >= 0 && idx < List.length items then
        Some (List.nth items idx)
      else None
  | _ -> None

(** Get a string value. *)
let as_string = function
  | String s -> Some s
  | _ -> None

(** Get a number value. *)
let as_number = function
  | Number n -> Some n
  | _ -> None

(** Get an integer value. *)
let as_int = function
  | Number n when Float.is_integer n -> Some (int_of_float n)
  | _ -> None

(** Get a boolean value. *)
let as_bool = function
  | Bool b -> Some b
  | _ -> None

(** Get an array value. *)
let as_array = function
  | Array items -> Some items
  | _ -> None

(** Get an object value. *)
let as_object = function
  | Object pairs -> Some pairs
  | _ -> None

(** Check if value is null. *)
let is_null = function
  | Null -> true
  | _ -> false

(** Check if value is a boolean. *)
let is_bool = function
  | Bool _ -> true
  | _ -> false

(** Check if value is a number. *)
let is_number = function
  | Number _ -> true
  | _ -> false

(** Check if value is a string. *)
let is_string = function
  | String _ -> true
  | _ -> false

(** Check if value is an array. *)
let is_array = function
  | Array _ -> true
  | _ -> false

(** Check if value is an object. *)
let is_object = function
  | Object _ -> true
  | _ -> false

(** Get nested value by path (dot-separated keys or array indices). *)
let get_path path value =
  let parts = String.split_on_char '.' path in
  let rec traverse parts current =
    match parts with
    | [] -> Some current
    | key :: rest ->
        let next =
          match int_of_string_opt key with
          | Some idx -> get_index idx current
          | None -> get key current
        in
        match next with
        | None -> None
        | Some v -> traverse rest v
  in
  traverse parts value

(** Set a value in an object (creates new object). *)
let set key new_value = function
  | Object pairs ->
      let pairs = List.filter (fun (k, _) -> k <> key) pairs in
      Object ((key, new_value) :: pairs)
  | v -> v

(** Remove a key from an object. *)
let remove key = function
  | Object pairs ->
      Object (List.filter (fun (k, _) -> k <> key) pairs)
  | v -> v

(** Merge two objects (second overrides first). *)
let merge a b =
  match a, b with
  | Object pairs_a, Object pairs_b ->
      let keys_b = List.map fst pairs_b in
      let filtered_a = List.filter (fun (k, _) -> not (List.mem k keys_b)) pairs_a in
      Object (filtered_a @ pairs_b)
  | _, b -> b

(** Count elements in an array or keys in an object. *)
let length = function
  | Array items -> List.length items
  | Object pairs -> List.length pairs
  | _ -> 0

(** Map over array elements. *)
let map_array f = function
  | Array items -> Array (List.map f items)
  | v -> v

(** Filter array elements. *)
let filter_array f = function
  | Array items -> Array (List.filter f items)
  | v -> v

(** Get all keys from an object. *)
let keys = function
  | Object pairs -> List.map fst pairs
  | _ -> []

(** Get all values from an object. *)
let values = function
  | Object pairs -> List.map snd pairs
  | _ -> []

(** Check if object has a key. *)
let has_key key = function
  | Object pairs -> List.mem_assoc key pairs
  | _ -> false

(** Deep equality check. *)
let rec equal a b =
  match a, b with
  | Null, Null -> true
  | Bool a, Bool b -> a = b
  | Number a, Number b -> a = b
  | String a, String b -> a = b
  | Array a, Array b ->
      List.length a = List.length b &&
      List.for_all2 equal a b
  | Object a, Object b ->
      List.length a = List.length b &&
      List.for_all (fun (k, v) ->
        match List.assoc_opt k b with
        | Some v2 -> equal v v2
        | None -> false
      ) a
  | _ -> false
