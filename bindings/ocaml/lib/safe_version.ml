(* SPDX-License-Identifier: PMPL-1.0 *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe semantic versioning operations. *)

(** Semantic version representation. *)
type t = {
  major: int;
  minor: int;
  patch: int;
  prerelease: string list;
  build: string list;
}

type error =
  | Invalid_format
  | Invalid_major
  | Invalid_minor
  | Invalid_patch
  | Invalid_prerelease
  | Invalid_build
  | Negative_version

(** Create a new version. *)
let create ?(prerelease=[]) ?(build=[]) major minor patch =
  if major < 0 then Error Negative_version
  else if minor < 0 then Error Negative_version
  else if patch < 0 then Error Negative_version
  else Ok { major; minor; patch; prerelease; build }

(** Parse a version string. *)
let parse str =
  let str = String.trim str in
  (* Remove leading 'v' if present *)
  let str = if String.length str > 0 && (str.[0] = 'v' || str.[0] = 'V') then
    String.sub str 1 (String.length str - 1)
  else str in
  try
    (* Split off build metadata first *)
    let version_pre, build =
      match String.index_opt str '+' with
      | None -> (str, [])
      | Some idx ->
          let build_str = String.sub str (idx + 1) (String.length str - idx - 1) in
          (String.sub str 0 idx, String.split_on_char '.' build_str)
    in
    (* Split off prerelease *)
    let version_str, prerelease =
      match String.index_opt version_pre '-' with
      | None -> (version_pre, [])
      | Some idx ->
          let pre_str = String.sub version_pre (idx + 1) (String.length version_pre - idx - 1) in
          (String.sub version_pre 0 idx, String.split_on_char '.' pre_str)
    in
    (* Parse major.minor.patch *)
    match String.split_on_char '.' version_str with
    | [major_s] ->
        let major = int_of_string major_s in
        create ~prerelease ~build major 0 0
    | [major_s; minor_s] ->
        let major = int_of_string major_s in
        let minor = int_of_string minor_s in
        create ~prerelease ~build major minor 0
    | [major_s; minor_s; patch_s] ->
        let major = int_of_string major_s in
        let minor = int_of_string minor_s in
        let patch = int_of_string patch_s in
        create ~prerelease ~build major minor patch
    | _ -> Error Invalid_format
  with _ -> Error Invalid_format

(** Format version to string. *)
let format v =
  let base = Printf.sprintf "%d.%d.%d" v.major v.minor v.patch in
  let with_pre =
    if v.prerelease = [] then base
    else base ^ "-" ^ String.concat "." v.prerelease
  in
  if v.build = [] then with_pre
  else with_pre ^ "+" ^ String.concat "." v.build

(** Format without build metadata. *)
let format_without_build v =
  let base = Printf.sprintf "%d.%d.%d" v.major v.minor v.patch in
  if v.prerelease = [] then base
  else base ^ "-" ^ String.concat "." v.prerelease

(** Compare two prerelease identifiers. *)
let compare_prerelease_id a b =
  match int_of_string_opt a, int_of_string_opt b with
  | Some na, Some nb -> compare na nb
  | Some _, None -> -1  (* Numeric < alphanumeric *)
  | None, Some _ -> 1
  | None, None -> String.compare a b

(** Compare two prerelease lists. *)
let rec compare_prerelease a b =
  match a, b with
  | [], [] -> 0
  | [], _ -> -1  (* No prerelease > has prerelease is FALSE, having prerelease is lower *)
  | _, [] -> 1   (* Wait, SemVer says: when major.minor.patch are equal, prerelease < release *)
  | x::xs, y::ys ->
      let c = compare_prerelease_id x y in
      if c <> 0 then c else compare_prerelease xs ys

(** Compare two versions. *)
let compare a b =
  let c = compare a.major b.major in
  if c <> 0 then c
  else
    let c = compare a.minor b.minor in
    if c <> 0 then c
    else
      let c = compare a.patch b.patch in
      if c <> 0 then c
      else
        (* Prerelease comparison per SemVer 2.0.0 *)
        match a.prerelease, b.prerelease with
        | [], [] -> 0
        | [], _ -> 1   (* Release > prerelease *)
        | _, [] -> -1  (* Prerelease < release *)
        | pa, pb -> compare_prerelease pa pb

(** Check equality (ignores build metadata per SemVer). *)
let equal a b = compare a b = 0

(** Check if a < b. *)
let lt a b = compare a b < 0

(** Check if a <= b. *)
let le a b = compare a b <= 0

(** Check if a > b. *)
let gt a b = compare a b > 0

(** Check if a >= b. *)
let ge a b = compare a b >= 0

(** Bump major version. *)
let bump_major v =
  { major = v.major + 1; minor = 0; patch = 0; prerelease = []; build = [] }

(** Bump minor version. *)
let bump_minor v =
  { v with minor = v.minor + 1; patch = 0; prerelease = []; build = [] }

(** Bump patch version. *)
let bump_patch v =
  { v with patch = v.patch + 1; prerelease = []; build = [] }

(** Set prerelease. *)
let with_prerelease prerelease v =
  { v with prerelease; build = [] }

(** Set build metadata. *)
let with_build build v =
  { v with build }

(** Clear prerelease and build. *)
let release v =
  { v with prerelease = []; build = [] }

(** Check if version is stable (>= 1.0.0 and no prerelease). *)
let is_stable v =
  v.major >= 1 && v.prerelease = []

(** Check if version is prerelease. *)
let is_prerelease v =
  v.prerelease <> []

(** Check if version satisfies a simple range constraint.
    Supports: =, >, <, >=, <=, ~ (tilde), ^ (caret) *)
let satisfies range v =
  let range = String.trim range in
  if String.length range = 0 then false
  else
    let parse_constraint str =
      let str = String.trim str in
      if String.length str = 0 then None
      else
        let first = str.[0] in
        if first = '=' then
          Some (`Eq, String.sub str 1 (String.length str - 1))
        else if first = '>' then
          if String.length str > 1 && str.[1] = '=' then
            Some (`Ge, String.sub str 2 (String.length str - 2))
          else
            Some (`Gt, String.sub str 1 (String.length str - 1))
        else if first = '<' then
          if String.length str > 1 && str.[1] = '=' then
            Some (`Le, String.sub str 2 (String.length str - 2))
          else
            Some (`Lt, String.sub str 1 (String.length str - 1))
        else if first = '~' then
          Some (`Tilde, String.sub str 1 (String.length str - 1))
        else if first = '^' then
          Some (`Caret, String.sub str 1 (String.length str - 1))
        else
          Some (`Eq, str)
    in
    match parse_constraint range with
    | None -> false
    | Some (op, ver_str) ->
        match parse ver_str with
        | Error _ -> false
        | Ok target ->
            match op with
            | `Eq -> equal v target
            | `Gt -> gt v target
            | `Ge -> ge v target
            | `Lt -> lt v target
            | `Le -> le v target
            | `Tilde ->
                (* ~1.2.3 means >=1.2.3 <1.3.0 *)
                ge v target && v.major = target.major && v.minor = target.minor
            | `Caret ->
                (* ^1.2.3 means >=1.2.3 <2.0.0 (or <0.2.0 if major=0, etc.) *)
                if not (ge v target) then false
                else if target.major > 0 then v.major = target.major
                else if target.minor > 0 then v.major = 0 && v.minor = target.minor
                else v.major = 0 && v.minor = 0 && v.patch = target.patch

(** Zero version (0.0.0). *)
let zero = { major = 0; minor = 0; patch = 0; prerelease = []; build = [] }

(** Initial development version (0.1.0). *)
let initial = { major = 0; minor = 1; patch = 0; prerelease = []; build = [] }

(** First stable version (1.0.0). *)
let stable = { major = 1; minor = 0; patch = 0; prerelease = []; build = [] }
