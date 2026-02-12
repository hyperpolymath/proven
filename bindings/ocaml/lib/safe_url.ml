(* SPDX-License-Identifier: Apache-2.0 *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe URL parsing and validation with security checks. *)

(** URL scheme types. *)
type scheme =
  | Http
  | Https
  | Ftp
  | Ftps
  | Ws
  | Wss
  | File
  | Mailto
  | Tel
  | Data
  | Other of string

(** Parsed URL components. *)
type t = {
  scheme: scheme;
  username: string option;
  password: string option;
  host: string option;
  port: int option;
  path: string;
  query: string option;
  fragment: string option;
}

type error =
  | Invalid_format
  | Invalid_scheme
  | Invalid_host
  | Invalid_port
  | Invalid_encoding
  | Dangerous_scheme

(** Convert scheme to string. *)
let scheme_to_string = function
  | Http -> "http"
  | Https -> "https"
  | Ftp -> "ftp"
  | Ftps -> "ftps"
  | Ws -> "ws"
  | Wss -> "wss"
  | File -> "file"
  | Mailto -> "mailto"
  | Tel -> "tel"
  | Data -> "data"
  | Other s -> s

(** Parse scheme from string. *)
let scheme_of_string s =
  match String.lowercase_ascii s with
  | "http" -> Http
  | "https" -> Https
  | "ftp" -> Ftp
  | "ftps" -> Ftps
  | "ws" -> Ws
  | "wss" -> Wss
  | "file" -> File
  | "mailto" -> Mailto
  | "tel" -> Tel
  | "data" -> Data
  | s -> Other s

(** Check if scheme is secure (uses encryption). *)
let is_secure_scheme = function
  | Https | Ftps | Wss -> true
  | _ -> false

(** Check if scheme is considered dangerous for web contexts. *)
let is_dangerous_scheme = function
  | Other "javascript" | Other "vbscript" -> true
  | Data -> true  (* Can be used for XSS *)
  | _ -> false

(** Percent-encode a string for URL use. *)
let percent_encode ?(allow_slash=false) str =
  let buf = Buffer.create (String.length str * 3) in
  String.iter (fun c ->
    match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' ->
        Buffer.add_char buf c
    | '/' when allow_slash ->
        Buffer.add_char buf c
    | c ->
        Printf.bprintf buf "%%%02X" (Char.code c)
  ) str;
  Buffer.contents buf

(** Decode a percent-encoded string. *)
let percent_decode str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec decode i =
    if i >= len then Ok (Buffer.contents buf)
    else if str.[i] = '%' then
      if i + 2 >= len then Error Invalid_encoding
      else
        try
          let hex = String.sub str (i + 1) 2 in
          let code = int_of_string ("0x" ^ hex) in
          Buffer.add_char buf (Char.chr code);
          decode (i + 3)
        with _ -> Error Invalid_encoding
    else begin
      Buffer.add_char buf str.[i];
      decode (i + 1)
    end
  in
  decode 0

(** Parse a URL string. *)
let parse str =
  let str = String.trim str in
  if String.length str = 0 then Error Invalid_format
  else
    (* Find scheme *)
    match String.index_opt str ':' with
    | None -> Error Invalid_scheme
    | Some colon_idx ->
        let scheme_str = String.sub str 0 colon_idx in
        let scheme = scheme_of_string scheme_str in
        if is_dangerous_scheme scheme then Error Dangerous_scheme
        else
          let rest = String.sub str (colon_idx + 1) (String.length str - colon_idx - 1) in
          (* Check for authority (//host) *)
          let has_authority = String.length rest >= 2 && rest.[0] = '/' && rest.[1] = '/' in
          if has_authority then
            let after_slashes = String.sub rest 2 (String.length rest - 2) in
            (* Split by / to get authority and path *)
            let authority, path_query_fragment =
              match String.index_opt after_slashes '/' with
              | None -> (after_slashes, "")
              | Some idx ->
                  (String.sub after_slashes 0 idx,
                   String.sub after_slashes idx (String.length after_slashes - idx))
            in
            (* Parse authority: [user[:password]@]host[:port] *)
            let userinfo, hostport =
              match String.index_opt authority '@' with
              | None -> (None, authority)
              | Some idx ->
                  (Some (String.sub authority 0 idx),
                   String.sub authority (idx + 1) (String.length authority - idx - 1))
            in
            let username, password =
              match userinfo with
              | None -> (None, None)
              | Some ui ->
                  match String.index_opt ui ':' with
                  | None -> (Some ui, None)
                  | Some idx ->
                      (Some (String.sub ui 0 idx),
                       Some (String.sub ui (idx + 1) (String.length ui - idx - 1)))
            in
            (* Parse host and port *)
            let host, port =
              match String.rindex_opt hostport ':' with
              | None -> (hostport, None)
              | Some idx ->
                  let port_str = String.sub hostport (idx + 1) (String.length hostport - idx - 1) in
                  try
                    let port = int_of_string port_str in
                    if port < 0 || port > 65535 then
                      (hostport, None)  (* Invalid port, treat as part of host *)
                    else
                      (String.sub hostport 0 idx, Some port)
                  with _ -> (hostport, None)
            in
            (* Parse path, query, fragment *)
            let path, query_fragment =
              match String.index_opt path_query_fragment '?' with
              | None -> (path_query_fragment, None)
              | Some idx ->
                  (String.sub path_query_fragment 0 idx,
                   Some (String.sub path_query_fragment (idx + 1) (String.length path_query_fragment - idx - 1)))
            in
            let query, fragment =
              match query_fragment with
              | None ->
                  (* Check for fragment in path *)
                  (match String.index_opt path '#' with
                   | None -> (None, None)
                   | Some idx ->
                       (None, Some (String.sub path (idx + 1) (String.length path - idx - 1))))
              | Some qf ->
                  match String.index_opt qf '#' with
                  | None -> (Some qf, None)
                  | Some idx ->
                      (Some (String.sub qf 0 idx),
                       Some (String.sub qf (idx + 1) (String.length qf - idx - 1)))
            in
            let path =
              match String.index_opt path '#' with
              | None -> path
              | Some idx -> String.sub path 0 idx
            in
            Ok {
              scheme;
              username;
              password;
              host = if String.length host = 0 then None else Some host;
              port;
              path = if String.length path = 0 then "/" else path;
              query;
              fragment;
            }
          else
            (* No authority - simple path-based URL *)
            Ok {
              scheme;
              username = None;
              password = None;
              host = None;
              port = None;
              path = rest;
              query = None;
              fragment = None;
            }

(** Format a URL to string. *)
let format url =
  let buf = Buffer.create 256 in
  Buffer.add_string buf (scheme_to_string url.scheme);
  Buffer.add_char buf ':';
  if Option.is_some url.host then begin
    Buffer.add_string buf "//";
    (match url.username with
     | Some u ->
         Buffer.add_string buf u;
         (match url.password with
          | Some p ->
              Buffer.add_char buf ':';
              Buffer.add_string buf p
          | None -> ());
         Buffer.add_char buf '@'
     | None -> ());
    (match url.host with
     | Some h -> Buffer.add_string buf h
     | None -> ());
    (match url.port with
     | Some p -> Printf.bprintf buf ":%d" p
     | None -> ())
  end;
  Buffer.add_string buf url.path;
  (match url.query with
   | Some q ->
       Buffer.add_char buf '?';
       Buffer.add_string buf q
   | None -> ());
  (match url.fragment with
   | Some f ->
       Buffer.add_char buf '#';
       Buffer.add_string buf f
   | None -> ());
  Buffer.contents buf

(** Check if a URL string is valid. *)
let is_valid str =
  match parse str with
  | Ok _ -> true
  | Error _ -> false

(** Check if URL uses HTTPS. *)
let is_https url =
  url.scheme = Https

(** Get the origin (scheme + host + port). *)
let get_origin url =
  match url.host with
  | None -> None
  | Some host ->
      let port_str = match url.port with
        | Some p -> Printf.sprintf ":%d" p
        | None -> ""
      in
      Some (Printf.sprintf "%s://%s%s" (scheme_to_string url.scheme) host port_str)

(** Join a base URL with a relative path. *)
let join base relative =
  if String.length relative = 0 then Ok base
  else if String.length relative >= 2 && relative.[0] = '/' && relative.[1] = '/' then
    (* Protocol-relative URL *)
    parse ((scheme_to_string base.scheme) ^ ":" ^ relative)
  else if relative.[0] = '/' then
    (* Absolute path *)
    Ok { base with path = relative; query = None; fragment = None }
  else if relative.[0] = '?' then
    (* Query only *)
    Ok { base with query = Some (String.sub relative 1 (String.length relative - 1)); fragment = None }
  else if relative.[0] = '#' then
    (* Fragment only *)
    Ok { base with fragment = Some (String.sub relative 1 (String.length relative - 1)) }
  else
    (* Relative path *)
    let base_dir =
      match String.rindex_opt base.path '/' with
      | None -> "/"
      | Some idx -> String.sub base.path 0 (idx + 1)
    in
    Ok { base with path = base_dir ^ relative; query = None; fragment = None }

(** Normalize a URL (lowercase scheme/host, remove default ports, etc.). *)
let normalize url =
  let scheme = url.scheme in
  let host = Option.map String.lowercase_ascii url.host in
  let port =
    match url.port, scheme with
    | Some 80, Http -> None
    | Some 443, Https -> None
    | Some 21, Ftp -> None
    | p, _ -> p
  in
  { url with scheme; host; port }

(** Extract query parameters as key-value pairs. *)
let query_params url =
  match url.query with
  | None -> []
  | Some q ->
      String.split_on_char '&' q
      |> List.filter_map (fun pair ->
          match String.index_opt pair '=' with
          | None -> Some (pair, "")
          | Some idx ->
              let key = String.sub pair 0 idx in
              let value = String.sub pair (idx + 1) (String.length pair - idx - 1) in
              Some (key, value))

(** Build a query string from key-value pairs. *)
let build_query params =
  params
  |> List.map (fun (k, v) ->
      if String.length v = 0 then percent_encode k
      else Printf.sprintf "%s=%s" (percent_encode k) (percent_encode v))
  |> String.concat "&"

(** Add or update a query parameter. *)
let set_query_param url key value =
  let params = query_params url in
  let params = List.filter (fun (k, _) -> k <> key) params in
  let params = params @ [(key, value)] in
  { url with query = Some (build_query params) }

(** Remove a query parameter. *)
let remove_query_param url key =
  let params = query_params url in
  let params = List.filter (fun (k, _) -> k <> key) params in
  if List.length params = 0 then { url with query = None }
  else { url with query = Some (build_query params) }
