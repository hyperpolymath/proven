(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(** Proven: Formally verified safety primitives for OCaml.

    This library is a thin FFI wrapper around [libproven], which provides
    formally verified (Idris 2) safety primitives.  All computation is
    performed inside the verified Idris 2 core -- this OCaml package does
    data marshaling only.

    Every public function returns [option] or [result] instead of raising
    exceptions. *)

open Ctypes
module F = Proven_ffi

(* ========================================================================== *)
(* Internal helpers                                                           *)
(* ========================================================================== *)

(** Allocate a ctypes char pointer from an OCaml string. *)
let cstring_of_string (s : string) : char ptr =
  let len = String.length s in
  let buf = allocate_n char ~count:(len + 1) in
  for i = 0 to len - 1 do
    (buf +@ i) <-@ s.[i]
  done;
  (buf +@ len) <-@ '\000';
  buf

(** Read a C string (pointer + length) into an OCaml string, then free it
    via [proven_free_string]. *)
let read_and_free_string (p : char ptr) (len : Unsigned.size_t) : string =
  let n = Unsigned.Size_t.to_int len in
  let s = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set s i !@(p +@ i)
  done;
  F.proven_free_string p;
  Bytes.to_string s

(** Check if a status value is ok (== 0). *)
let status_ok (s : int32) : bool =
  Int32.equal s F.proven_status_ok

(** Unwrap an IntResult into [Some int64] if ok, [None] otherwise. *)
let unwrap_int_result (r : F.int_result structure) : int64 option =
  if status_ok (getf r F.ir_status) then
    Some (getf r F.ir_value)
  else
    None

(** Unwrap a BoolResult into [Some bool] if ok, [None] otherwise. *)
let unwrap_bool_result (r : F.bool_result structure) : bool option =
  if status_ok (getf r F.br_status) then
    Some (getf r F.br_value)
  else
    None

(** Unwrap a StringResult into [Some string] if ok, [None] otherwise.
    The C string is freed after copying. *)
let unwrap_string_result (r : F.string_result structure) : string option =
  if status_ok (getf r F.sr_status) then
    let p = getf r F.sr_value in
    let len = getf r F.sr_length in
    if is_null p then None
    else Some (read_and_free_string p len)
  else
    None

(** Unwrap a FloatResult into [Some float] if ok, [None] otherwise. *)
let unwrap_float_result (r : F.float_result structure) : float option =
  if status_ok (getf r F.fr_status) then
    Some (getf r F.fr_value)
  else
    None

(* ========================================================================== *)
(* Lifecycle                                                                  *)
(* ========================================================================== *)

module Lifecycle = struct
  (** Initialize libproven.  Returns [true] if initialization succeeded. *)
  let init () : bool =
    let rc = F.proven_init () in
    Int32.equal rc 0l

  (** Deinitialize libproven. *)
  let deinit () : unit =
    F.proven_deinit ()

  (** Check whether libproven has been initialized. *)
  let is_initialized () : bool =
    F.proven_is_initialized ()
end

(* ========================================================================== *)
(* Version                                                                    *)
(* ========================================================================== *)

module Version = struct
  (** Library major version. *)
  let major () : int =
    Unsigned.UInt32.to_int (F.proven_version_major ())

  (** Library minor version. *)
  let minor () : int =
    Unsigned.UInt32.to_int (F.proven_version_minor ())

  (** Library patch version. *)
  let patch () : int =
    Unsigned.UInt32.to_int (F.proven_version_patch ())

  (** FFI ABI version. *)
  let abi_version () : int =
    Unsigned.UInt32.to_int (F.proven_ffi_abi_version ())

  (** Number of modules in libproven. *)
  let module_count () : int =
    Unsigned.UInt32.to_int (F.proven_module_count ())
end

(* ========================================================================== *)
(* SafeMath                                                                   *)
(* ========================================================================== *)

module SafeMath = struct
  (** Safe division.  Returns [None] on division by zero. *)
  let div (a : int) (b : int) : int option =
    unwrap_int_result (F.proven_math_div (Int64.of_int a) (Int64.of_int b))
    |> Option.map Int64.to_int

  (** Safe modulo.  Returns [None] on division by zero. *)
  let modulo (a : int) (b : int) : int option =
    unwrap_int_result (F.proven_math_mod (Int64.of_int a) (Int64.of_int b))
    |> Option.map Int64.to_int

  (** Checked addition.  Returns [None] on overflow. *)
  let add (a : int) (b : int) : int option =
    unwrap_int_result (F.proven_math_add_checked (Int64.of_int a) (Int64.of_int b))
    |> Option.map Int64.to_int

  (** Checked subtraction.  Returns [None] on underflow. *)
  let sub (a : int) (b : int) : int option =
    unwrap_int_result (F.proven_math_sub_checked (Int64.of_int a) (Int64.of_int b))
    |> Option.map Int64.to_int

  (** Checked multiplication.  Returns [None] on overflow. *)
  let mul (a : int) (b : int) : int option =
    unwrap_int_result (F.proven_math_mul_checked (Int64.of_int a) (Int64.of_int b))
    |> Option.map Int64.to_int

  (** Safe absolute value.  Handles MIN_INT correctly. *)
  let abs (n : int) : int option =
    unwrap_int_result (F.proven_math_abs_safe (Int64.of_int n))
    |> Option.map Int64.to_int

  (** Clamp value to range [lo, hi]. *)
  let clamp ~lo ~hi (value : int) : int =
    Int64.to_int (F.proven_math_clamp (Int64.of_int lo) (Int64.of_int hi) (Int64.of_int value))

  (** Integer power with overflow checking. *)
  let pow (base : int) (exp : int) : int option =
    unwrap_int_result (F.proven_math_pow_checked (Int64.of_int base) (Unsigned.UInt32.of_int exp))
    |> Option.map Int64.to_int

  (** 64-bit variants for when OCaml [int] is too narrow. *)

  let div64 (a : int64) (b : int64) : int64 option =
    unwrap_int_result (F.proven_math_div a b)

  let add64 (a : int64) (b : int64) : int64 option =
    unwrap_int_result (F.proven_math_add_checked a b)

  let sub64 (a : int64) (b : int64) : int64 option =
    unwrap_int_result (F.proven_math_sub_checked a b)

  let mul64 (a : int64) (b : int64) : int64 option =
    unwrap_int_result (F.proven_math_mul_checked a b)
end

(* ========================================================================== *)
(* SafeString                                                                 *)
(* ========================================================================== *)

module SafeString = struct
  (** Check whether a byte sequence is valid UTF-8. *)
  let is_valid_utf8 (s : string) : bool =
    let p = cstring_of_string s in
    let r = F.proven_string_is_valid_utf8 p (Unsigned.Size_t.of_int (String.length s)) in
    match unwrap_bool_result r with Some v -> v | None -> false

  (** Escape a string for SQL (single-quote doubling). *)
  let escape_sql (s : string) : string option =
    let p = cstring_of_string s in
    unwrap_string_result (F.proven_string_escape_sql p (Unsigned.Size_t.of_int (String.length s)))

  (** Escape a string for HTML (< > & " '). *)
  let escape_html (s : string) : string option =
    let p = cstring_of_string s in
    unwrap_string_result (F.proven_string_escape_html p (Unsigned.Size_t.of_int (String.length s)))

  (** Escape a string for JavaScript (within quotes). *)
  let escape_js (s : string) : string option =
    let p = cstring_of_string s in
    unwrap_string_result (F.proven_string_escape_js p (Unsigned.Size_t.of_int (String.length s)))
end

(* ========================================================================== *)
(* SafePath                                                                   *)
(* ========================================================================== *)

module SafePath = struct
  (** Check whether a path attempts directory traversal. *)
  let has_traversal (s : string) : bool =
    let p = cstring_of_string s in
    let r = F.proven_path_has_traversal p (Unsigned.Size_t.of_int (String.length s)) in
    match unwrap_bool_result r with Some v -> v | None -> true

  (** Sanitize a filename by removing dangerous characters. *)
  let sanitize_filename (s : string) : string option =
    let p = cstring_of_string s in
    unwrap_string_result (F.proven_path_sanitize_filename p (Unsigned.Size_t.of_int (String.length s)))
end

(* ========================================================================== *)
(* SafeCrypto                                                                 *)
(* ========================================================================== *)

module SafeCrypto = struct
  (** Constant-time byte comparison (timing-safe). *)
  let constant_time_eq (a : string) (b : string) : bool =
    let pa = cstring_of_string a in
    let pb = cstring_of_string b in
    let r = F.proven_crypto_constant_time_eq
        pa (Unsigned.Size_t.of_int (String.length a))
        pb (Unsigned.Size_t.of_int (String.length b)) in
    match unwrap_bool_result r with Some v -> v | None -> false

  (** Fill a buffer with cryptographically secure random bytes.
      Returns [None] on failure. *)
  let random_bytes (n : int) : string option =
    if n <= 0 then Some ""
    else
      let buf = allocate_n char ~count:n in
      let rc = F.proven_crypto_random_bytes buf (Unsigned.Size_t.of_int n) in
      if Int32.equal rc 0l then begin
        let s = Bytes.create n in
        for i = 0 to n - 1 do
          Bytes.set s i !@(buf +@ i)
        done;
        Some (Bytes.to_string s)
      end else
        None
end

(* ========================================================================== *)
(* SafeEmail                                                                  *)
(* ========================================================================== *)

module SafeEmail = struct
  (** Check whether an email address is valid (RFC 5321 simplified). *)
  let is_valid (s : string) : bool =
    let p = cstring_of_string s in
    let r = F.proven_email_is_valid p (Unsigned.Size_t.of_int (String.length s)) in
    match unwrap_bool_result r with Some v -> v | None -> false
end

(* ========================================================================== *)
(* SafeNetwork                                                                *)
(* ========================================================================== *)

module SafeNetwork = struct
  (** Parse an IPv4 address string.  Returns the four octets on success. *)
  let parse_ipv4 (s : string) : (int * int * int * int) option =
    let p = cstring_of_string s in
    let r = F.proven_network_parse_ipv4 p (Unsigned.Size_t.of_int (String.length s)) in
    if status_ok (getf r F.ipv4r_status) then
      let addr = getf r F.ipv4r_address in
      let octets = getf addr F.ipv4_octets in
      Some (
        Unsigned.UInt8.to_int (CArray.get octets 0),
        Unsigned.UInt8.to_int (CArray.get octets 1),
        Unsigned.UInt8.to_int (CArray.get octets 2),
        Unsigned.UInt8.to_int (CArray.get octets 3)
      )
    else
      None

  (** Check whether an IPv4 address is in a private range (RFC 1918). *)
  let ipv4_is_private (a : int) (b : int) (c : int) (d : int) : bool =
    let addr = make F.ipv4_address in
    let octets = getf addr F.ipv4_octets in
    CArray.set octets 0 (Unsigned.UInt8.of_int a);
    CArray.set octets 1 (Unsigned.UInt8.of_int b);
    CArray.set octets 2 (Unsigned.UInt8.of_int c);
    CArray.set octets 3 (Unsigned.UInt8.of_int d);
    F.proven_network_ipv4_is_private addr

  (** Check whether an IPv4 address is loopback (127.0.0.0/8). *)
  let ipv4_is_loopback (a : int) (b : int) (c : int) (d : int) : bool =
    let addr = make F.ipv4_address in
    let octets = getf addr F.ipv4_octets in
    CArray.set octets 0 (Unsigned.UInt8.of_int a);
    CArray.set octets 1 (Unsigned.UInt8.of_int b);
    CArray.set octets 2 (Unsigned.UInt8.of_int c);
    CArray.set octets 3 (Unsigned.UInt8.of_int d);
    F.proven_network_ipv4_is_loopback addr
end

(* ========================================================================== *)
(* SafeHeader                                                                 *)
(* ========================================================================== *)

module SafeHeader = struct
  (** Check for CRLF injection characters in a header value. *)
  let has_crlf (s : string) : bool =
    let p = cstring_of_string s in
    let r = F.proven_header_has_crlf p (Unsigned.Size_t.of_int (String.length s)) in
    match unwrap_bool_result r with Some v -> v | None -> true

  (** Check whether a header name is valid per RFC 7230. *)
  let is_valid_name (s : string) : bool =
    let p = cstring_of_string s in
    let r = F.proven_header_is_valid_name p (Unsigned.Size_t.of_int (String.length s)) in
    match unwrap_bool_result r with Some v -> v | None -> false

  (** Check whether a header name is in the dangerous-headers list. *)
  let is_dangerous (s : string) : bool =
    let p = cstring_of_string s in
    let r = F.proven_header_is_dangerous p (Unsigned.Size_t.of_int (String.length s)) in
    match unwrap_bool_result r with Some v -> v | None -> true

  (** Render a validated header string "Name: Value". *)
  let render ~name ~value : string option =
    let pn = cstring_of_string name in
    let pv = cstring_of_string value in
    unwrap_string_result (F.proven_header_render
        pn (Unsigned.Size_t.of_int (String.length name))
        pv (Unsigned.Size_t.of_int (String.length value)))

  (** Build a Content-Security-Policy header value from JSON directives. *)
  let build_csp (directives_json : string) : string option =
    let p = cstring_of_string directives_json in
    unwrap_string_result (F.proven_header_build_csp p (Unsigned.Size_t.of_int (String.length directives_json)))

  (** Build an HSTS header value. *)
  let build_hsts ~max_age ~include_subdomains ~preload : string option =
    unwrap_string_result (F.proven_header_build_hsts (Int64.of_int max_age) include_subdomains preload)
end

(* ========================================================================== *)
(* SafeCookie                                                                 *)
(* ========================================================================== *)

module SafeCookie = struct
  (** Check for cookie injection characters. *)
  let has_injection (s : string) : bool =
    let p = cstring_of_string s in
    let r = F.proven_cookie_has_injection p (Unsigned.Size_t.of_int (String.length s)) in
    match unwrap_bool_result r with Some v -> v | None -> true

  (** Validate a cookie name. *)
  let validate_name (s : string) : bool =
    let p = cstring_of_string s in
    let r = F.proven_cookie_validate_name p (Unsigned.Size_t.of_int (String.length s)) in
    match unwrap_bool_result r with Some v -> v | None -> false

  (** Validate a cookie value. *)
  let validate_value (s : string) : bool =
    let p = cstring_of_string s in
    let r = F.proven_cookie_validate_value p (Unsigned.Size_t.of_int (String.length s)) in
    match unwrap_bool_result r with Some v -> v | None -> false

  (** Get cookie prefix type: 0=none, 1=__Secure-, 2=__Host-. *)
  let get_prefix (s : string) : int option =
    let p = cstring_of_string s in
    unwrap_int_result (F.proven_cookie_get_prefix p (Unsigned.Size_t.of_int (String.length s)))
    |> Option.map Int64.to_int

  (** Build a delete-cookie header value. *)
  let build_delete (name : string) : string option =
    let p = cstring_of_string name in
    unwrap_string_result (F.proven_cookie_build_delete p (Unsigned.Size_t.of_int (String.length name)))

  (* Note: build_set_cookie requires CookieAttributes struct marshaling.
     Add if needed by extending Proven_ffi with the struct definition. *)
end

(* ========================================================================== *)
(* SafeContentType                                                            *)
(* ========================================================================== *)

module SafeContentType = struct
  (** Check whether a content type could be sniffed to something dangerous. *)
  let can_sniff_dangerous (s : string) : bool =
    let p = cstring_of_string s in
    let r = F.proven_content_type_can_sniff_dangerous p (Unsigned.Size_t.of_int (String.length s)) in
    match unwrap_bool_result r with Some v -> v | None -> true

  (** Check whether a content type is JSON. *)
  let is_json ~subtype ~suffix : bool =
    let ps = cstring_of_string subtype in
    let pf = cstring_of_string suffix in
    let r = F.proven_content_type_is_json
        ps (Unsigned.Size_t.of_int (String.length subtype))
        pf (Unsigned.Size_t.of_int (String.length suffix)) in
    match unwrap_bool_result r with Some v -> v | None -> false

  (** Check whether a content type is XML. *)
  let is_xml ~subtype ~suffix : bool =
    let ps = cstring_of_string subtype in
    let pf = cstring_of_string suffix in
    let r = F.proven_content_type_is_xml
        ps (Unsigned.Size_t.of_int (String.length subtype))
        pf (Unsigned.Size_t.of_int (String.length suffix)) in
    match unwrap_bool_result r with Some v -> v | None -> false
end

(* ========================================================================== *)
(* SafeUUID                                                                   *)
(* ========================================================================== *)

module SafeUUID = struct
  (** Generate a random UUID v4.  Returns the 36-character string. *)
  let v4 () : string option =
    let r = F.proven_uuid_v4 () in
    if status_ok (getf r F.ur_status) then
      let u = getf r F.ur_uuid in
      let sr = F.proven_uuid_to_string u in
      unwrap_string_result sr
    else
      None

  (** Parse a UUID from its string representation. Returns the 36-char string
      if valid, [None] otherwise. *)
  let parse (s : string) : string option =
    let p = cstring_of_string s in
    let r = F.proven_uuid_parse p (Unsigned.Size_t.of_int (String.length s)) in
    if status_ok (getf r F.ur_status) then
      let u = getf r F.ur_uuid in
      unwrap_string_result (F.proven_uuid_to_string u)
    else
      None

  (** Check whether a UUID is nil (all zeros). *)
  let is_nil (s : string) : bool =
    let p = cstring_of_string s in
    let r = F.proven_uuid_parse p (Unsigned.Size_t.of_int (String.length s)) in
    if status_ok (getf r F.ur_status) then
      F.proven_uuid_is_nil (getf r F.ur_uuid)
    else
      false

  (** Get the UUID version number (1-8). *)
  let version (s : string) : int option =
    let p = cstring_of_string s in
    let r = F.proven_uuid_parse p (Unsigned.Size_t.of_int (String.length s)) in
    if status_ok (getf r F.ur_status) then
      Some (Unsigned.UInt8.to_int (F.proven_uuid_version (getf r F.ur_uuid)))
    else
      None

  (** Check whether a string is a valid UUID. *)
  let is_valid (s : string) : bool =
    let p = cstring_of_string s in
    let r = F.proven_uuid_parse p (Unsigned.Size_t.of_int (String.length s)) in
    status_ok (getf r F.ur_status)
end

(* ========================================================================== *)
(* SafeJson                                                                   *)
(* ========================================================================== *)

module SafeJson = struct
  (** Check whether a string is valid JSON. *)
  let is_valid (s : string) : bool =
    let p = cstring_of_string s in
    let r = F.proven_json_is_valid p (Unsigned.Size_t.of_int (String.length s)) in
    match unwrap_bool_result r with Some v -> v | None -> false

  (** JSON value types. *)
  type json_type =
    | Null
    | Bool
    | Number
    | String
    | Array
    | Object
    | Invalid

  (** Get the root-level JSON value type. *)
  let get_type (s : string) : json_type =
    let p = cstring_of_string s in
    let t = F.proven_json_get_type p (Unsigned.Size_t.of_int (String.length s)) in
    match Int32.to_int t with
    | 0 -> Null
    | 1 -> Bool
    | 2 -> Number
    | 3 -> String
    | 4 -> Array
    | 5 -> Object
    | _ -> Invalid
end

(* ========================================================================== *)
(* SafeDateTime                                                               *)
(* ========================================================================== *)

module SafeDateTime = struct
  (** Parsed date-time components. *)
  type t = {
    year : int;
    month : int;
    day : int;
    hour : int;
    minute : int;
    second : int;
    nanosecond : int;
    tz_offset_minutes : int;
  }

  (** Parse an ISO 8601 date/time string. *)
  let parse (s : string) : t option =
    let p = cstring_of_string s in
    let r = F.proven_datetime_parse p (Unsigned.Size_t.of_int (String.length s)) in
    if status_ok (getf r F.dtr_status) then
      let dt = getf r F.dtr_datetime in
      Some {
        year   = Int32.to_int (getf dt F.dt_year);
        month  = Unsigned.UInt8.to_int (getf dt F.dt_month);
        day    = Unsigned.UInt8.to_int (getf dt F.dt_day);
        hour   = Unsigned.UInt8.to_int (getf dt F.dt_hour);
        minute = Unsigned.UInt8.to_int (getf dt F.dt_minute);
        second = Unsigned.UInt8.to_int (getf dt F.dt_second);
        nanosecond = Unsigned.UInt32.to_int (getf dt F.dt_nanosecond);
        tz_offset_minutes = getf dt F.dt_tz_offset_minutes;
      }
    else
      None

  (** Format a date-time as ISO 8601. *)
  let format_iso8601 (t : t) : string option =
    let dt = make F.datetime in
    setf dt F.dt_year (Int32.of_int t.year);
    setf dt F.dt_month (Unsigned.UInt8.of_int t.month);
    setf dt F.dt_day (Unsigned.UInt8.of_int t.day);
    setf dt F.dt_hour (Unsigned.UInt8.of_int t.hour);
    setf dt F.dt_minute (Unsigned.UInt8.of_int t.minute);
    setf dt F.dt_second (Unsigned.UInt8.of_int t.second);
    setf dt F.dt_nanosecond (Unsigned.UInt32.of_int t.nanosecond);
    setf dt F.dt_tz_offset_minutes t.tz_offset_minutes;
    unwrap_string_result (F.proven_datetime_format_iso8601 dt)

  (** Check whether a year is a leap year. *)
  let is_leap_year (year : int) : bool =
    F.proven_datetime_is_leap_year (Int32.of_int year)

  (** Get the number of days in a month. *)
  let days_in_month ~year ~month : int =
    Unsigned.UInt8.to_int (F.proven_datetime_days_in_month (Int32.of_int year) (Unsigned.UInt8.of_int month))
end

(* ========================================================================== *)
(* SafeFloat                                                                  *)
(* ========================================================================== *)

module SafeFloat = struct
  (** Safe floating-point division. *)
  let div (a : float) (b : float) : float option =
    unwrap_float_result (F.proven_float_div a b)

  (** Check whether a float is finite (not NaN or Inf). *)
  let is_finite (x : float) : bool =
    F.proven_float_is_finite x

  (** Check whether a float is NaN. *)
  let is_nan (x : float) : bool =
    F.proven_float_is_nan x

  (** Safe square root (returns [None] for negative input). *)
  let sqrt (x : float) : float option =
    unwrap_float_result (F.proven_float_sqrt x)

  (** Safe natural logarithm (returns [None] for non-positive input). *)
  let ln (x : float) : float option =
    unwrap_float_result (F.proven_float_ln x)
end

(* ========================================================================== *)
(* SafePassword                                                               *)
(* ========================================================================== *)

module SafePassword = struct
  (** Password strength levels. *)
  type strength =
    | Very_weak
    | Weak
    | Fair
    | Strong
    | Very_strong

  (** Password validation result. *)
  type validation = {
    strength : strength;
    has_lowercase : bool;
    has_uppercase : bool;
    has_digit : bool;
    has_special : bool;
    length : int;
  }

  (** Validate password strength. *)
  let validate (s : string) : validation =
    let p = cstring_of_string s in
    let r = F.proven_password_validate p (Unsigned.Size_t.of_int (String.length s)) in
    let str = match Int32.to_int (getf r F.pw_strength) with
      | 0 -> Very_weak
      | 1 -> Weak
      | 2 -> Fair
      | 3 -> Strong
      | 4 -> Very_strong
      | _ -> Very_weak
    in
    {
      strength = str;
      has_lowercase = getf r F.pw_has_lowercase;
      has_uppercase = getf r F.pw_has_uppercase;
      has_digit = getf r F.pw_has_digit;
      has_special = getf r F.pw_has_special;
      length = Unsigned.Size_t.to_int (getf r F.pw_length);
    }

  (** Check whether a password is in the common-passwords list. *)
  let is_common (s : string) : bool =
    let p = cstring_of_string s in
    F.proven_password_is_common p (Unsigned.Size_t.of_int (String.length s))
end

(* ========================================================================== *)
(* SafeVersion (semver)                                                       *)
(* ========================================================================== *)

module SafeVersion = struct
  (** Parse a semantic version string. Returns (major, minor, patch) on
      success, or [None] on parse failure. *)
  let parse (s : string) : (int * int * int) option =
    let p = cstring_of_string s in
    let r = F.proven_version_parse_semver p (Unsigned.Size_t.of_int (String.length s)) in
    if status_ok (getf r F.vr_status) then begin
      let v = getf r F.vr_version in
      let major = Unsigned.UInt32.to_int (getf v F.sv_major) in
      let minor = Unsigned.UInt32.to_int (getf v F.sv_minor) in
      let patch = Unsigned.UInt32.to_int (getf v F.sv_patch) in
      (* Free the version result (may contain prerelease string) *)
      let vp = allocate F.semantic_version v in
      F.proven_version_free vp;
      Some (major, minor, patch)
    end else
      None

  (** Compare two version strings.  Returns [Some n] where [n < 0] means
      [a < b], [n = 0] means equal, [n > 0] means [a > b].
      Returns [None] if either string fails to parse. *)
  let compare (a_str : string) (b_str : string) : int option =
    let pa = cstring_of_string a_str in
    let ra = F.proven_version_parse_semver pa (Unsigned.Size_t.of_int (String.length a_str)) in
    if not (status_ok (getf ra F.vr_status)) then None
    else begin
      let pb = cstring_of_string b_str in
      let rb = F.proven_version_parse_semver pb (Unsigned.Size_t.of_int (String.length b_str)) in
      if not (status_ok (getf rb F.vr_status)) then begin
        let vpa = allocate F.semantic_version (getf ra F.vr_version) in
        F.proven_version_free vpa;
        None
      end else begin
        let va = getf ra F.vr_version in
        let vb = getf rb F.vr_version in
        let result = Int32.to_int (F.proven_version_compare va vb) in
        let vpa = allocate F.semantic_version va in
        let vpb = allocate F.semantic_version vb in
        F.proven_version_free vpa;
        F.proven_version_free vpb;
        Some result
      end
    end
end

(* ========================================================================== *)
(* SafeGeo                                                                    *)
(* ========================================================================== *)

module SafeGeo = struct
  (** Validate and normalize a geographic coordinate.
      Returns [(latitude, longitude)] on success. *)
  let validate ~lat ~lon : (float * float) option =
    let r = F.proven_geo_validate lat lon in
    if status_ok (getf r F.geor_status) then
      let coord = getf r F.geor_coordinate in
      Some (getf coord F.geo_latitude, getf coord F.geo_longitude)
    else
      None

  (** Calculate distance between two points in meters (Haversine). *)
  let distance ~lat1 ~lon1 ~lat2 ~lon2 : float option =
    let a = make F.geo_coordinate in
    setf a F.geo_latitude lat1;
    setf a F.geo_longitude lon1;
    let b = make F.geo_coordinate in
    setf b F.geo_latitude lat2;
    setf b F.geo_longitude lon2;
    unwrap_float_result (F.proven_geo_distance a b)

  (** Check whether a coordinate is within a bounding box. *)
  let in_bounds ~lat ~lon ~min_lat ~max_lat ~min_lon ~max_lon : bool =
    let coord = make F.geo_coordinate in
    setf coord F.geo_latitude lat;
    setf coord F.geo_longitude lon;
    F.proven_geo_in_bounds coord min_lat max_lat min_lon max_lon
end

(* ========================================================================== *)
(* SafeChecksum                                                               *)
(* ========================================================================== *)

module SafeChecksum = struct
  (** Calculate CRC32 of a byte string. *)
  let crc32 (s : string) : int option =
    let p = cstring_of_string s in
    unwrap_int_result (F.proven_checksum_crc32 p (Unsigned.Size_t.of_int (String.length s)))
    |> Option.map Int64.to_int

  (** Verify that CRC32 of data matches an expected value. *)
  let verify_crc32 (s : string) (expected : int) : bool =
    let p = cstring_of_string s in
    let r = F.proven_checksum_verify_crc32 p
        (Unsigned.Size_t.of_int (String.length s))
        (Unsigned.UInt32.of_int expected) in
    match unwrap_bool_result r with Some v -> v | None -> false
end

(* ========================================================================== *)
(* SafeProbability                                                            *)
(* ========================================================================== *)

module SafeProbability = struct
  (** Create a probability value, clamped to [0, 1]. *)
  let create (v : float) : float =
    F.proven_probability_create v

  (** Probability of independent events both occurring (a * b). *)
  let both (a : float) (b : float) : float =
    F.proven_probability_and a b

  (** Probability of mutually exclusive events (a + b). *)
  let either_exclusive (a : float) (b : float) : float =
    F.proven_probability_or_exclusive a b

  (** Complement probability (1 - p). *)
  let complement (p : float) : float =
    F.proven_probability_not p
end

(* ========================================================================== *)
(* SafeCalculator                                                             *)
(* ========================================================================== *)

module SafeCalculator = struct
  (** Evaluate a mathematical expression string. *)
  let eval (expr : string) : float option =
    let p = cstring_of_string expr in
    unwrap_float_result (F.proven_calculator_eval p (Unsigned.Size_t.of_int (String.length expr)))
end

(* ========================================================================== *)
(* SafeHex                                                                    *)
(* ========================================================================== *)

module SafeHex = struct
  (** Hex-encode bytes.  [uppercase] selects A-F vs a-f. *)
  let encode ?(uppercase = false) (s : string) : string option =
    let p = cstring_of_string s in
    unwrap_string_result (F.proven_hex_encode p (Unsigned.Size_t.of_int (String.length s)) uppercase)

  (** Hex-decode a string to bytes. *)
  let decode (hex : string) : string option =
    let p = cstring_of_string hex in
    let r = F.proven_hex_decode p (Unsigned.Size_t.of_int (String.length hex)) in
    if status_ok (getf r F.hdr_status) then begin
      let data = getf r F.hdr_data in
      let len = Unsigned.Size_t.to_int (getf r F.hdr_length) in
      if is_null data then None
      else begin
        let s = Bytes.create len in
        for i = 0 to len - 1 do
          Bytes.set s i (Char.chr (Unsigned.UInt8.to_int !@(data +@ i)))
        done;
        let rp = allocate F.hex_decode_result r in
        F.proven_hex_free rp;
        Some (Bytes.to_string s)
      end
    end else
      None
end

(* ========================================================================== *)
(* SafeCurrency                                                               *)
(* ========================================================================== *)

module SafeCurrency = struct
  (** Parsed currency amount. *)
  type t = {
    amount_minor : int64;
    currency_code : string;
    decimal_places : int;
  }

  (** Parse a currency string (e.g., "USD 123.45"). *)
  let parse (s : string) : t option =
    let p = cstring_of_string s in
    let r = F.proven_currency_parse p (Unsigned.Size_t.of_int (String.length s)) in
    if status_ok (getf r F.cur_status) then begin
      let code_arr = getf r F.cur_currency_code in
      let code = Printf.sprintf "%c%c%c"
          (Char.chr (Unsigned.UInt8.to_int (CArray.get code_arr 0)))
          (Char.chr (Unsigned.UInt8.to_int (CArray.get code_arr 1)))
          (Char.chr (Unsigned.UInt8.to_int (CArray.get code_arr 2))) in
      Some {
        amount_minor = getf r F.cur_amount_minor;
        currency_code = code;
        decimal_places = Unsigned.UInt8.to_int (getf r F.cur_decimal_places);
      }
    end else
      None

  (** Format a currency amount to string. *)
  let format ~amount_minor ~currency_code ~decimal_places : string option =
    if String.length currency_code <> 3 then None
    else begin
      let code = CArray.make uint8_t 3 in
      CArray.set code 0 (Unsigned.UInt8.of_int (Char.code currency_code.[0]));
      CArray.set code 1 (Unsigned.UInt8.of_int (Char.code currency_code.[1]));
      CArray.set code 2 (Unsigned.UInt8.of_int (Char.code currency_code.[2]));
      unwrap_string_result (F.proven_currency_format amount_minor code (Unsigned.UInt8.of_int decimal_places))
    end
end

(* ========================================================================== *)
(* SafePhone                                                                  *)
(* ========================================================================== *)

module SafePhone = struct
  (** Parsed phone number. *)
  type t = {
    country_code : int;
    national_number : int64;
    is_valid : bool;
  }

  (** Parse a phone number to E.164 components. *)
  let parse (s : string) : t option =
    let p = cstring_of_string s in
    let r = F.proven_phone_parse p (Unsigned.Size_t.of_int (String.length s)) in
    if status_ok (getf r F.phr_status) then
      Some {
        country_code = Unsigned.UInt16.to_int (getf r F.phr_country_code);
        national_number = Unsigned.UInt64.to_int64 (getf r F.phr_national_number);
        is_valid = getf r F.phr_is_valid;
      }
    else
      None

  (** Format a phone number as E.164 string. *)
  let format_e164 ~country_code ~national_number : string option =
    unwrap_string_result (F.proven_phone_format_e164
        (Unsigned.UInt16.of_int country_code)
        (Unsigned.UInt64.of_int64 national_number))
end

(* ========================================================================== *)
(* SafeColor                                                                  *)
(* ========================================================================== *)

module SafeColor = struct
  (** RGB color. *)
  type rgb = { r : int; g : int; b : int }

  (** HSL color. *)
  type hsl = { h : float; s : float; l : float }

  (** Parse a hex color string (#RRGGBB or #RGB). *)
  let parse_hex (s : string) : rgb option =
    let p = cstring_of_string s in
    let r = F.proven_color_parse_hex p (Unsigned.Size_t.of_int (String.length s)) in
    if status_ok (getf r F.cpr_status) then begin
      let c = getf r F.cpr_color in
      Some {
        r = Unsigned.UInt8.to_int (getf c F.rgb_r);
        g = Unsigned.UInt8.to_int (getf c F.rgb_g);
        b = Unsigned.UInt8.to_int (getf c F.rgb_b);
      }
    end else
      None

  (** Convert RGB to HSL. *)
  let rgb_to_hsl (color : rgb) : hsl =
    let c = make F.rgb_color in
    setf c F.rgb_r (Unsigned.UInt8.of_int color.r);
    setf c F.rgb_g (Unsigned.UInt8.of_int color.g);
    setf c F.rgb_b (Unsigned.UInt8.of_int color.b);
    let h = F.proven_color_rgb_to_hsl c in
    { h = getf h F.hsl_h; s = getf h F.hsl_s; l = getf h F.hsl_l }

  (** Format RGB as hex string. *)
  let to_hex (color : rgb) : string option =
    let c = make F.rgb_color in
    setf c F.rgb_r (Unsigned.UInt8.of_int color.r);
    setf c F.rgb_g (Unsigned.UInt8.of_int color.g);
    setf c F.rgb_b (Unsigned.UInt8.of_int color.b);
    unwrap_string_result (F.proven_color_to_hex c)
end

(* ========================================================================== *)
(* SafeAngle                                                                  *)
(* ========================================================================== *)

module SafeAngle = struct
  (** Convert degrees to radians. *)
  let deg_to_rad (degrees : float) : float =
    F.proven_angle_deg_to_rad degrees

  (** Convert radians to degrees. *)
  let rad_to_deg (radians : float) : float =
    F.proven_angle_rad_to_deg radians

  (** Normalize angle to [0, 360) degrees. *)
  let normalize_degrees (degrees : float) : float =
    F.proven_angle_normalize_degrees degrees

  (** Normalize angle to [0, 2*pi) radians. *)
  let normalize_radians (radians : float) : float =
    F.proven_angle_normalize_radians radians
end

(* ========================================================================== *)
(* SafeUnit                                                                   *)
(* ========================================================================== *)

module SafeUnit = struct
  (** Length unit identifiers (matching libproven LengthUnit enum). *)
  type length_unit =
    | Meters | Kilometers | Centimeters | Millimeters
    | Feet | Inches | Miles | Yards

  let length_unit_to_int = function
    | Meters -> 0 | Kilometers -> 1 | Centimeters -> 2 | Millimeters -> 3
    | Feet -> 4 | Inches -> 5 | Miles -> 6 | Yards -> 7

  (** Temperature unit identifiers (matching libproven TempUnit enum). *)
  type temp_unit = Celsius | Fahrenheit | Kelvin

  let temp_unit_to_int = function
    | Celsius -> 0 | Fahrenheit -> 1 | Kelvin -> 2

  (** Convert a length value between units. *)
  let convert_length ~from ~to_ (value : float) : float option =
    unwrap_float_result (F.proven_unit_convert_length value
        (Int32.of_int (length_unit_to_int from))
        (Int32.of_int (length_unit_to_int to_)))

  (** Convert a temperature value between units. *)
  let convert_temp ~from ~to_ (value : float) : float option =
    unwrap_float_result (F.proven_unit_convert_temp value
        (Int32.of_int (temp_unit_to_int from))
        (Int32.of_int (temp_unit_to_int to_)))
end

(* ========================================================================== *)
(* SafeHttp                                                                   *)
(* ========================================================================== *)

module SafeHttp = struct
  (** URL-encode a string (RFC 3986 percent encoding). *)
  let url_encode (s : string) : string option =
    let p = cstring_of_string s in
    unwrap_string_result (F.proven_http_url_encode p (Unsigned.Size_t.of_int (String.length s)))

  (** URL-decode a percent-encoded string. *)
  let url_decode (s : string) : string option =
    let p = cstring_of_string s in
    unwrap_string_result (F.proven_http_url_decode p (Unsigned.Size_t.of_int (String.length s)))
end

(* ========================================================================== *)
(* SafeBuffer                                                                 *)
(* ========================================================================== *)

module SafeBuffer = struct
  (** Opaque buffer handle. *)
  type t = F.bounded_buffer structure ptr

  (** Create a bounded buffer with the given capacity. *)
  let create (capacity : int) : t option =
    let r = F.proven_buffer_create (Unsigned.Size_t.of_int capacity) in
    if status_ok (getf r F.bufr_status) then
      let buf = getf r F.bufr_buffer in
      if is_null buf then None
      else Some buf
    else
      None

  (** Append data to the buffer. Returns [true] on success. *)
  let append (buf : t) (data : string) : bool =
    let p = cstring_of_string data in
    let rc = F.proven_buffer_append buf p (Unsigned.Size_t.of_int (String.length data)) in
    Int32.equal rc 0l

  (** Free the buffer. *)
  let free (buf : t) : unit =
    F.proven_buffer_free buf
end

(* ========================================================================== *)
(* SafeRateLimiter                                                            *)
(* ========================================================================== *)

module SafeRateLimiter = struct
  (** Opaque rate limiter handle. *)
  type t = F.rate_limiter structure ptr

  (** Create a token-bucket rate limiter. *)
  let create ~capacity ~refill_rate : t option =
    F.proven_rate_limiter_create capacity refill_rate

  (** Try to acquire [tokens] from the limiter. Returns [true] if allowed. *)
  let try_acquire (rl : t) (tokens : float) : bool =
    F.proven_rate_limiter_try_acquire rl tokens

  (** Free the rate limiter. *)
  let free (rl : t) : unit =
    F.proven_rate_limiter_free rl
end

(* ========================================================================== *)
(* SafeCircuitBreaker                                                         *)
(* ========================================================================== *)

module SafeCircuitBreaker = struct
  (** Circuit breaker states. *)
  type state = Closed | Open | Half_open

  (** Opaque circuit breaker handle. *)
  type t = F.circuit_breaker structure ptr

  (** Create a circuit breaker. *)
  let create ~failure_threshold ~success_threshold ~timeout_ms : t option =
    F.proven_circuit_breaker_create
      (Unsigned.UInt32.of_int failure_threshold)
      (Unsigned.UInt32.of_int success_threshold)
      (Int64.of_int timeout_ms)

  (** Check whether a request should be allowed. *)
  let allow (cb : t) : bool =
    F.proven_circuit_breaker_allow cb

  (** Record a successful request. *)
  let success (cb : t) : unit =
    F.proven_circuit_breaker_success cb

  (** Record a failed request. *)
  let failure (cb : t) : unit =
    F.proven_circuit_breaker_failure cb

  (** Get the current circuit breaker state. *)
  let state (cb : t) : state =
    match Int32.to_int (F.proven_circuit_breaker_state cb) with
    | 0 -> Closed
    | 1 -> Open
    | 2 -> Half_open
    | _ -> Open

  (** Free the circuit breaker. *)
  let free (cb : t) : unit =
    F.proven_circuit_breaker_free cb
end

(* ========================================================================== *)
(* SafeRetry                                                                  *)
(* ========================================================================== *)

module SafeRetry = struct
  (** Retry configuration. *)
  type config = {
    max_attempts : int;
    base_delay_ms : int;
    max_delay_ms : int;
    multiplier : float;
  }

  let make_config (c : config) : F.retry_config structure =
    let rc = make F.retry_config in
    setf rc F.rc_max_attempts (Unsigned.UInt32.of_int c.max_attempts);
    setf rc F.rc_base_delay_ms (Unsigned.UInt64.of_int c.base_delay_ms);
    setf rc F.rc_max_delay_ms (Unsigned.UInt64.of_int c.max_delay_ms);
    setf rc F.rc_multiplier c.multiplier;
    rc

  (** Calculate the delay for a given attempt number. *)
  let delay (config : config) (attempt : int) : int =
    Unsigned.UInt64.to_int (F.proven_retry_delay (make_config config) (Unsigned.UInt32.of_int attempt))

  (** Check whether a retry should be attempted. *)
  let should_retry (config : config) (attempt : int) : bool =
    F.proven_retry_should_retry (make_config config) (Unsigned.UInt32.of_int attempt)
end

(* ========================================================================== *)
(* SafeMonotonic                                                              *)
(* ========================================================================== *)

module SafeMonotonic = struct
  (** Opaque monotonic counter handle. *)
  type t = F.monotonic_counter structure ptr

  (** Create a monotonic counter with initial value and maximum. *)
  let create ~initial ~max_value : t option =
    F.proven_monotonic_create
      (Unsigned.UInt64.of_int initial)
      (Unsigned.UInt64.of_int max_value)

  (** Get the next value from the counter. Returns [None] at overflow. *)
  let next (c : t) : int64 option =
    unwrap_int_result (F.proven_monotonic_next c)

  (** Free the counter. *)
  let free (c : t) : unit =
    F.proven_monotonic_free c
end

(* ========================================================================== *)
(* SafeStateMachine                                                           *)
(* ========================================================================== *)

module SafeStateMachine = struct
  (** Opaque state machine handle. *)
  type t = F.state_machine structure ptr

  (** Create a state machine with [state_count] states and an initial state. *)
  let create ~state_count ~initial_state : t option =
    F.proven_state_machine_create
      (Unsigned.UInt32.of_int state_count)
      (Unsigned.UInt32.of_int initial_state)

  (** Allow a transition from one state to another. Returns success. *)
  let allow_transition (sm : t) ~from ~to_ : bool =
    F.proven_state_machine_allow sm
      (Unsigned.UInt32.of_int from) (Unsigned.UInt32.of_int to_)

  (** Try to transition to a new state. Returns [true] if the transition
      was allowed and completed. *)
  let transition (sm : t) ~to_ : bool =
    F.proven_state_machine_transition sm (Unsigned.UInt32.of_int to_)

  (** Get the current state. *)
  let current_state (sm : t) : int =
    Unsigned.UInt32.to_int (F.proven_state_machine_state sm)

  (** Free the state machine. *)
  let free (sm : t) : unit =
    F.proven_state_machine_free sm
end

(* ========================================================================== *)
(* SafeTensor                                                                 *)
(* ========================================================================== *)

module SafeTensor = struct
  (** Opaque 2D tensor handle. *)
  type t = F.tensor_2d structure ptr

  (** Create a [rows x cols] tensor (initialized to zero). *)
  let create ~rows ~cols : t option =
    F.proven_tensor_create
      (Unsigned.Size_t.of_int rows)
      (Unsigned.Size_t.of_int cols)

  (** Set a value at (row, col). Returns [true] on success. *)
  let set (t : t) ~row ~col (value : float) : bool =
    let rc = F.proven_tensor_set t
        (Unsigned.Size_t.of_int row)
        (Unsigned.Size_t.of_int col) value in
    Int32.equal rc 0l

  (** Get a value at (row, col). *)
  let get (t : t) ~row ~col : float option =
    unwrap_float_result (F.proven_tensor_get t
        (Unsigned.Size_t.of_int row)
        (Unsigned.Size_t.of_int col))

  (** Matrix multiplication.  Returns [None] if dimensions are incompatible. *)
  let matmul (a : t) (b : t) : t option =
    F.proven_tensor_matmul a b

  (** Free the tensor. *)
  let free (t : t) : unit =
    F.proven_tensor_free t
end

(* ========================================================================== *)
(* SafeMl                                                                     *)
(* ========================================================================== *)

module SafeMl = struct
  (** Sigmoid activation function. *)
  let sigmoid (x : float) : float =
    F.proven_ml_sigmoid x

  (** ReLU activation function. *)
  let relu (x : float) : float =
    F.proven_ml_relu x

  (** Leaky ReLU activation function. *)
  let leaky_relu ?(alpha = 0.01) (x : float) : float =
    F.proven_ml_leaky_relu x alpha

  (** Clamp value to [min_val, max_val]. *)
  let clamp ~min_val ~max_val (x : float) : float =
    F.proven_ml_clamp x min_val max_val
end

(* ========================================================================== *)
(* SafeLru                                                                    *)
(* ========================================================================== *)

module SafeLru = struct
  (** Opaque LRU cache handle. *)
  type t = F.lru_cache structure ptr

  (** Create an LRU cache with the given capacity. *)
  let create (capacity : int) : t option =
    F.proven_lru_create (Unsigned.Size_t.of_int capacity)

  (** Get a value by key.  Returns [None] on miss. *)
  let get (cache : t) (key : int) : int64 option =
    unwrap_int_result (F.proven_lru_get cache (Unsigned.UInt64.of_int key))

  (** Put a key-value pair.  Returns [true] on success. *)
  let put (cache : t) (key : int) (value : int64) : bool =
    let rc = F.proven_lru_put cache (Unsigned.UInt64.of_int key) value in
    Int32.equal rc 0l

  (** Free the cache. *)
  let free (cache : t) : unit =
    F.proven_lru_free cache
end

(* ========================================================================== *)
(* SafeGraph                                                                  *)
(* ========================================================================== *)

module SafeGraph = struct
  (** Opaque graph handle. *)
  type t = F.graph structure ptr

  (** Create a graph with the given number of nodes. *)
  let create (node_count : int) : t option =
    F.proven_graph_create (Unsigned.Size_t.of_int node_count)

  (** Add a directed edge.  Returns [true] on success. *)
  let add_edge (g : t) ~from ~to_ : bool =
    let rc = F.proven_graph_add_edge g
        (Unsigned.Size_t.of_int from)
        (Unsigned.Size_t.of_int to_) in
    Int32.equal rc 0l

  (** Check whether a directed edge exists. *)
  let has_edge (g : t) ~from ~to_ : bool =
    F.proven_graph_has_edge g
      (Unsigned.Size_t.of_int from)
      (Unsigned.Size_t.of_int to_)

  (** Free the graph. *)
  let free (g : t) : unit =
    F.proven_graph_free g
end

(* ========================================================================== *)
(* SafeQueue                                                                  *)
(* ========================================================================== *)

module SafeQueue = struct
  (** Opaque bounded queue handle. *)
  type t = F.bounded_queue structure ptr

  (** Create a bounded FIFO queue with the given capacity. *)
  let create (capacity : int) : t option =
    F.proven_queue_create (Unsigned.Size_t.of_int capacity)

  (** Push a value.  Returns [true] on success, [false] if full. *)
  let push (q : t) (value : int64) : bool =
    F.proven_queue_push q value

  (** Pop a value.  Returns [None] if empty. *)
  let pop (q : t) : int64 option =
    unwrap_int_result (F.proven_queue_pop q)

  (** Get current queue size. *)
  let size (q : t) : int =
    Unsigned.Size_t.to_int (F.proven_queue_size q)

  (** Free the queue. *)
  let free (q : t) : unit =
    F.proven_queue_free q
end

(* ========================================================================== *)
(* SafeBloom                                                                  *)
(* ========================================================================== *)

module SafeBloom = struct
  (** Opaque bloom filter handle. *)
  type t = F.bloom_filter structure ptr

  (** Create a bloom filter. *)
  let create ~expected_elements ~false_positive_rate : t option =
    F.proven_bloom_create
      (Unsigned.Size_t.of_int expected_elements)
      false_positive_rate

  (** Add an element to the filter. *)
  let add (bf : t) (s : string) : unit =
    let p = cstring_of_string s in
    F.proven_bloom_add bf p (Unsigned.Size_t.of_int (String.length s))

  (** Check whether an element might be in the filter. *)
  let contains (bf : t) (s : string) : bool =
    let p = cstring_of_string s in
    F.proven_bloom_contains bf p (Unsigned.Size_t.of_int (String.length s))

  (** Free the bloom filter. *)
  let free (bf : t) : unit =
    F.proven_bloom_free bf
end

(* ========================================================================== *)
(* Callbacks                                                                  *)
(* ========================================================================== *)

module Callbacks = struct
  (** Event types that can trigger callbacks. *)
  type event_type =
    | Validation_failed
    | Resource_acquired
    | Resource_released
    | Rate_limit_hit
    | Circuit_state_change
    | Signal_received
    | Retry_attempt
    | Crypto_operation
    | Custom

  let event_type_to_int = function
    | Validation_failed -> 1l
    | Resource_acquired -> 2l
    | Resource_released -> 3l
    | Rate_limit_hit -> 4l
    | Circuit_state_change -> 5l
    | Signal_received -> 6l
    | Retry_attempt -> 7l
    | Crypto_operation -> 8l
    | Custom -> 100l

  (** Query how many callbacks are registered for an event type. *)
  let count (et : event_type) : int =
    Unsigned.UInt32.to_int (F.proven_callback_count (event_type_to_int et))

  (** Unregister all callbacks.  Returns the number removed. *)
  let clear_all () : int =
    Unsigned.UInt32.to_int (F.proven_callback_clear_all ())
end
