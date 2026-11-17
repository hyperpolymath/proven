(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> *)

(** Proven: safety primitives for OCaml, bound to libproven.

    This library marshals data only. It exposes exactly the libproven
    functions whose C ABI status ledger row ([proven-abi.tsv] beside
    [proven.h]) is [exported]. Each wrapper names its backend:

    - [SafeHeader.has_crlf] is computed by the compiled Idris core (RefC).
    - [SafePath.has_traversal] and [Version] are computed in Zig and are not
      Idris-verified implementations.

    The other [proven.h] functions are owed ABI obligations, not available
    functions. Every public function returns a plain value or [option]; none
    raises. *)

open Ctypes
module F = Proven_ffi

(* ========================================================================== *)
(* Internal helpers                                                           *)
(* ========================================================================== *)

(** Copy an OCaml string into a NUL-terminated buffer the caller owns.
    Embedded NUL bytes are data: the length passed to C is [String.length]. *)
let cstring_of_string (s : string) : char ptr =
  let len = String.length s in
  let buf = allocate_n char ~count:(len + 1) in
  for i = 0 to len - 1 do
    (buf +@ i) <-@ s.[i]
  done;
  (buf +@ len) <-@ '\000';
  buf

(** Unwrap a BoolResult into [Some bool] if ok, [None] otherwise. *)
let unwrap_bool_result (r : F.bool_result structure) : bool option =
  if Int32.equal (getf r F.br_status) F.proven_status_ok then
    Some (getf r F.br_value)
  else
    None

let check_span call (s : string) : bool option =
  let p = cstring_of_string s in
  unwrap_bool_result (call p (Unsigned.Size_t.of_int (String.length s)))

(* ========================================================================== *)
(* Version (zig-legacy)                                                       *)
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
end

(* ========================================================================== *)
(* SafePath (zig-legacy)                                                      *)
(* ========================================================================== *)

module SafePath = struct
  (** Check whether a path attempts directory traversal. Fails closed: a
      library error is reported as [true]. Use {!has_traversal_result} to
      distinguish errors. *)
  let has_traversal_result (s : string) : bool option =
    check_span F.proven_path_has_traversal s

  let has_traversal (s : string) : bool =
    match has_traversal_result s with Some v -> v | None -> true
end

(* ========================================================================== *)
(* SafeHeader (idris-refc)                                                    *)
(* ========================================================================== *)

module SafeHeader = struct
  (** Check every byte of a header value for CR or LF. Fails closed: a
      library error (including an unavailable Idris backend) is reported as
      [true]. Use {!has_crlf_result} to distinguish errors. *)
  let has_crlf_result (s : string) : bool option =
    check_span F.proven_header_has_crlf s

  let has_crlf (s : string) : bool =
    match has_crlf_result s with Some v -> v | None -> true
end
