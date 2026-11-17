(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> *)

(** Low-level ctypes FFI bindings to libproven.

    Binds only symbols whose row in the C ABI status ledger
    ([bindings/c/include/proven-abi.tsv]) is [exported]: present in
    libproven with the signature declared in [proven.h]. Every other
    [proven.h] function is an [owed] or [conflict] row and is deliberately
    not bound here; binding it eagerly would make this library fail to load.
    [scripts/full-api-audit.sh] enforces this against the real library.

    Users should prefer the safe wrappers in {!Proven} instead. *)

open Ctypes
open Foreign

(* -------------------------------------------------------------------------- *)
(* Library handle                                                             *)
(* -------------------------------------------------------------------------- *)

let lib = Dl.dlopen ~filename:"libproven.so" ~flags:[Dl.RTLD_NOW; Dl.RTLD_GLOBAL]

(* -------------------------------------------------------------------------- *)
(* Enum: ProvenStatus                                                         *)
(* -------------------------------------------------------------------------- *)

(** Status codes returned by libproven functions. *)
let proven_status_ok                = 0l
let proven_status_err_null_pointer  = -1l
let proven_status_err_invalid_arg   = -2l
let proven_status_err_overflow      = -3l
let proven_status_err_underflow     = -4l
let proven_status_err_div_by_zero   = -5l
let proven_status_err_parse_failure = -6l
let proven_status_err_validation    = -7l
let proven_status_err_out_of_bounds = -8l
let proven_status_err_encoding      = -9l
let proven_status_err_alloc_failed  = -10l

(* -------------------------------------------------------------------------- *)
(* Struct: BoolResult { status: i32, value: bool }                            *)
(* -------------------------------------------------------------------------- *)

type bool_result
let bool_result : bool_result structure typ = structure "BoolResult"
let br_status = field bool_result "status" int32_t
let br_value  = field bool_result "value"  bool
let () = seal bool_result

(* ========================================================================== *)
(* Function bindings: ledger status `exported`                                *)
(* ========================================================================== *)

(* -- Memory management ----------------------------------------------------- *)

let proven_free_string =
  foreign ~from:lib "proven_free_string"
    (ptr char @-> returning void)

(* -- Version info ---------------------------------------------------------- *)

let proven_version_major =
  foreign ~from:lib "proven_version_major"
    (void @-> returning uint32_t)

let proven_version_minor =
  foreign ~from:lib "proven_version_minor"
    (void @-> returning uint32_t)

let proven_version_patch =
  foreign ~from:lib "proven_version_patch"
    (void @-> returning uint32_t)

let proven_ffi_abi_version =
  foreign ~from:lib "proven_ffi_abi_version"
    (void @-> returning uint32_t)

(* -- SafePath (zig-legacy backend) ----------------------------------------- *)

let proven_path_has_traversal =
  foreign ~from:lib "proven_path_has_traversal"
    (ptr char @-> size_t @-> returning bool_result)

(* -- SafeHeader (idris-refc backend) --------------------------------------- *)

let proven_header_has_crlf =
  foreign ~from:lib "proven_header_has_crlf"
    (ptr char @-> size_t @-> returning bool_result)
