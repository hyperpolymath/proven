(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)
(* Only the five declared wired symbols; no global lifecycle or broad API. *)
open Ctypes

let lib = Dl.dlopen ~filename:"libproven.so" ~flags:[Dl.RTLD_NOW; Dl.RTLD_LOCAL]
type bool_result
let bool_result : bool_result structure typ = structure "ProvenBoolResult"
let status = field bool_result "status" int32_t
let value = field bool_result "value" bool
let () = seal bool_result
let header = Foreign.foreign ~from:lib "proven_header_has_crlf"
  (ptr char @-> size_t @-> returning bool_result)
let path = Foreign.foreign ~from:lib "proven_path_has_traversal"
  (ptr char @-> size_t @-> returning bool_result)
let _free_string = Foreign.foreign ~from:lib "proven_free_string"
  (ptr char @-> returning void)
let version = Foreign.foreign ~from:lib "proven_version"
  (void @-> returning string)
let build_info = Foreign.foreign ~from:lib "proven_build_info"
  (void @-> returning string)

let check_bytes call bytes =
  let len = String.length bytes in
  (* Own a non-null span even for empty input. Embedded NUL is copied as data. *)
  let buffer = allocate_n char ~count:(len + 1) in
  for i = 0 to len - 1 do (buffer +@ i) <-@ bytes.[i] done;
  (buffer +@ len) <-@ '\000';
  let result = call buffer (Unsigned.Size_t.of_int len) in
  if Int32.equal (getf result status) 0l then Some (getf result value) else None

module SafeHeader = struct
  let has_crlf bytes = check_bytes header bytes
end
module SafePath = struct
  let has_traversal bytes = check_bytes path bytes
end
module Version = struct
  let version () = version ()
  let build_info () = build_info ()
end
