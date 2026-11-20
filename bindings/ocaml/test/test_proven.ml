(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> *)

(** Tests for the Proven OCaml FFI bindings.

    These tests call through to libproven via ctypes.  They require
    [libproven.so] to be loadable at runtime (e.g., via LD_LIBRARY_PATH).
    Only ledger-[exported] functions exist in the binding; owed functions
    are tracked in [proven-abi.tsv], not tested here. *)

(* -------------------------------------------------------------------------- *)
(* SafePath                                                                   *)
(* -------------------------------------------------------------------------- *)

let test_path_traversal () =
  Alcotest.(check bool) "../ detected" true (Proven.SafePath.has_traversal "../etc");
  Alcotest.(check bool) "safe path" false (Proven.SafePath.has_traversal "normal/path");
  Alcotest.(check (option bool)) "status ok" (Some false)
    (Proven.SafePath.has_traversal_result "normal/path")

(* -------------------------------------------------------------------------- *)
(* SafeHeader                                                                 *)
(* -------------------------------------------------------------------------- *)

let test_header_crlf () =
  List.iter (fun (value, expected) ->
      Alcotest.(check (option bool)) (String.escaped value) (Some expected)
        (Proven.SafeHeader.has_crlf_result value))
    [ "", false; "application/json", false; "\r", true; "\n", true;
      "a\000\rZ", true; "\255\000\128", false ]

(* -------------------------------------------------------------------------- *)
(* Version                                                                    *)
(* -------------------------------------------------------------------------- *)

let test_version_info () =
  Alcotest.(check bool) "major >= 0" true (Proven.Version.major () >= 0);
  Alcotest.(check bool) "minor >= 0" true (Proven.Version.minor () >= 0);
  Alcotest.(check bool) "patch >= 0" true (Proven.Version.patch () >= 0);
  Alcotest.(check bool) "abi >= 1" true (Proven.Version.abi_version () >= 1)

let () =
  Alcotest.run "proven" [
    "SafePath", [
      Alcotest.test_case "traversal" `Quick test_path_traversal;
    ];
    "SafeHeader", [
      Alcotest.test_case "crlf" `Quick test_header_crlf;
    ];
    "Version", [
      Alcotest.test_case "info" `Quick test_version_info;
    ];
  ]
