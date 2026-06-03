(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> *)

(** hello_smoke.ml - End-to-end "does the binding link and run" smoke test. *)

open Proven

let () =
  (* SafePath.has_traversal — happy + detection paths. *)
  let ok_safe = SafePath.has_traversal "/var/www/html/index.html" in
  let ok_evil = SafePath.has_traversal "../../etc/passwd" in

  if ok_safe <> false then begin
    print_endline "FAIL: SafePath.has_traversal('/var/www/html/index.html') did not return false";
    exit 1
  end;
  if ok_evil <> true then begin
    print_endline "FAIL: SafePath.has_traversal('../../etc/passwd') did not return true";
    exit 1
  end;

  (* Library identification. *)
  let major = Version.major () in
  let minor = Version.minor () in
  let patch = Version.patch () in

  Printf.printf "hello_smoke: OK (libproven %d.%d.%d)\n" major minor patch;
  exit 0
