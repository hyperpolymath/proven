(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> *)

(** hello_smoke.ml - End-to-end "does the binding link and run" smoke test. *)

open Proven_wired

let () =
  (* SafePath.has_traversal — happy + detection paths. *)
  let ok_safe = SafePath.has_traversal "/var/www/html/index.html" in
  let ok_evil = SafePath.has_traversal "../../etc/passwd" in

  if ok_safe <> Some false then begin
    print_endline "FAIL: SafePath.has_traversal('/var/www/html/index.html') did not return false";
    exit 1
  end;
  if ok_evil <> Some true then begin
    print_endline "FAIL: SafePath.has_traversal('../../etc/passwd') did not return true";
    exit 1
  end;

  (* Exact bytes, including lone breaks, embedded NUL and non-UTF-8. *)
  List.iter (fun (value, expected) ->
    if SafeHeader.has_crlf value <> Some expected then begin
      print_endline "FAIL: SafeHeader byte-span counterexample";
      exit 1
    end
  ) ["", false; "application/json", false; "\r", true; "\n", true;
     "a\000\rZ", true; "\255\000\128", false];

  (* Check the binding's byte conversion across every octet and pair. *)
  for a = 0 to 255 do
    for b = 0 to 255 do
      let bytes = String.init 2 (function 0 -> Char.chr a | _ -> Char.chr b) in
      let expected = a = 10 || a = 13 || b = 10 || b = 13 in
      if SafeHeader.has_crlf bytes <> Some expected then begin
        Printf.eprintf "FAIL: header pair %d,%d\n" a b; exit 1
      end
    done
  done;

  (* Library identification. *)
  Printf.printf "hello_smoke: OK (libproven %s; %s)\n"
    (Version.version ()) (Version.build_info ());
  exit 0
