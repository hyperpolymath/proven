(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)
(* Keep the full eager-ABI availability gate separate from the wired smoke.
   This must fail while declared symbols are absent; it is not a mock. *)
let () = Printf.printf "Full API loaded: %d\n" (Proven.Version.major ())
