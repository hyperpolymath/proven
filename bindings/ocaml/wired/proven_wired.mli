(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)
(** The available five-symbol ABI profile, independent of the full Proven API.
    Loading requires those actual exports. Nonzero call statuses return None. *)
module SafeHeader : sig
  (** Detect either CR or LF over the exact string bytes via Idris RefC. *)
  val has_crlf : string -> bool option
end
module SafePath : sig
  (** Legacy Zig export; outside the Idris header proof qualification. *)
  val has_traversal : string -> bool option
end
module Version : sig
  val version : unit -> string
  val build_info : unit -> string
end
