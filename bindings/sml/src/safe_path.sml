(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(* safe_path.sml -- Safe filesystem path operations.
 *
 * Thin wrapper around libproven's SafePath module.  Detects directory
 * traversal attacks ("..") and sanitizes filenames by removing dangerous
 * characters.  All logic executes in the verified Idris 2 core.
 *)

signature SAFE_PATH =
sig
  (** Check if a path contains directory traversal sequences ("..").
   *  Returns SOME true if traversal was detected, SOME false if safe,
   *  NONE on internal error. *)
  val hasTraversal : string -> bool option

  (** Sanitize a filename by removing dangerous characters.
   *  Returns SOME with the sanitized filename, NONE on error. *)
  val sanitizeFilename : string -> string option
end

structure SafePath :> SAFE_PATH =
struct

  fun hasTraversal (s : string) : bool option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
    in
      ProvenMarshal.unwrapBool (LibProven.pathHasTraversal (bytes, len))
    end

  fun sanitizeFilename (s : string) : string option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
    in
      ProvenMarshal.unwrapString (LibProven.pathSanitizeFilename (bytes, len))
    end

end
