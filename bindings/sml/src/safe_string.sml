(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(* safe_string.sml -- Safe string operations.
 *
 * Thin wrapper around libproven's SafeString module.  Provides UTF-8
 * validation and context-aware string escaping for SQL, HTML, and
 * JavaScript.  All computation is performed in the verified Idris 2 core.
 *)

signature SAFE_STRING =
sig
  (** Check whether a byte sequence is valid UTF-8. *)
  val isValidUtf8 : string -> bool option

  (** Escape a string for inclusion in SQL (single-quote escaping).
   *  Prefer parameterized queries over string escaping. *)
  val escapeSql : string -> string option

  (** Escape a string for safe inclusion in HTML (prevents XSS). *)
  val escapeHtml : string -> string option

  (** Escape a string for safe inclusion in JavaScript string literals. *)
  val escapeJs : string -> string option
end

structure SafeString :> SAFE_STRING =
struct

  fun isValidUtf8 (s : string) : bool option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
    in
      ProvenMarshal.unwrapBool (LibProven.stringIsValidUtf8 (bytes, len))
    end

  fun escapeSql (s : string) : string option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
    in
      ProvenMarshal.unwrapString (LibProven.stringEscapeSql (bytes, len))
    end

  fun escapeHtml (s : string) : string option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
    in
      ProvenMarshal.unwrapString (LibProven.stringEscapeHtml (bytes, len))
    end

  fun escapeJs (s : string) : string option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
    in
      ProvenMarshal.unwrapString (LibProven.stringEscapeJs (bytes, len))
    end

end
