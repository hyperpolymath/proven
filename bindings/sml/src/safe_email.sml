(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(* safe_email.sml -- Safe email validation.
 *
 * Thin wrapper around libproven's SafeEmail module.  Validates email
 * addresses against RFC 5321 (simplified) rules.  The validation logic
 * is executed entirely in the formally verified Idris 2 core.
 *)

signature SAFE_EMAIL =
sig
  (** Validate an email address (RFC 5321 simplified).
   *  Returns SOME true if valid, SOME false if invalid, NONE on error. *)
  val isValid : string -> bool option
end

structure SafeEmail :> SAFE_EMAIL =
struct

  fun isValid (email : string) : bool option =
    let
      val bytes = ProvenMarshal.stringToBytes email
      val len = ProvenMarshal.stringLen email
    in
      ProvenMarshal.unwrapBool (LibProven.emailIsValid (bytes, len))
    end

end
