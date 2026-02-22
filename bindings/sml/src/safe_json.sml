(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(* safe_json.sml -- Safe JSON validation and type detection.
 *
 * Thin wrapper around libproven's SafeJson module.  Validates JSON strings
 * and detects the root-level value type.  All parsing is performed in the
 * formally verified Idris 2 core -- no JSON parser is reimplemented here.
 *)

signature SAFE_JSON =
sig
  (** JSON value types. *)
  datatype json_type =
      JSON_NULL
    | JSON_BOOL
    | JSON_NUMBER
    | JSON_STRING
    | JSON_ARRAY
    | JSON_OBJECT
    | JSON_INVALID

  (** Check if a string is valid JSON.
   *  Returns SOME true if valid, SOME false if invalid, NONE on error. *)
  val isValid : string -> bool option

  (** Get the root-level JSON value type.
   *  Returns JSON_INVALID if the input is not valid JSON. *)
  val getType : string -> json_type
end

structure SafeJson :> SAFE_JSON =
struct

  datatype json_type =
      JSON_NULL
    | JSON_BOOL
    | JSON_NUMBER
    | JSON_STRING
    | JSON_ARRAY
    | JSON_OBJECT
    | JSON_INVALID

  fun isValid (s : string) : bool option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
    in
      ProvenMarshal.unwrapBool (LibProven.jsonIsValid (bytes, len))
    end

  fun getType (s : string) : json_type =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
      val raw = LibProven.jsonGetType (bytes, len)
    in
      case raw of
        0  => JSON_NULL
      | 1  => JSON_BOOL
      | 2  => JSON_NUMBER
      | 3  => JSON_STRING
      | 4  => JSON_ARRAY
      | 5  => JSON_OBJECT
      | _  => JSON_INVALID
    end

end
