(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(* proven.sml -- Main entry point for the Proven SML binding.
 *
 * Re-exports all sub-modules as a single top-level structure for
 * convenient access.  This is the recommended import for most users:
 *
 *   open Proven
 *   val result = Math.add (3, 5)
 *
 * ALL computation is performed in the formally verified Idris 2 core
 * via the C FFI bridge.  This SML package does data marshaling only.
 *)

structure Proven =
struct

  (* ---- Lifecycle --------------------------------------------------------- *)

  (** Initialize the Proven runtime (includes Idris 2 runtime).
   *  Must be called before any other Proven function.
   *  Returns true on success. *)
  fun init () : bool =
    ProvenStatus.isOk (LibProven.init ())

  (** Deinitialize the Proven runtime.  Call when done. *)
  fun deinit () : unit =
    LibProven.deinit ()

  (** Check if the runtime is initialized. *)
  fun isInitialized () : bool =
    LibProven.isInitialized ()

  (* ---- Version Info ------------------------------------------------------ *)

  structure Version =
  struct
    fun major () : Word32.word = LibProven.versionMajor ()
    fun minor () : Word32.word = LibProven.versionMinor ()
    fun patch () : Word32.word = LibProven.versionPatch ()
    fun abiVersion () : Word32.word = LibProven.ffiAbiVersion ()
    fun moduleCount () : Word32.word = LibProven.moduleCount ()

    fun toString () : string =
      String.concatWith "." [
        Word32.toString (major ()),
        Word32.toString (minor ()),
        Word32.toString (patch ())
      ]
  end

  (* ---- Sub-module Aliases ------------------------------------------------ *)

  (** Safe integer arithmetic with overflow/underflow detection. *)
  structure Math = SafeMath

  (** Safe string operations: UTF-8 validation, SQL/HTML/JS escaping. *)
  structure Str = SafeString

  (** Safe filesystem path operations: traversal detection, sanitization. *)
  structure Path = SafePath

  (** Email address validation (RFC 5321). *)
  structure Email = SafeEmail

  (** URL parsing, percent-encoding, and decoding. *)
  structure Url = SafeUrl

  (** Network address parsing and classification. *)
  structure Network = SafeNetwork

  (** Cryptographic primitives: constant-time comparison, random bytes,
   *  hex encoding/decoding, CRC32. *)
  structure Crypto = SafeCrypto

  (** JSON validation and type detection. *)
  structure Json = SafeJson

  (** ISO 8601 date/time parsing and formatting. *)
  structure Datetime = SafeDatetime

  (** Safe floating-point arithmetic. *)
  structure Float = SafeFloat

  (* ---- Status Codes (re-export) ------------------------------------------ *)

  structure Status = ProvenStatus

end
