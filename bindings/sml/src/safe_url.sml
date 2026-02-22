(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(* safe_url.sml -- Safe URL parsing and validation.
 *
 * Thin wrapper around libproven's SafeUrl module.  Parses URLs into
 * component parts (scheme, host, port, path, query, fragment) using
 * the formally verified Idris 2 core.
 *
 * Also provides HTTP percent-encoding/decoding from SafeHttp.
 *)

signature SAFE_URL =
sig
  (** Parsed URL components. *)
  type url_components = {
    scheme   : string,
    host     : string,
    port     : int option,
    path     : string,
    query    : string,
    fragment : string
  }

  (** Parse a URL into its components.
   *  Returns SOME with parsed components, NONE on parse failure. *)
  val parse : string -> url_components option

  (** URL-encode a string (RFC 3986 percent encoding).
   *  Unreserved characters pass through; all others become %XX. *)
  val encode : string -> string option

  (** URL-decode a percent-encoded string. *)
  val decode : string -> string option
end

structure SafeUrl :> SAFE_URL =
struct

  type url_components = {
    scheme   : string,
    host     : string,
    port     : int option,
    path     : string,
    query    : string,
    fragment : string
  }

  (* URL parsing returns a ProvenUrlResult struct via pointer.  We read
   * the status field and then extract each component string using pointer
   * offset arithmetic matching the C struct layout.
   *
   * ProvenUrlResult layout (64-bit):
   *   offset  0: int32_t status        (4 bytes, padded to 8)
   *   offset  8: char* scheme          (8 bytes)
   *   offset 16: size_t scheme_len     (8 bytes)
   *   offset 24: char* host            (8 bytes)
   *   offset 32: size_t host_len       (8 bytes)
   *   offset 40: uint16_t port         (2 bytes)
   *   offset 42: bool has_port         (1 byte, padded to 48)
   *   offset 48: char* path            (8 bytes)
   *   offset 56: size_t path_len       (8 bytes)
   *   offset 64: char* query           (8 bytes)
   *   offset 72: size_t query_len      (8 bytes)
   *   offset 80: char* fragment        (8 bytes)
   *   offset 88: size_t fragment_len   (8 bytes)
   *
   * Note: exact offsets may vary by platform ABI.  These are for x86-64
   * Linux with standard alignment. *)

  fun readCString (basePtr : MLton.Pointer.t, ptrOffset : int, lenOffset : int) : string =
    let
      val strPtr = MLton.Pointer.getPointer (basePtr, ptrOffset)
      val strLen = Word64.toInt (MLton.Pointer.getWord64 (basePtr, lenOffset))
    in
      if strPtr = MLton.Pointer.null orelse strLen = 0 then ""
      else CharVector.tabulate (strLen, fn i =>
        Char.chr (Word8.toInt (MLton.Pointer.getWord8 (strPtr, i))))
    end

  fun parse (urlStr : string) : url_components option =
    let
      val bytes = ProvenMarshal.stringToBytes urlStr
      val len = ProvenMarshal.stringLen urlStr
      val resultPtr = LibProven.urlParse_raw (bytes, len)
    in
      if resultPtr = MLton.Pointer.null then NONE
      else
        let
          val status = MLton.Pointer.getInt32 (resultPtr, 0)
        in
          if ProvenStatus.isFail status then
            (LibProven.urlFree resultPtr; NONE)
          else
            let
              (* Read component strings from the struct at their offsets.
               * Pointer offsets are in units of the respective type size.
               * For getPointer, offset is in pointer-sized units (8 bytes). *)
              val scheme   = readCString (resultPtr, 1, 2)  (* offsets 8, 16 *)
              val host     = readCString (resultPtr, 3, 4)  (* offsets 24, 32 *)
              val portRaw  = Word16.toInt (MLton.Pointer.getWord16 (resultPtr, 20)) (* offset 40 *)
              val hasPort  = MLton.Pointer.getWord8 (resultPtr, 42) <> 0w0
              val path     = readCString (resultPtr, 6, 7)  (* offsets 48, 56 *)
              val query    = readCString (resultPtr, 8, 9)  (* offsets 64, 72 *)
              val fragment = readCString (resultPtr, 10, 11) (* offsets 80, 88 *)
              val result = {
                scheme   = scheme,
                host     = host,
                port     = if hasPort then SOME portRaw else NONE,
                path     = path,
                query    = query,
                fragment = fragment
              }
            in
              LibProven.urlFree resultPtr;
              SOME result
            end
        end
    end

  fun encode (s : string) : string option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
    in
      ProvenMarshal.unwrapString (LibProven.httpUrlEncode (bytes, len))
    end

  fun decode (s : string) : string option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
    in
      ProvenMarshal.unwrapString (LibProven.httpUrlDecode (bytes, len))
    end

end
