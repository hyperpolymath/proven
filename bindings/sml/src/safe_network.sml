(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(* safe_network.sml -- Safe network address operations.
 *
 * Thin wrapper around libproven's SafeNetwork module.  Parses IPv4
 * addresses and classifies them as private (RFC 1918) or loopback.
 * All parsing and classification is performed in the verified Idris 2 core.
 *)

signature SAFE_NETWORK =
sig
  (** IPv4 address represented as four octets. *)
  type ipv4 = { o1 : Word8.word, o2 : Word8.word,
                o3 : Word8.word, o4 : Word8.word }

  (** Parse an IPv4 address string (e.g., "192.168.1.1").
   *  Returns SOME with the parsed address, NONE on parse failure. *)
  val parseIpv4 : string -> ipv4 option

  (** Check if an IPv4 address is private (RFC 1918).
   *  Private ranges: 10.x.x.x, 172.16-31.x.x, 192.168.x.x *)
  val ipv4IsPrivate : ipv4 -> bool

  (** Check if an IPv4 address is loopback (127.0.0.0/8). *)
  val ipv4IsLoopback : ipv4 -> bool

  (** Format an IPv4 address as a dotted-quad string. *)
  val ipv4ToString : ipv4 -> string
end

structure SafeNetwork :> SAFE_NETWORK =
struct

  type ipv4 = { o1 : Word8.word, o2 : Word8.word,
                o3 : Word8.word, o4 : Word8.word }

  (* ProvenIPv4Result layout (64-bit):
   *   offset 0: int32_t status (4 bytes)
   *   offset 4: uint8_t octets[4] (4 bytes)
   *)

  fun parseIpv4 (s : string) : ipv4 option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
      val resultPtr = LibProven.networkParseIpv4 (bytes, len)
    in
      if resultPtr = MLton.Pointer.null then NONE
      else
        let
          val status = MLton.Pointer.getInt32 (resultPtr, 0)
        in
          if ProvenStatus.isFail status then NONE
          else
            SOME {
              o1 = MLton.Pointer.getWord8 (resultPtr, 4),
              o2 = MLton.Pointer.getWord8 (resultPtr, 5),
              o3 = MLton.Pointer.getWord8 (resultPtr, 6),
              o4 = MLton.Pointer.getWord8 (resultPtr, 7)
            }
        end
    end

  fun ipv4IsPrivate ({ o1, o2, o3, o4 } : ipv4) : bool =
    LibProven.networkIpv4IsPrivate_raw (o1, o2, o3, o4)

  fun ipv4IsLoopback ({ o1, o2, o3, o4 } : ipv4) : bool =
    LibProven.networkIpv4IsLoopback_raw (o1, o2, o3, o4)

  fun ipv4ToString ({ o1, o2, o3, o4 } : ipv4) : string =
    String.concatWith "." [
      Int.toString (Word8.toInt o1),
      Int.toString (Word8.toInt o2),
      Int.toString (Word8.toInt o3),
      Int.toString (Word8.toInt o4)
    ]

end
