(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(* safe_crypto.sml -- Safe cryptographic primitives.
 *
 * Thin wrapper around libproven's SafeCrypto module.  Provides constant-time
 * byte comparison (timing-attack safe) and cryptographically secure random
 * byte generation.  Also wraps SafeHex for hex encoding/decoding and
 * SafeChecksum for CRC32.
 *
 * All cryptographic operations are performed in the verified Idris 2 core.
 *)

signature SAFE_CRYPTO =
sig
  (** Constant-time byte comparison.  Returns SOME true if equal, SOME false
   *  if not equal, NONE on internal error.  Timing-attack safe. *)
  val constantTimeEq : string * string -> bool option

  (** Fill a Word8 vector with cryptographically secure random bytes.
   *  Returns SOME with the random bytes, NONE on failure. *)
  val randomBytes : int -> Word8.word vector option

  (** Hex-encode a byte string.  uppercase=true for A-F, false for a-f.
   *  Returns SOME with the hex string, NONE on error. *)
  val hexEncode : string * bool -> string option

  (** Hex-decode a string to raw bytes.
   *  Returns SOME with the decoded bytes, NONE on invalid hex input. *)
  val hexDecode : string -> Word8.word vector option

  (** Calculate CRC32 checksum of a byte string.
   *  Returns SOME with the checksum value, NONE on error. *)
  val crc32 : string -> Int64.int option

  (** Verify CRC32 checksum matches expected value.
   *  Returns SOME true if match, SOME false if mismatch, NONE on error. *)
  val verifyCrc32 : string * Word32.word -> bool option
end

structure SafeCrypto :> SAFE_CRYPTO =
struct

  fun constantTimeEq (a : string, b : string) : bool option =
    let
      val bytesA = ProvenMarshal.stringToBytes a
      val lenA = ProvenMarshal.stringLen a
      val bytesB = ProvenMarshal.stringToBytes b
      val lenB = ProvenMarshal.stringLen b
    in
      ProvenMarshal.unwrapBool (LibProven.cryptoConstantTimeEq (bytesA, lenA, bytesB, lenB))
    end

  fun randomBytes (n : int) : Word8.word vector option =
    if n <= 0 then SOME (Vector.fromList [])
    else
      let
        val buf = MLton.Pointer.malloc (Word.fromInt n)
        val status = LibProven.cryptoRandomBytes (buf, Word64.fromInt n)
      in
        if ProvenStatus.isOk status then
          let
            val result = Vector.tabulate (n, fn i => MLton.Pointer.getWord8 (buf, i))
          in
            MLton.Pointer.free buf;
            SOME result
          end
        else
          (MLton.Pointer.free buf; NONE)
      end

  fun hexEncode (s : string, uppercase : bool) : string option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
    in
      ProvenMarshal.unwrapString (LibProven.hexEncode (bytes, len, uppercase))
    end

  fun hexDecode (s : string) : Word8.word vector option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
      val resultPtr = LibProven.hexDecode_raw (bytes, len)
    in
      if resultPtr = MLton.Pointer.null then NONE
      else
        let
          (* ProvenHexDecodeResult layout:
           *   offset 0: int32_t status (4 bytes, padded to 8)
           *   offset 8: uint8_t* data  (8 bytes)
           *   offset 16: size_t length (8 bytes)
           *)
          val status = MLton.Pointer.getInt32 (resultPtr, 0)
        in
          if ProvenStatus.isFail status then
            (LibProven.hexFree resultPtr; NONE)
          else
            let
              val dataPtr = MLton.Pointer.getPointer (resultPtr, 1)
              val dataLen = Word64.toInt (MLton.Pointer.getWord64 (resultPtr, 2))
              val result = Vector.tabulate (dataLen, fn i =>
                MLton.Pointer.getWord8 (dataPtr, i))
            in
              LibProven.hexFree resultPtr;
              SOME result
            end
        end
    end

  fun crc32 (s : string) : Int64.int option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
    in
      ProvenMarshal.unwrapInt (LibProven.checksumCrc32 (bytes, len))
    end

  fun verifyCrc32 (s : string, expected : Word32.word) : bool option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
    in
      ProvenMarshal.unwrapBool (LibProven.checksumVerifyCrc32 (bytes, len, expected))
    end

end
