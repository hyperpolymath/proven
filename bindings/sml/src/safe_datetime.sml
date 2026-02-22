(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(* safe_datetime.sml -- Safe ISO 8601 date/time handling.
 *
 * Thin wrapper around libproven's SafeDateTime module.  Parses and formats
 * ISO 8601 date/time strings, checks leap years, and computes days per month.
 * All logic is performed in the formally verified Idris 2 core.
 *)

signature SAFE_DATETIME =
sig
  (** Date-time components. *)
  type datetime = {
    year            : Int32.int,
    month           : Word8.word,   (** 1-12 *)
    day             : Word8.word,   (** 1-31 *)
    hour            : Word8.word,   (** 0-23 *)
    minute          : Word8.word,   (** 0-59 *)
    second          : Word8.word,   (** 0-59 *)
    nanosecond      : Word32.word,
    tzOffsetMinutes : Int16.int     (** 0 for UTC *)
  }

  (** Parse an ISO 8601 date-time string.
   *  Supported formats: YYYY-MM-DD, YYYY-MM-DDTHH:MM:SS,
   *  YYYY-MM-DDTHH:MM:SSZ, YYYY-MM-DDTHH:MM:SS+HH:MM
   *  Returns SOME with parsed datetime, NONE on parse failure. *)
  val parse : string -> datetime option

  (** Format a datetime as an ISO 8601 string.
   *  Returns SOME with the formatted string, NONE on error. *)
  val formatIso8601 : datetime -> string option

  (** Check if a year is a leap year. *)
  val isLeapYear : Int32.int -> bool

  (** Get the number of days in a given month of a given year.
   *  Returns 0 if the month is invalid. *)
  val daysInMonth : Int32.int * Word8.word -> Word8.word
end

structure SafeDatetime :> SAFE_DATETIME =
struct

  type datetime = {
    year            : Int32.int,
    month           : Word8.word,
    day             : Word8.word,
    hour            : Word8.word,
    minute          : Word8.word,
    second          : Word8.word,
    nanosecond      : Word32.word,
    tzOffsetMinutes : Int16.int
  }

  (* ProvenDateTimeResult layout (64-bit):
   *   offset  0: int32_t status           (4 bytes)
   *   offset  4: int32_t year             (4 bytes)
   *   offset  8: uint8_t month            (1 byte)
   *   offset  9: uint8_t day              (1 byte)
   *   offset 10: uint8_t hour             (1 byte)
   *   offset 11: uint8_t minute           (1 byte)
   *   offset 12: uint8_t second           (1 byte)
   *   offset 13: (3 bytes padding)
   *   offset 16: uint32_t nanosecond      (4 bytes)
   *   offset 20: int16_t tz_offset_minutes (2 bytes)
   *)

  fun parse (s : string) : datetime option =
    let
      val bytes = ProvenMarshal.stringToBytes s
      val len = ProvenMarshal.stringLen s
      val resultPtr = LibProven.datetimeParse_raw (bytes, len)
    in
      if resultPtr = MLton.Pointer.null then NONE
      else
        let
          val status = MLton.Pointer.getInt32 (resultPtr, 0)
        in
          if ProvenStatus.isFail status then NONE
          else
            SOME {
              year            = MLton.Pointer.getInt32 (resultPtr, 4),
              month           = MLton.Pointer.getWord8 (resultPtr, 8),
              day             = MLton.Pointer.getWord8 (resultPtr, 9),
              hour            = MLton.Pointer.getWord8 (resultPtr, 10),
              minute          = MLton.Pointer.getWord8 (resultPtr, 11),
              second          = MLton.Pointer.getWord8 (resultPtr, 12),
              nanosecond      = MLton.Pointer.getWord32 (resultPtr, 16),
              tzOffsetMinutes = MLton.Pointer.getInt16 (resultPtr, 20)
            }
        end
    end

  fun formatIso8601 (dt : datetime) : string option =
    let
      (* We need to write the datetime components into a C struct and pass
       * it to proven_datetime_format_iso8601.  Allocate a buffer large
       * enough for ProvenDateTime (24 bytes) and fill it. *)
      val buf = MLton.Pointer.malloc (Word.fromInt 24)
      val () = MLton.Pointer.setInt32 (buf, 0, #year dt)
      val () = MLton.Pointer.setWord8 (buf, 4, #month dt) (* offset 4 after the first int32 *)
      val () = MLton.Pointer.setWord8 (buf, 5, #day dt)   (* but these are byte offsets *)
      val () = MLton.Pointer.setWord8 (buf, 6, #hour dt)
      val () = MLton.Pointer.setWord8 (buf, 7, #minute dt)
      val () = MLton.Pointer.setWord8 (buf, 8, #second dt)
      val () = MLton.Pointer.setWord32 (buf, 12, #nanosecond dt)
      val () = MLton.Pointer.setInt16 (buf, 16, #tzOffsetMinutes dt)
      val (status, strPtr, strLen) = LibProven.datetimeFormatIso8601_raw buf
    in
      MLton.Pointer.free buf;
      ProvenMarshal.unwrapString (status, strPtr, strLen)
    end

  fun isLeapYear (year : Int32.int) : bool =
    LibProven.datetimeIsLeapYear year

  fun daysInMonth (year : Int32.int, month : Word8.word) : Word8.word =
    LibProven.datetimeDaysInMonth (year, month)

end
