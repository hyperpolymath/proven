--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Proven.Safe_Datetime is

   type Byte_Array is array (Natural range <>) of aliased unsigned_char;

   function To_Bytes (S : String) return Byte_Array is
      Result : Byte_Array (0 .. S'Length - 1);
   begin
      for I in S'Range loop
         Result (I - S'First) := unsigned_char (Character'Pos (S (I)));
      end loop;
      return Result;
   end To_Bytes;

   function Parse (S : String) return Datetime_Result is
      Bytes  : aliased Byte_Array := To_Bytes (S);
      Result : FFI.DateTime_Result;
   begin
      if S'Length = 0 then
         return (Success => False,
                 Error_Code => Integer (FFI.PROVEN_ERR_INVALID_ARGUMENT));
      end if;
      Result := FFI.Datetime_Parse
        (Bytes (Bytes'First)'Access, Bytes'Length);
      if Result.Status /= FFI.PROVEN_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      return (Success => True,
              Value => (Year              => Integer (Result.Datetime.Year),
                        Month             => Integer (Result.Datetime.Month),
                        Day               => Integer (Result.Datetime.Day),
                        Hour              => Integer (Result.Datetime.Hour),
                        Minute            => Integer (Result.Datetime.Minute),
                        Second            => Integer (Result.Datetime.Second),
                        Nanosecond        => Natural (Result.Datetime.Nanosecond),
                        Tz_Offset_Minutes => Integer (Result.Datetime.Tz_Offset_Minutes)));
   end Parse;

   function Format_ISO8601 (DT : Datetime) return Format_Result is
      C_DT   : FFI.C_DateTime;
      Result : FFI.String_Result;
   begin
      C_DT.Year              := int (DT.Year);
      C_DT.Month             := unsigned_char (DT.Month);
      C_DT.Day               := unsigned_char (DT.Day);
      C_DT.Hour              := unsigned_char (DT.Hour);
      C_DT.Minute            := unsigned_char (DT.Minute);
      C_DT.Second            := unsigned_char (DT.Second);
      C_DT.Nanosecond        := unsigned (DT.Nanosecond);
      C_DT.Tz_Offset_Minutes := short (DT.Tz_Offset_Minutes);

      Result := FFI.Datetime_Format_ISO8601 (C_DT);
      if Result.Status /= FFI.PROVEN_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      if Result.Value = Null_Ptr then
         return (Success => False,
                 Error_Code => Integer (FFI.PROVEN_ERR_NULL_POINTER));
      end if;
      declare
         C_Str : constant String := Value (Result.Value, Result.Length);
         Res   : Format_Result (Success => True);
      begin
         Res.Last := C_Str'Length;
         Res.Value (1 .. Res.Last) := C_Str;
         FFI.Proven_Free_String (Result.Value);
         return Res;
      end;
   end Format_ISO8601;

   function Is_Leap_Year (Year : Integer) return Boolean is
   begin
      return Boolean (FFI.Datetime_Is_Leap_Year (int (Year)));
   end Is_Leap_Year;

   function Days_In_Month (Year : Integer; Month : Integer) return Natural is
   begin
      return Natural (FFI.Datetime_Days_In_Month
        (int (Year), unsigned_char (Month)));
   end Days_In_Month;

end Proven.Safe_Datetime;
