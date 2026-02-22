--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Proven.Safe_String is

   --  Convert an Ada string to a C byte array, call the FFI function, and
   --  convert the result back. The caller-frees pattern matches libproven's API.

   type Byte_Array is array (Natural range <>) of aliased unsigned_char;

   function To_Bytes (S : String) return Byte_Array is
      Result : Byte_Array (0 .. S'Length - 1);
   begin
      for I in S'Range loop
         Result (I - S'First) := unsigned_char (Character'Pos (S (I)));
      end loop;
      return Result;
   end To_Bytes;

   function To_String_Result (R : FFI.String_Result) return Safe_String_Result
   is
   begin
      if R.Status /= FFI.PROVEN_OK then
         return (Success => False, Error_Code => Integer (R.Status));
      end if;

      if R.Value = Null_Ptr then
         return (Success => False, Error_Code => Integer (FFI.PROVEN_ERR_NULL_POINTER));
      end if;

      declare
         C_Str : constant String := Value (R.Value, R.Length);
         Res   : Safe_String_Result (Success => True);
      begin
         Res.Last := C_Str'Length;
         Res.Value (1 .. Res.Last) := C_Str;
         FFI.Proven_Free_String (R.Value);
         return Res;
      end;
   end To_String_Result;

   function Is_Valid_UTF8 (Value : String) return Safe_Bool_Result is
      Bytes  : aliased Byte_Array := To_Bytes (Value);
      Result : FFI.Bool_Result;
   begin
      if Value'Length = 0 then
         return (Success => True, Value => True);
      end if;
      Result := FFI.String_Is_Valid_UTF8
        (Bytes (Bytes'First)'Access, Bytes'Length);
      if Result.Status /= FFI.PROVEN_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      return (Success => True, Value => Boolean (Result.Value));
   end Is_Valid_UTF8;

   function Escape_Html (Value : String) return Safe_String_Result is
      Bytes : aliased Byte_Array := To_Bytes (Value);
   begin
      if Value'Length = 0 then
         return (Success => True, Value => (others => ' '), Last => 0);
      end if;
      return To_String_Result
        (FFI.String_Escape_HTML (Bytes (Bytes'First)'Access, Bytes'Length));
   end Escape_Html;

   function Escape_Sql (Value : String) return Safe_String_Result is
      Bytes : aliased Byte_Array := To_Bytes (Value);
   begin
      if Value'Length = 0 then
         return (Success => True, Value => (others => ' '), Last => 0);
      end if;
      return To_String_Result
        (FFI.String_Escape_SQL (Bytes (Bytes'First)'Access, Bytes'Length));
   end Escape_Sql;

   function Escape_Js (Value : String) return Safe_String_Result is
      Bytes : aliased Byte_Array := To_Bytes (Value);
   begin
      if Value'Length = 0 then
         return (Success => True, Value => (others => ' '), Last => 0);
      end if;
      return To_String_Result
        (FFI.String_Escape_JS (Bytes (Bytes'First)'Access, Bytes'Length));
   end Escape_Js;

end Proven.Safe_String;
