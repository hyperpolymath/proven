--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Proven.Safe_UUID is

   function To_Ada_UUID (C : FFI.C_Uuid) return UUID is
      Result : UUID;
   begin
      for I in 0 .. 15 loop
         Result.Bytes (I) := Natural (C.Bytes (I));
      end loop;
      return Result;
   end To_Ada_UUID;

   function To_C_UUID (U : UUID) return FFI.C_Uuid is
      Result : FFI.C_Uuid;
   begin
      for I in 0 .. 15 loop
         Result.Bytes (I) := unsigned_char (U.Bytes (I));
      end loop;
      return Result;
   end To_C_UUID;

   function V4_Generate return UUID_Result is
      C_UUID : aliased FFI.C_Uuid;
      Status : int;
   begin
      Status := FFI.UUID_V4_Generate (C_UUID'Access);
      if Status /= FFI.UUID_OK then
         return (Success => False, Error_Code => Integer (Status));
      end if;
      return (Success => True, Value => To_Ada_UUID (C_UUID));
   end V4_Generate;

   function Parse (S : String) return UUID_Result is
      C_Str  : chars_ptr := New_String (S);
      C_UUID : aliased FFI.C_Uuid;
      Status : int;
   begin
      Status := FFI.UUID_Parse (C_Str, S'Length, C_UUID'Access);
      Free (C_Str);
      if Status /= FFI.UUID_OK then
         return (Success => False, Error_Code => Integer (Status));
      end if;
      return (Success => True, Value => To_Ada_UUID (C_UUID));
   end Parse;

   function Format (U : UUID) return Format_Result is
      C_UUID : aliased constant FFI.C_Uuid := To_C_UUID (U);
      Buf    : char_array (0 .. UUID_String_Length);
      C_Buf  : chars_ptr;
      Status : int;
      Res    : Format_Result (Success => True);
   begin
      C_Buf := New_Char_Array (Buf);
      Status := FFI.UUID_To_String
        (C_UUID'Access, C_Buf, UUID_String_Length + 1);
      if Status /= FFI.UUID_OK then
         Free (C_Buf);
         return (Success => False, Error_Code => Integer (Status));
      end if;
      declare
         S : constant String := Value (C_Buf);
      begin
         Free (C_Buf);
         Res.Value := (others => ' ');
         Res.Last := S'Length;
         Res.Value (1 .. Res.Last) := S;
         return Res;
      end;
   end Format;

   function To_URN (U : UUID) return Format_Result is
      C_UUID : aliased constant FFI.C_Uuid := To_C_UUID (U);
      Buf    : char_array (0 .. FFI.UUID_URN_LEN);
      C_Buf  : chars_ptr;
      Status : int;
      Res    : Format_Result (Success => True);
   begin
      C_Buf := New_Char_Array (Buf);
      Status := FFI.UUID_To_URN
        (C_UUID'Access, C_Buf, FFI.UUID_URN_LEN + 1);
      if Status /= FFI.UUID_OK then
         Free (C_Buf);
         return (Success => False, Error_Code => Integer (Status));
      end if;
      declare
         S : constant String := Value (C_Buf);
      begin
         Free (C_Buf);
         Res.Value := (others => ' ');
         Res.Last := S'Length;
         Res.Value (1 .. Res.Last) := S;
         return Res;
      end;
   end To_URN;

   function Is_Nil (U : UUID) return Boolean is
      C_UUID : aliased constant FFI.C_Uuid := To_C_UUID (U);
   begin
      return Boolean (FFI.UUID_Is_Nil (C_UUID'Access));
   end Is_Nil;

   function Equal (A, B : UUID) return Boolean is
      C_A : aliased constant FFI.C_Uuid := To_C_UUID (A);
      C_B : aliased constant FFI.C_Uuid := To_C_UUID (B);
   begin
      return Boolean (FFI.UUID_Equals (C_A'Access, C_B'Access));
   end Equal;

   function Is_Valid (S : String) return Boolean is
      C_Str : chars_ptr := New_String (S);
      R     : C_bool;
   begin
      R := FFI.UUID_Is_Valid (C_Str, S'Length);
      Free (C_Str);
      return Boolean (R);
   end Is_Valid;

end Proven.Safe_UUID;
