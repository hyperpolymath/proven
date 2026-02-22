--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Proven.Safe_Phone is

   function To_Ada_Phone (C : FFI.C_Phone_Number) return Phone_Number is
      Result : Phone_Number;
      Len    : constant Natural := Natural (C.National_Number_Len);
   begin
      Result.Country := Country_Code (C.Country_Code);
      Result.National_Number := (others => ' ');
      Result.National_Last := Natural'Min (Len, Max_National_Len);
      for I in 0 .. Result.National_Last - 1 loop
         Result.National_Number (I + 1) :=
            Character (C.National_Number (I));
      end loop;
      return Result;
   end To_Ada_Phone;

   function To_C_Phone (Phone : Phone_Number) return FFI.C_Phone_Number is
      Result : FFI.C_Phone_Number;
   begin
      Result.Country_Code := int (Phone.Country);
      Result.National_Number := (others => nul);
      Result.National_Number_Len := unsigned_char (Phone.National_Last);
      for I in 1 .. Phone.National_Last loop
         Result.National_Number (I - 1) :=
            char (Phone.National_Number (I));
      end loop;
      return Result;
   end To_C_Phone;

   function Parse (Input : String) return Phone_Result is
      C_Str  : chars_ptr := New_String (Input);
      Result : FFI.Phone_Result;
   begin
      Result := FFI.Phone_Parse (C_Str, Input'Length);
      Free (C_Str);
      if Result.Status /= FFI.PHONE_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      return (Success => True, Number => To_Ada_Phone (Result.Number));
   end Parse;

   function Is_Valid (Input : String) return Boolean is
      C_Str : chars_ptr := New_String (Input);
      R     : C_bool;
   begin
      R := FFI.Phone_Is_Valid (C_Str, Input'Length);
      Free (C_Str);
      return Boolean (R);
   end Is_Valid;

   function To_E164 (Phone : Phone_Number) return Format_Result is
      C_Phone : aliased constant FFI.C_Phone_Number := To_C_Phone (Phone);
      Buf     : char_array (0 .. size_t (Max_Format_Len));
      C_Buf   : chars_ptr;
      Status  : int;
   begin
      C_Buf := New_Char_Array (Buf);
      Status := FFI.Phone_Format_E164
        (C_Phone'Access, C_Buf, size_t (Max_Format_Len) + 1);
      if Status /= FFI.PHONE_OK then
         Free (C_Buf);
         return (Success => False, Error_Code => Integer (Status));
      end if;
      declare
         S   : constant String := Value (C_Buf);
         Res : Format_Result (Success => True);
      begin
         Free (C_Buf);
         Res.Value := (others => ' ');
         Res.Last := Natural'Min (S'Length, Max_Format_Len);
         Res.Value (1 .. Res.Last) := S (S'First .. S'First + Res.Last - 1);
         return Res;
      end;
   end To_E164;

   function To_International (Phone : Phone_Number) return Format_Result is
      C_Phone : aliased constant FFI.C_Phone_Number := To_C_Phone (Phone);
      Buf     : char_array (0 .. size_t (Max_Format_Len));
      C_Buf   : chars_ptr;
      Status  : int;
   begin
      C_Buf := New_Char_Array (Buf);
      Status := FFI.Phone_Format_International
        (C_Phone'Access, C_Buf, size_t (Max_Format_Len) + 1);
      if Status /= FFI.PHONE_OK then
         Free (C_Buf);
         return (Success => False, Error_Code => Integer (Status));
      end if;
      declare
         S   : constant String := Value (C_Buf);
         Res : Format_Result (Success => True);
      begin
         Free (C_Buf);
         Res.Value := (others => ' ');
         Res.Last := Natural'Min (S'Length, Max_Format_Len);
         Res.Value (1 .. Res.Last) := S (S'First .. S'First + Res.Last - 1);
         return Res;
      end;
   end To_International;

   function Get_Calling_Code (Code : Country_Code) return Natural is
   begin
      return Natural (FFI.Phone_Get_Calling_Code (int (Code)));
   end Get_Calling_Code;

   function Digit_Count (Phone : Phone_Number) return Natural is
      C_Phone : aliased constant FFI.C_Phone_Number := To_C_Phone (Phone);
   begin
      return Natural (FFI.Phone_Digit_Count (C_Phone'Access));
   end Digit_Count;

end Proven.Safe_Phone;
