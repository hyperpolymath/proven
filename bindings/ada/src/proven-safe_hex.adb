--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Proven.Safe_Hex is

   function Encode (Bytes : Byte_Array) return Encode_Result is
      C_Bytes : aliased array (0 .. Bytes'Length - 1) of aliased unsigned_char;
      Hex_Buf : char_array (0 .. size_t (Bytes'Length) * 2);
      C_Hex   : chars_ptr;
      Result  : FFI.Hex_Encode_Result;
      Res     : Encode_Result (Success => True);
   begin
      if Bytes'Length = 0 then
         Res.Value := (others => ' ');
         Res.Last := 0;
         return Res;
      end if;
      for I in Bytes'Range loop
         C_Bytes (I - Bytes'First) := unsigned_char (Bytes (I));
      end loop;
      C_Hex := New_Char_Array (Hex_Buf);
      Result := FFI.Hex_Encode
        (C_Bytes (C_Bytes'First)'Access, C_Bytes'Length,
         C_Hex, size_t (Bytes'Length) * 2 + 1);
      if Result.Status /= FFI.HEX_OK then
         Free (C_Hex);
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      declare
         S : constant String := Value (C_Hex);
      begin
         Free (C_Hex);
         Res.Value := (others => ' ');
         Res.Last := Natural'Min (S'Length, Max_Encode_Chars);
         Res.Value (1 .. Res.Last) := S (S'First .. S'First + Res.Last - 1);
         return Res;
      end;
   end Encode;

   function Decode (Hex : String) return Decode_Result is
      C_Hex    : chars_ptr := New_String (Hex);
      C_Bytes  : aliased array (0 .. Max_Decode_Bytes - 1) of aliased unsigned_char;
      Result   : FFI.Hex_Decode_Result;
   begin
      if Hex'Length = 0 then
         Free (C_Hex);
         return (Success => True,
                 Bytes   => (others => 0),
                 Length  => 0);
      end if;
      Result := FFI.Hex_Decode
        (C_Hex, Hex'Length, C_Bytes (C_Bytes'First)'Access, Max_Decode_Bytes);
      Free (C_Hex);
      if Result.Status /= FFI.HEX_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      declare
         Res : Decode_Result (Success => True);
      begin
         Res.Bytes := (others => 0);
         Res.Length := Natural (Result.Bytes_Written);
         for I in 0 .. Res.Length - 1 loop
            Res.Bytes (I) := Unsigned_8 (C_Bytes (I));
         end loop;
         return Res;
      end;
   end Decode;

   function Is_Valid (S : String) return Boolean is
      C_Str : chars_ptr := New_String (S);
      R     : C_bool;
   begin
      R := FFI.Hex_Is_Valid (C_Str, S'Length);
      Free (C_Str);
      return Boolean (R);
   end Is_Valid;

   function Is_Valid_Bytes (S : String) return Boolean is
      C_Str : chars_ptr := New_String (S);
      R     : C_bool;
   begin
      R := FFI.Hex_Is_Valid_Bytes (C_Str, S'Length);
      Free (C_Str);
      return Boolean (R);
   end Is_Valid_Bytes;

   function Constant_Time_Equal (A, B : String) return Boolean is
      C_A : chars_ptr := New_String (A);
      C_B : chars_ptr := New_String (B);
      R   : C_bool;
   begin
      R := FFI.Hex_Constant_Time_Eq (C_A, A'Length, C_B, B'Length);
      Free (C_A);
      Free (C_B);
      return Boolean (R);
   end Constant_Time_Equal;

   function Hex_To_Int (Hex : String) return Int_Result is
      C_Hex  : chars_ptr := New_String (Hex);
      Result : FFI.Hex_Int_Result;
   begin
      Result := FFI.Hex_To_Int (C_Hex, Hex'Length);
      Free (C_Hex);
      if Result.Status /= FFI.HEX_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      return (Success => True, Value => Unsigned_64 (Result.Value));
   end Hex_To_Int;

end Proven.Safe_Hex;
