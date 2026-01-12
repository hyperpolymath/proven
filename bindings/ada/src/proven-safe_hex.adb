--  SPDX-License-Identifier: AGPL-3.0-or-later
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Proven.Safe_Hex is

   Hex_Chars_Lower : constant String := "0123456789abcdef";
   Hex_Chars_Upper : constant String := "0123456789ABCDEF";

   function Is_Hex_Char (C : Character) return Boolean is
   begin
      return (C >= '0' and C <= '9') or
             (C >= 'a' and C <= 'f') or
             (C >= 'A' and C <= 'F');
   end Is_Hex_Char;

   function Hex_Char_To_Nibble (C : Character) return Unsigned_8 is
   begin
      case C is
         when '0' .. '9' =>
            return Unsigned_8 (Character'Pos (C) - Character'Pos ('0'));
         when 'a' .. 'f' =>
            return Unsigned_8 (Character'Pos (C) - Character'Pos ('a') + 10);
         when 'A' .. 'F' =>
            return Unsigned_8 (Character'Pos (C) - Character'Pos ('A') + 10);
         when others =>
            return 255;  --  Invalid marker
      end case;
   end Hex_Char_To_Nibble;

   function Nibble_To_Hex_Char (N : Unsigned_8) return Character is
   begin
      return Hex_Chars_Lower (Natural (N and 16#0F#) + 1);
   end Nibble_To_Hex_Char;

   function Nibble_To_Hex_Char_Upper (N : Unsigned_8) return Character is
   begin
      return Hex_Chars_Upper (Natural (N and 16#0F#) + 1);
   end Nibble_To_Hex_Char_Upper;

   function Encode (Bytes : Byte_Array) return String is
      Result : String (1 .. Bytes'Length * 2);
      Pos    : Positive := 1;
   begin
      for I in Bytes'Range loop
         Result (Pos) := Nibble_To_Hex_Char (Shift_Right (Bytes (I), 4));
         Result (Pos + 1) := Nibble_To_Hex_Char (Bytes (I) and 16#0F#);
         Pos := Pos + 2;
      end loop;
      return Result;
   end Encode;

   function Encode_Upper (Bytes : Byte_Array) return String is
      Result : String (1 .. Bytes'Length * 2);
      Pos    : Positive := 1;
   begin
      for I in Bytes'Range loop
         Result (Pos) := Nibble_To_Hex_Char_Upper (Shift_Right (Bytes (I), 4));
         Result (Pos + 1) := Nibble_To_Hex_Char_Upper (Bytes (I) and 16#0F#);
         Pos := Pos + 2;
      end loop;
      return Result;
   end Encode_Upper;

   function Decode (Hex : String) return Decode_Result is
      Byte_Count : constant Natural := Hex'Length / 2;
   begin
      --  Check even length
      if Hex'Length mod 2 /= 0 then
         return (Valid => False);
      end if;

      --  Check maximum size
      if Byte_Count > Max_Decode_Bytes then
         return (Valid => False);
      end if;

      --  Empty case
      if Hex'Length = 0 then
         return (Valid  => True,
                 Bytes  => (others => 0),
                 Length => 0);
      end if;

      declare
         Result_Bytes : Decode_Bytes := (others => 0);
         High, Low    : Unsigned_8;
         Hex_Pos      : Natural := Hex'First;
      begin
         for I in 0 .. Byte_Count - 1 loop
            High := Hex_Char_To_Nibble (Hex (Hex_Pos));
            Low := Hex_Char_To_Nibble (Hex (Hex_Pos + 1));

            --  Check for invalid characters
            if High = 255 or Low = 255 then
               return (Valid => False);
            end if;

            Result_Bytes (I) := Shift_Left (High, 4) or Low;
            Hex_Pos := Hex_Pos + 2;
         end loop;

         return (Valid  => True,
                 Bytes  => Result_Bytes,
                 Length => Byte_Count);
      end;
   end Decode;

   function Decode_Or_Raise (Hex : String) return Byte_Array is
      Result : constant Decode_Result := Decode (Hex);
   begin
      if not Result.Valid then
         raise Hex_Error with "Invalid hex string";
      end if;
      return Result.Bytes (0 .. Result.Length - 1);
   end Decode_Or_Raise;

   function Is_Valid (S : String) return Boolean is
   begin
      for I in S'Range loop
         if not Is_Hex_Char (S (I)) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Valid;

   function Is_Valid_Bytes (S : String) return Boolean is
   begin
      return S'Length mod 2 = 0 and then Is_Valid (S);
   end Is_Valid_Bytes;

   function Format_Spaced (Hex : String) return Format_Result is
   begin
      --  Check even length
      if Hex'Length mod 2 /= 0 then
         return (Valid => False);
      end if;

      --  Empty case
      if Hex'Length = 0 then
         return (Valid => True,
                 Value => (others => ' '),
                 Last  => 0);
      end if;

      declare
         Byte_Count  : constant Natural := Hex'Length / 2;
         Result_Len  : constant Natural := Hex'Length + Byte_Count - 1;
         Result      : Format_Result (Valid => True);
         Result_Pos  : Positive := 1;
         Hex_Pos     : Natural := Hex'First;
      begin
         if Result_Len > Max_Format_Len then
            return (Valid => False);
         end if;

         Result.Value := (others => ' ');

         for I in 1 .. Byte_Count loop
            if I > 1 then
               Result.Value (Result_Pos) := ' ';
               Result_Pos := Result_Pos + 1;
            end if;
            Result.Value (Result_Pos) := Hex (Hex_Pos);
            Result.Value (Result_Pos + 1) := Hex (Hex_Pos + 1);
            Result_Pos := Result_Pos + 2;
            Hex_Pos := Hex_Pos + 2;
         end loop;

         Result.Last := Result_Len;
         return Result;
      end;
   end Format_Spaced;

   function Format_Colons (Hex : String) return Format_Result is
   begin
      --  Check even length
      if Hex'Length mod 2 /= 0 then
         return (Valid => False);
      end if;

      --  Empty case
      if Hex'Length = 0 then
         return (Valid => True,
                 Value => (others => ' '),
                 Last  => 0);
      end if;

      declare
         Byte_Count  : constant Natural := Hex'Length / 2;
         Result_Len  : constant Natural := Hex'Length + Byte_Count - 1;
         Result      : Format_Result (Valid => True);
         Result_Pos  : Positive := 1;
         Hex_Pos     : Natural := Hex'First;
      begin
         if Result_Len > Max_Format_Len then
            return (Valid => False);
         end if;

         Result.Value := (others => ' ');

         for I in 1 .. Byte_Count loop
            if I > 1 then
               Result.Value (Result_Pos) := ':';
               Result_Pos := Result_Pos + 1;
            end if;
            Result.Value (Result_Pos) := Hex (Hex_Pos);
            Result.Value (Result_Pos + 1) := Hex (Hex_Pos + 1);
            Result_Pos := Result_Pos + 2;
            Hex_Pos := Hex_Pos + 2;
         end loop;

         Result.Last := Result_Len;
         return Result;
      end;
   end Format_Colons;

   function Constant_Time_Equal (A, B : String) return Boolean is
      Diff : Unsigned_8 := 0;
   begin
      --  Length mismatch - still perform constant-time ops
      if A'Length /= B'Length then
         return False;
      end if;

      --  Empty strings are equal
      if A'Length = 0 then
         return True;
      end if;

      --  Compare character by character in constant time
      for I in 0 .. A'Length - 1 loop
         declare
            Char_A : constant Character := To_Lower (A (A'First + I));
            Char_B : constant Character := To_Lower (B (B'First + I));
         begin
            Diff := Diff or
                    (Unsigned_8 (Character'Pos (Char_A)) xor
                     Unsigned_8 (Character'Pos (Char_B)));
         end;
      end loop;

      return Diff = 0;
   end Constant_Time_Equal;

   function Int_To_Hex
     (Value : Unsigned_64; Min_Width : Positive := 1) return String
   is
      Temp   : Unsigned_64 := Value;
      Buffer : String (1 .. 16);  --  Max 16 hex digits for 64-bit
      Pos    : Natural := Buffer'Last;
   begin
      --  Handle zero specially
      if Temp = 0 then
         if Min_Width = 1 then
            return "0";
         else
            return (1 .. Min_Width => '0');
         end if;
      end if;

      --  Build hex string from right to left
      while Temp > 0 loop
         Buffer (Pos) := Nibble_To_Hex_Char (Unsigned_8 (Temp and 16#0F#));
         Temp := Shift_Right (Temp, 4);
         Pos := Pos - 1;
      end loop;

      declare
         Result_Len : constant Natural := Buffer'Last - Pos;
         Result     : String (1 .. Natural'Max (Result_Len, Min_Width));
      begin
         if Result_Len >= Min_Width then
            return Buffer (Pos + 1 .. Buffer'Last);
         else
            --  Pad with zeros
            Result (1 .. Min_Width - Result_Len) := (others => '0');
            Result (Min_Width - Result_Len + 1 .. Min_Width) :=
               Buffer (Pos + 1 .. Buffer'Last);
            return Result;
         end if;
      end;
   end Int_To_Hex;

   function Hex_To_Int (Hex : String) return Int_Result is
      Result : Unsigned_64 := 0;
      Nibble : Unsigned_8;
   begin
      --  Empty string
      if Hex'Length = 0 then
         return (Valid => False);
      end if;

      --  Maximum 16 hex digits for 64-bit
      if Hex'Length > 16 then
         return (Valid => False);
      end if;

      for I in Hex'Range loop
         Nibble := Hex_Char_To_Nibble (Hex (I));
         if Nibble = 255 then
            return (Valid => False);
         end if;

         --  Check for overflow
         if Result > Shift_Right (Unsigned_64'Last, 4) then
            return (Valid => False);
         end if;

         Result := Shift_Left (Result, 4) or Unsigned_64 (Nibble);
      end loop;

      return (Valid => True, Value => Result);
   end Hex_To_Int;

end Proven.Safe_Hex;
