--  SPDX-License-Identifier: AGPL-3.0-or-later
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

package body Proven.Safe_UUID is

   Hex_Chars : constant String := "0123456789abcdef";

   --  Forward declarations for internal helpers
   function Hex_Char_To_Nibble (C : Character) return Integer;
   function Nibble_To_Hex_Char (N : Unsigned_8) return Character;

   function Hex_Char_To_Nibble (C : Character) return Integer is
   begin
      case C is
         when '0' .. '9' =>
            return Character'Pos (C) - Character'Pos ('0');
         when 'a' .. 'f' =>
            return Character'Pos (C) - Character'Pos ('a') + 10;
         when 'A' .. 'F' =>
            return Character'Pos (C) - Character'Pos ('A') + 10;
         when others =>
            return -1;
      end case;
   end Hex_Char_To_Nibble;

   function Nibble_To_Hex_Char (N : Unsigned_8) return Character is
   begin
      return Hex_Chars (Integer (N and 16#0F#) + 1);
   end Nibble_To_Hex_Char;

   function From_Bytes (Bytes : UUID_Bytes) return UUID is
   begin
      return (Bytes => Bytes);
   end From_Bytes;

   function As_Bytes (U : UUID) return UUID_Bytes is
   begin
      return U.Bytes;
   end As_Bytes;

   function Version (U : UUID) return UUID_Version is
      Version_Nibble : constant Unsigned_8 :=
         Shift_Right (U.Bytes (6), 4) and 16#0F#;
   begin
      case Version_Nibble is
         when 1 => return V1;
         when 2 => return V2;
         when 3 => return V3;
         when 4 => return V4;
         when 5 => return V5;
         when 0 =>
            --  Check if all bytes are zero for nil UUID
            for I in U.Bytes'Range loop
               if U.Bytes (I) /= 0 then
                  return Unknown_Version;
               end if;
            end loop;
            return Nil_Version;
         when others => return Unknown_Version;
      end case;
   end Version;

   function Variant (U : UUID) return UUID_Variant is
      Variant_Byte : constant Unsigned_8 := U.Bytes (8);
   begin
      if Shift_Right (Variant_Byte, 7) = 0 then
         return NCS;
      elsif Shift_Right (Variant_Byte, 6) = 2#10# then
         return RFC_4122;
      elsif Shift_Right (Variant_Byte, 5) = 2#110# then
         return Microsoft;
      else
         return Future;
      end if;
   end Variant;

   function Is_Nil (U : UUID) return Boolean is
   begin
      for I in U.Bytes'Range loop
         if U.Bytes (I) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Nil;

   function Format (U : UUID) return String is
      Result : String (1 .. 36);
      Pos    : Positive := 1;

      --  Forward declarations for nested procedures
      procedure Append_Byte (B : Unsigned_8);
      procedure Append_Dash;

      procedure Append_Byte (B : Unsigned_8) is
      begin
         Result (Pos) := Nibble_To_Hex_Char (Shift_Right (B, 4));
         Pos := Pos + 1;
         Result (Pos) := Nibble_To_Hex_Char (B and 16#0F#);
         Pos := Pos + 1;
      end Append_Byte;

      procedure Append_Dash is
      begin
         Result (Pos) := '-';
         Pos := Pos + 1;
      end Append_Dash;

   begin
      --  Format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
      --  Bytes:  0-3      4-5  6-7  8-9  10-15

      for I in 0 .. 3 loop
         Append_Byte (U.Bytes (I));
      end loop;
      Append_Dash;

      for I in 4 .. 5 loop
         Append_Byte (U.Bytes (I));
      end loop;
      Append_Dash;

      for I in 6 .. 7 loop
         Append_Byte (U.Bytes (I));
      end loop;
      Append_Dash;

      for I in 8 .. 9 loop
         Append_Byte (U.Bytes (I));
      end loop;
      Append_Dash;

      for I in 10 .. 15 loop
         Append_Byte (U.Bytes (I));
      end loop;

      return Result;
   end Format;

   function To_URN (U : UUID) return String is
   begin
      return "urn:uuid:" & Format (U);
   end To_URN;

   function Parse (S : String) return Parse_Result is
      Hex_Str                 : String (1 .. 32);
      Hex_Pos                 : Positive := 1;
      Bytes                   : UUID_Bytes;
      High_Nibble, Low_Nibble : Integer;
   begin
      --  Validate length
      if S'Length /= 36 then
         return (Valid => False);
      end if;

      --  Validate dash positions (relative to S'First)
      declare
         Offset : constant Integer := S'First - 1;
      begin
         if S (Offset + 9) /= '-'
           or else S (Offset + 14) /= '-'
           or else S (Offset + 19) /= '-'
           or else S (Offset + 24) /= '-'
         then
            return (Valid => False);
         end if;
      end;

      --  Extract hex characters, skipping dashes
      for I in S'Range loop
         if S (I) /= '-' then
            if Hex_Pos > 32 then
               return (Valid => False);
            end if;
            Hex_Str (Hex_Pos) := S (I);
            Hex_Pos := Hex_Pos + 1;
         end if;
      end loop;

      --  Check we got exactly 32 hex chars
      if Hex_Pos /= 33 then
         return (Valid => False);
      end if;

      --  Convert hex pairs to bytes
      for I in 0 .. 15 loop
         High_Nibble := Hex_Char_To_Nibble (Hex_Str (I * 2 + 1));
         Low_Nibble := Hex_Char_To_Nibble (Hex_Str (I * 2 + 2));

         if High_Nibble < 0 or Low_Nibble < 0 then
            return (Valid => False);
         end if;

         Bytes (I) := Unsigned_8 (High_Nibble * 16 + Low_Nibble);
      end loop;

      return (Valid => True, Value => (Bytes => Bytes));
   end Parse;

   function Parse_Or_Raise (S : String) return UUID is
      Result : constant Parse_Result := Parse (S);
   begin
      if not Result.Valid then
         raise UUID_Parse_Error with "Invalid UUID format: " & S;
      end if;
      return Result.Value;
   end Parse_Or_Raise;

   function V4_From_Bytes (Random_Bytes : UUID_Bytes) return UUID is
      Bytes : UUID_Bytes := Random_Bytes;
   begin
      --  Set version to 4
      Bytes (6) := (Bytes (6) and 16#0F#) or 16#40#;
      --  Set variant to RFC 4122
      Bytes (8) := (Bytes (8) and 16#3F#) or 16#80#;
      return (Bytes => Bytes);
   end V4_From_Bytes;

   function Is_Valid (S : String) return Boolean is
      Result : constant Parse_Result := Parse (S);
   begin
      return Result.Valid;
   end Is_Valid;

   function Equal (A, B : UUID) return Boolean is
   begin
      return A.Bytes = B.Bytes;
   end Equal;

end Proven.Safe_UUID;
