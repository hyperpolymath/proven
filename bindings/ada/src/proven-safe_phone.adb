--  SPDX-License-Identifier: AGPL-3.0-or-later
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Proven.Safe_Phone is

   --  Forward declarations for internal helper functions
   function Extract_Digit_Chars (Input : String) return String;
   function Try_Parse_Country_Code (Digit_Str : String;
                                    Code      : out Country_Code;
                                    CC_Len    : out Natural) return Boolean;

   function Country_Code_Value (Code : Country_Code) return Natural is
   begin
      case Code is
         when US       => return 1;
         when RU       => return 7;
         when EG       => return 20;
         when ZA       => return 27;
         when FR       => return 33;
         when ES       => return 34;
         when IT       => return 39;
         when UK       => return 44;
         when DE       => return 49;
         when MX       => return 52;
         when BR       => return 55;
         when AU       => return 61;
         when JP       => return 81;
         when KR       => return 82;
         when CN       => return 86;
         when IN_India => return 91;
         when Unknown  => return 0;
      end case;
   end Country_Code_Value;

   function Country_Code_From_Value (Value : Natural) return Country_Code is
   begin
      case Value is
         when 1  => return US;
         when 7  => return RU;
         when 20 => return EG;
         when 27 => return ZA;
         when 33 => return FR;
         when 34 => return ES;
         when 39 => return IT;
         when 44 => return UK;
         when 49 => return DE;
         when 52 => return MX;
         when 55 => return BR;
         when 61 => return AU;
         when 81 => return JP;
         when 82 => return KR;
         when 86 => return CN;
         when 91 => return IN_India;
         when others => return Unknown;
      end case;
   end Country_Code_From_Value;

   function Extract_Digit_Chars (Input : String) return String is
      Result : String (1 .. Input'Length);
      Pos    : Natural := 0;
   begin
      for I in Input'Range loop
         if Input (I) >= '0' and Input (I) <= '9' then
            Pos := Pos + 1;
            Result (Pos) := Input (I);
         end if;
      end loop;
      return Result (1 .. Pos);
   end Extract_Digit_Chars;

   function Try_Parse_Country_Code (Digit_Str : String;
                                    Code      : out Country_Code;
                                    CC_Len    : out Natural) return Boolean is
      Value : Natural;
   begin
      --  Try 3-digit codes first, then 2, then 1
      for Len in reverse 1 .. 3 loop
         if Digit_Str'Length >= Len then
            begin
               Value := Natural'Value
                  (Digit_Str (Digit_Str'First .. Digit_Str'First + Len - 1));
               Code := Country_Code_From_Value (Value);
               if Code /= Unknown then
                  CC_Len := Len;
                  return True;
               end if;
            exception
               when Constraint_Error =>
                  null;  --  Continue trying other lengths
            end;
         end if;
      end loop;

      Code := Unknown;
      CC_Len := 0;
      return False;
   end Try_Parse_Country_Code;

   function Parse (Input : String) return Phone_Result is
      Trimmed   : constant String := Trim (Input, Ada.Strings.Both);
      Digit_Str : constant String := Extract_Digit_Chars (Trimmed);
      Code      : Country_Code;
      CC_Length : Natural;
   begin
      --  Check minimum length
      if Digit_Str'Length < 7 then
         return (Valid => False);
      end if;

      --  Check maximum length (E.164 max is 15)
      if Digit_Str'Length > 15 then
         return (Valid => False);
      end if;

      --  Try to parse country code
      if not Try_Parse_Country_Code (Digit_Str, Code, CC_Length) then
         return (Valid => False);
      end if;

      --  Check national number is not too short
      if Digit_Str'Length - CC_Length < 4 then
         return (Valid => False);
      end if;

      return (Valid  => True,
              Number => (Country => Code,
                         National_Number => To_Unbounded_String
                           (Digit_Str (Digit_Str'First + CC_Length ..
                                       Digit_Str'Last))));
   end Parse;

   function Parse_Or_Raise (Input : String) return Phone_Number is
      Result : constant Phone_Result := Parse (Input);
   begin
      if not Result.Valid then
         raise Phone_Parse_Error with "Invalid phone number: " & Input;
      end if;
      return Result.Number;
   end Parse_Or_Raise;

   function Is_Valid (Input : String) return Boolean is
      Result : constant Phone_Result := Parse (Input);
   begin
      return Result.Valid;
   end Is_Valid;

   function Get_Country_Code (Phone : Phone_Number) return Country_Code is
   begin
      return Phone.Country;
   end Get_Country_Code;

   function Get_National_Number (Phone : Phone_Number) return String is
   begin
      return To_String (Phone.National_Number);
   end Get_National_Number;

   function To_E164 (Phone : Phone_Number) return String is
      CC_Val : constant Natural := Country_Code_Value (Phone.Country);
   begin
      return "+" & Trim (Natural'Image (CC_Val), Ada.Strings.Left) &
             To_String (Phone.National_Number);
   end To_E164;

   function To_International (Phone : Phone_Number) return String is
      CC_Val : constant Natural := Country_Code_Value (Phone.Country);
      CC_Str : constant String :=
         Trim (Natural'Image (CC_Val), Ada.Strings.Left);
      Nat    : constant String := To_String (Phone.National_Number);
      Len    : constant Natural := Nat'Length;
   begin
      if Len <= 4 then
         return "+" & CC_Str & " " & Nat;
      elsif Len <= 7 then
         return "+" & CC_Str & " " &
                Nat (Nat'First .. Nat'First + 2) & " " &
                Nat (Nat'First + 3 .. Nat'Last);
      elsif Len <= 10 then
         return "+" & CC_Str & " " &
                Nat (Nat'First .. Nat'First + 2) & " " &
                Nat (Nat'First + 3 .. Nat'First + 5) & " " &
                Nat (Nat'First + 6 .. Nat'Last);
      else
         return "+" & CC_Str & " " & Nat;
      end if;
   end To_International;

   function Digit_Count (Phone : Phone_Number) return Natural is
      CC_Value   : constant Natural := Country_Code_Value (Phone.Country);
      CC_Digit_N : Natural;
   begin
      if CC_Value >= 100 then
         CC_Digit_N := 3;
      elsif CC_Value >= 10 then
         CC_Digit_N := 2;
      else
         CC_Digit_N := 1;
      end if;
      return CC_Digit_N + Length (Phone.National_Number);
   end Digit_Count;

end Proven.Safe_Phone;
