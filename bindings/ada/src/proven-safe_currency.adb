--  SPDX-License-Identifier: Apache-2.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

package body Proven.Safe_Currency is

   --  Forward declaration for internal helper
   function Power_Of_10 (N : Natural) return Long_Long_Integer;

   function Decimals (Code : Currency_Code) return Natural is
   begin
      case Code is
         when JPY | KRW | VND => return 0;
         when BTC | ETH       => return 8;
         when others          => return 2;
      end case;
   end Decimals;

   function Symbol (Code : Currency_Code) return String is
   begin
      case Code is
         when USD       => return "$";
         when EUR       => return "EUR";
         when GBP       => return "GBP";
         when JPY | CNY => return "JPY";
         when CHF       => return "Fr";
         when INR       => return "INR";
         when KRW       => return "KRW";
         when RUB       => return "RUB";
         when BTC       => return "BTC";
         when ETH       => return "ETH";
         when others    => return Currency_Code'Image (Code);
      end case;
   end Symbol;

   function Currency_Name (Code : Currency_Code) return String is
   begin
      case Code is
         when USD => return "US Dollar";
         when EUR => return "Euro";
         when GBP => return "British Pound";
         when JPY => return "Japanese Yen";
         when CHF => return "Swiss Franc";
         when CAD => return "Canadian Dollar";
         when AUD => return "Australian Dollar";
         when NZD => return "New Zealand Dollar";
         when CNY => return "Chinese Yuan";
         when INR => return "Indian Rupee";
         when BRL => return "Brazilian Real";
         when MXN => return "Mexican Peso";
         when KRW => return "South Korean Won";
         when SGD => return "Singapore Dollar";
         when HKD => return "Hong Kong Dollar";
         when BTC => return "Bitcoin";
         when ETH => return "Ethereum";
         when others => return "Currency";
      end case;
   end Currency_Name;

   function Parse_Code (S : String) return Currency_Code_Result is
      Upper_S : constant String := To_Upper (S);
   begin
      --  Use a structured approach that satisfies GNAT style rules
      if Upper_S'Length /= 3 then
         return (Valid => False);
      end if;

      --  Match known currency codes
      if Upper_S = "USD" then
         return (Valid => True, Code => USD);
      end if;

      if Upper_S = "EUR" then
         return (Valid => True, Code => EUR);
      end if;

      if Upper_S = "GBP" then
         return (Valid => True, Code => GBP);
      end if;

      if Upper_S = "JPY" then
         return (Valid => True, Code => JPY);
      end if;

      if Upper_S = "CHF" then
         return (Valid => True, Code => CHF);
      end if;

      if Upper_S = "CAD" then
         return (Valid => True, Code => CAD);
      end if;

      if Upper_S = "AUD" then
         return (Valid => True, Code => AUD);
      end if;

      if Upper_S = "NZD" then
         return (Valid => True, Code => NZD);
      end if;

      if Upper_S = "CNY" then
         return (Valid => True, Code => CNY);
      end if;

      if Upper_S = "INR" then
         return (Valid => True, Code => INR);
      end if;

      if Upper_S = "BRL" then
         return (Valid => True, Code => BRL);
      end if;

      if Upper_S = "MXN" then
         return (Valid => True, Code => MXN);
      end if;

      if Upper_S = "KRW" then
         return (Valid => True, Code => KRW);
      end if;

      if Upper_S = "SGD" then
         return (Valid => True, Code => SGD);
      end if;

      if Upper_S = "HKD" then
         return (Valid => True, Code => HKD);
      end if;

      if Upper_S = "SEK" then
         return (Valid => True, Code => SEK);
      end if;

      if Upper_S = "NOK" then
         return (Valid => True, Code => NOK);
      end if;

      if Upper_S = "DKK" then
         return (Valid => True, Code => DKK);
      end if;

      if Upper_S = "PLN" then
         return (Valid => True, Code => PLN);
      end if;

      if Upper_S = "RUB" then
         return (Valid => True, Code => RUB);
      end if;

      if Upper_S = "ZAR" then
         return (Valid => True, Code => ZAR);
      end if;

      if Upper_S = "TRY" then
         return (Valid => True, Code => TRY);
      end if;

      if Upper_S = "THB" then
         return (Valid => True, Code => THB);
      end if;

      if Upper_S = "MYR" then
         return (Valid => True, Code => MYR);
      end if;

      if Upper_S = "IDR" then
         return (Valid => True, Code => IDR);
      end if;

      if Upper_S = "PHP" then
         return (Valid => True, Code => PHP);
      end if;

      if Upper_S = "VND" then
         return (Valid => True, Code => VND);
      end if;

      if Upper_S = "AED" then
         return (Valid => True, Code => AED);
      end if;

      if Upper_S = "SAR" then
         return (Valid => True, Code => SAR);
      end if;

      if Upper_S = "ILS" then
         return (Valid => True, Code => ILS);
      end if;

      if Upper_S = "CZK" then
         return (Valid => True, Code => CZK);
      end if;

      if Upper_S = "HUF" then
         return (Valid => True, Code => HUF);
      end if;

      if Upper_S = "RON" then
         return (Valid => True, Code => RON);
      end if;

      if Upper_S = "BGN" then
         return (Valid => True, Code => BGN);
      end if;

      if Upper_S = "HRK" then
         return (Valid => True, Code => HRK);
      end if;

      if Upper_S = "ISK" then
         return (Valid => True, Code => ISK);
      end if;

      if Upper_S = "CLP" then
         return (Valid => True, Code => CLP);
      end if;

      if Upper_S = "COP" then
         return (Valid => True, Code => COP);
      end if;

      if Upper_S = "PEN" then
         return (Valid => True, Code => PEN);
      end if;

      if Upper_S = "ARS" then
         return (Valid => True, Code => ARS);
      end if;

      if Upper_S = "BTC" then
         return (Valid => True, Code => BTC);
      end if;

      if Upper_S = "ETH" then
         return (Valid => True, Code => ETH);
      end if;

      --  No match found
      return (Valid => False);
   end Parse_Code;

   function Parse_Code_Or_Raise (S : String) return Currency_Code is
      Result : constant Currency_Code_Result := Parse_Code (S);
   begin
      if not Result.Valid then
         raise Currency_Error with "Unknown currency code: " & S;
      end if;
      return Result.Code;
   end Parse_Code_Or_Raise;

   function Is_Valid_Code (S : String) return Boolean is
      Result : constant Currency_Code_Result := Parse_Code (S);
   begin
      return Result.Valid;
   end Is_Valid_Code;

   function Power_Of_10 (N : Natural) return Long_Long_Integer is
      Result : Long_Long_Integer := 1;
   begin
      for I in 1 .. N loop
         Result := Result * 10;
      end loop;
      return Result;
   end Power_Of_10;

   function From_Major
     (Amount : Long_Long_Integer; Code : Currency_Code) return Money
   is
      Multiplier : constant Long_Long_Integer := Power_Of_10 (Decimals (Code));
   begin
      return (Minor_Units => Amount * Multiplier, Currency => Code);
   end From_Major;

   function From_Minor
     (Amount : Long_Long_Integer; Code : Currency_Code) return Money
   is
   begin
      return (Minor_Units => Amount, Currency => Code);
   end From_Minor;

   function Zero (Code : Currency_Code) return Money is
   begin
      return (Minor_Units => 0, Currency => Code);
   end Zero;

   function Get_Currency (M : Money) return Currency_Code is
   begin
      return M.Currency;
   end Get_Currency;

   function Major (M : Money) return Long_Long_Integer is
      Divisor : constant Long_Long_Integer :=
         Power_Of_10 (Decimals (M.Currency));
   begin
      return M.Minor_Units / Divisor;
   end Major;

   function Minor (M : Money) return Long_Long_Integer is
   begin
      return M.Minor_Units;
   end Minor;

   function Add (A, B : Money) return Money_Result is
   begin
      if A.Currency /= B.Currency then
         return (Valid => False);
      end if;
      return (Valid => True,
              Value => (Minor_Units => A.Minor_Units + B.Minor_Units,
                        Currency    => A.Currency));
   end Add;

   function Subtract (A, B : Money) return Money_Result is
   begin
      if A.Currency /= B.Currency then
         return (Valid => False);
      end if;
      return (Valid => True,
              Value => (Minor_Units => A.Minor_Units - B.Minor_Units,
                        Currency    => A.Currency));
   end Subtract;

   function Multiply (M : Money; Scalar : Long_Long_Integer) return Money is
   begin
      return (Minor_Units => M.Minor_Units * Scalar, Currency => M.Currency);
   end Multiply;

   function Divide
     (M : Money; Scalar : Long_Long_Integer) return Money_Result
   is
   begin
      if Scalar = 0 then
         return (Valid => False);
      end if;
      return (Valid => True,
              Value => (Minor_Units => M.Minor_Units / Scalar,
                        Currency    => M.Currency));
   end Divide;

   function Is_Zero (M : Money) return Boolean is
   begin
      return M.Minor_Units = 0;
   end Is_Zero;

   function Is_Positive (M : Money) return Boolean is
   begin
      return M.Minor_Units > 0;
   end Is_Positive;

   function Is_Negative (M : Money) return Boolean is
   begin
      return M.Minor_Units < 0;
   end Is_Negative;

   function Abs_Value (M : Money) return Money is
   begin
      return (Minor_Units => abs M.Minor_Units, Currency => M.Currency);
   end Abs_Value;

   function Format (M : Money) return String is
      Dec        : constant Natural := Decimals (M.Currency);
      Divisor    : constant Long_Long_Integer := Power_Of_10 (Dec);
      Abs_Units  : constant Long_Long_Integer := abs M.Minor_Units;
      Major_Part : constant Long_Long_Integer := Abs_Units / Divisor;
      Minor_Part : constant Long_Long_Integer := Abs_Units mod Divisor;
      Sign_Str   : constant String :=
         (if M.Minor_Units < 0 then "-" else "");
      Major_Str  : constant String :=
         Trim (Long_Long_Integer'Image (Major_Part), Ada.Strings.Left);
      Minor_Str  : constant String :=
         Trim (Long_Long_Integer'Image (Minor_Part), Ada.Strings.Left);

      --  Pad a string on the left with Pad_Char to reach Width
      function Pad_Left
        (S : String; Width : Natural; Pad_Char : Character := '0')
        return String;

      function Pad_Left
        (S : String; Width : Natural; Pad_Char : Character := '0')
        return String
      is
         Trimmed : constant String := Trim (S, Ada.Strings.Both);
      begin
         if Trimmed'Length >= Width then
            return Trimmed;
         else
            return (1 .. Width - Trimmed'Length => Pad_Char) & Trimmed;
         end if;
      end Pad_Left;

   begin
      if Dec = 0 then
         return Sign_Str & Symbol (M.Currency) & Major_Str;
      else
         return Sign_Str & Symbol (M.Currency) & Major_Str &
                "." & Pad_Left (Minor_Str, Dec);
      end if;
   end Format;

end Proven.Safe_Currency;
