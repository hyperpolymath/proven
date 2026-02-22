--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe currency operations -- thin FFI wrapper over libproven.
--  Currency arithmetic and formatting done by the Idris2/Zig core.

package Proven.Safe_Currency is

   --  ISO 4217 currency code as integer (maps to CurrencyCode enum in C).
   --  Use the constants below for specific currencies.
   subtype Currency_Code is Integer;

   --  Major currencies
   USD : constant Currency_Code := 0;
   EUR : constant Currency_Code := 1;
   GBP : constant Currency_Code := 2;
   JPY : constant Currency_Code := 3;
   CHF : constant Currency_Code := 4;
   CAD : constant Currency_Code := 5;
   AUD : constant Currency_Code := 6;
   NZD : constant Currency_Code := 7;
   CNY : constant Currency_Code := 8;
   INR : constant Currency_Code := 9;
   BRL : constant Currency_Code := 10;
   MXN : constant Currency_Code := 11;

   --  Cryptocurrencies
   BTC : constant Currency_Code := 40;
   ETH : constant Currency_Code := 41;

   type Money is record
      Minor_Units : Long_Long_Integer;
      Currency    : Currency_Code;
   end record;

   type Money_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : Money;
         when False => Error_Code : Integer;
      end case;
   end record;

   type Code_Result (Success : Boolean := False) is record
      case Success is
         when True  => Code       : Currency_Code;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Parse currency code from string (calls currency_parse_code).
   function Parse_Code (S : String) return Code_Result;

   --  Check if string is valid currency code (calls currency_is_valid_code).
   function Is_Valid_Code (S : String) return Boolean;

   --  Get decimal places for currency (calls currency_get_decimals).
   function Decimals (Code : Currency_Code) return Natural;

   --  Create money from major units (calls money_from_major).
   function From_Major
     (Amount : Long_Long_Integer; Code : Currency_Code) return Money;

   --  Create money from minor units (calls money_from_minor).
   function From_Minor
     (Amount : Long_Long_Integer; Code : Currency_Code) return Money;

   --  Create zero money (calls money_zero).
   function Zero (Code : Currency_Code) return Money;

   --  Add two money values (calls money_add).
   function Add (A, B : Money) return Money_Result;

   --  Subtract two money values (calls money_sub).
   function Subtract (A, B : Money) return Money_Result;

   --  Multiply by scalar (calls money_mul).
   function Multiply (M : Money; Scalar : Long_Long_Integer) return Money_Result;

   --  Divide by scalar (calls money_div).
   function Divide
     (M : Money; Divisor : Long_Long_Integer) return Money_Result;

   --  Check if zero (calls money_is_zero).
   function Is_Zero (M : Money) return Boolean;

   --  Check if positive (calls money_is_positive).
   function Is_Positive (M : Money) return Boolean;

   --  Check if negative (calls money_is_negative).
   function Is_Negative (M : Money) return Boolean;

   --  Get major units (calls money_get_major).
   function Major (M : Money) return Long_Long_Integer;

   --  Get minor units (calls money_get_minor).
   function Minor (M : Money) return Long_Long_Integer;

   Max_Format_Len : constant := 64;

   type Format_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : String (1 .. Max_Format_Len);
                       Last       : Natural;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Format money as string with symbol (calls money_format).
   function Format (M : Money) return Format_Result;

end Proven.Safe_Currency;
