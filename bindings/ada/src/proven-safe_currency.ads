--  SPDX-License-Identifier: AGPL-3.0-or-later
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe currency operations with type-safe monetary values.

package Proven.Safe_Currency is

   --  ISO 4217 currency codes.
   type Currency_Code is (
      USD, EUR, GBP, JPY, CHF, CAD, AUD, NZD, CNY, INR,
      BRL, MXN, KRW, SGD, HKD, SEK, NOK, DKK, PLN, RUB,
      ZAR, TRY, THB, MYR, IDR, PHP, VND, AED, SAR, ILS,
      CZK, HUF, RON, BGN, HRK, ISK, CLP, COP, PEN, ARS,
      BTC, ETH
   );

   --  Parse result for currency code.
   type Currency_Code_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Code : Currency_Code;
         when False => null;
      end case;
   end record;

   --  Get number of decimal places for a currency.
   function Decimals (Code : Currency_Code) return Natural;

   --  Get currency symbol.
   function Symbol (Code : Currency_Code) return String;

   --  Get currency name.
   function Currency_Name (Code : Currency_Code) return String;

   --  Parse currency code from string.
   function Parse_Code (S : String) return Currency_Code_Result;

   --  Parse currency code, raising exception on failure.
   function Parse_Code_Or_Raise (S : String) return Currency_Code;

   --  Check if valid currency code string.
   function Is_Valid_Code (S : String) return Boolean;

   --  Type-safe monetary value.
   --  Amounts stored in minor units (cents, satoshis, etc.)
   type Money is record
      Minor_Units : Long_Long_Integer;
      Currency    : Currency_Code;
   end record;

   --  Result type for monetary operations.
   type Money_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Money;
         when False => null;
      end case;
   end record;

   --  Create from major units.
   function From_Major
     (Amount : Long_Long_Integer; Code : Currency_Code) return Money;

   --  Create from minor units.
   function From_Minor
     (Amount : Long_Long_Integer; Code : Currency_Code) return Money;

   --  Create zero amount.
   function Zero (Code : Currency_Code) return Money;

   --  Get the currency.
   function Get_Currency (M : Money) return Currency_Code;

   --  Get major units (truncated).
   function Major (M : Money) return Long_Long_Integer;

   --  Get minor units.
   function Minor (M : Money) return Long_Long_Integer;

   --  Add two monetary values (currencies must match).
   function Add (A, B : Money) return Money_Result;

   --  Subtract two monetary values (currencies must match).
   function Subtract (A, B : Money) return Money_Result;

   --  Multiply by scalar.
   function Multiply (M : Money; Scalar : Long_Long_Integer) return Money;

   --  Divide by scalar (returns failure if divisor is zero).
   function Divide
     (M : Money; Scalar : Long_Long_Integer) return Money_Result;

   --  Check if zero.
   function Is_Zero (M : Money) return Boolean;

   --  Check if positive.
   function Is_Positive (M : Money) return Boolean;

   --  Check if negative.
   function Is_Negative (M : Money) return Boolean;

   --  Absolute value.
   function Abs_Value (M : Money) return Money;

   --  Format as string with symbol.
   function Format (M : Money) return String;

   --  Exception for currency operations.
   Currency_Error : exception;

end Proven.Safe_Currency;
