--  SPDX-License-Identifier: AGPL-3.0-or-later
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe phone number validation following E.164.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Proven.Safe_Phone is

   --  Country calling codes.
   type Country_Code is (
      US,       --  1  - USA, Canada
      RU,       --  7  - Russia
      EG,       --  20 - Egypt
      ZA,       --  27 - South Africa
      FR,       --  33 - France
      ES,       --  34 - Spain
      IT,       --  39 - Italy
      UK,       --  44 - UK
      DE,       --  49 - Germany
      MX,       --  52 - Mexico
      BR,       --  55 - Brazil
      AU,       --  61 - Australia
      JP,       --  81 - Japan
      KR,       --  82 - South Korea
      CN,       --  86 - China
      IN_India, --  91 - India (IN is reserved in Ada)
      Unknown
   );

   --  Get numeric value for country code.
   function Country_Code_Value (Code : Country_Code) return Natural;

   --  Parse country code from numeric value.
   function Country_Code_From_Value (Value : Natural) return Country_Code;

   --  Validated phone number.
   type Phone_Number is record
      Country         : Country_Code;
      National_Number : Unbounded_String;
   end record;

   --  Parse result for phone numbers.
   type Phone_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Number : Phone_Number;
         when False => null;
      end case;
   end record;

   --  Parse phone number from string.
   function Parse (Input : String) return Phone_Result;

   --  Parse phone number, raising exception on failure.
   function Parse_Or_Raise (Input : String) return Phone_Number;

   --  Check if valid phone number.
   function Is_Valid (Input : String) return Boolean;

   --  Get country code from phone number.
   function Get_Country_Code (Phone : Phone_Number) return Country_Code;

   --  Get national number portion.
   function Get_National_Number (Phone : Phone_Number) return String;

   --  Format in E.164 format (+CountryNational).
   function To_E164 (Phone : Phone_Number) return String;

   --  Format in international format with spaces.
   function To_International (Phone : Phone_Number) return String;

   --  Get total digit count.
   function Digit_Count (Phone : Phone_Number) return Natural;

   --  Exception for phone parsing errors.
   Phone_Parse_Error : exception;

end Proven.Safe_Phone;
