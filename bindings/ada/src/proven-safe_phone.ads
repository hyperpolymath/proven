--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe phone number operations -- thin FFI wrapper over libproven.
--  Phone parsing and formatting done by the Idris2/Zig core.

package Proven.Safe_Phone is

   Max_National_Len : constant := 15;
   Max_Format_Len   : constant := 32;

   --  Country calling code (numeric value from E.164).
   subtype Country_Code is Natural;

   type Phone_Number is record
      Country         : Country_Code;
      National_Number : String (1 .. Max_National_Len);
      National_Last   : Natural;
   end record;

   type Phone_Result (Success : Boolean := False) is record
      case Success is
         when True  => Number     : Phone_Number;
         when False => Error_Code : Integer;
      end case;
   end record;

   type Format_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : String (1 .. Max_Format_Len);
                       Last       : Natural;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Parse phone number from string (calls phone_parse).
   function Parse (Input : String) return Phone_Result;

   --  Check if string is valid phone number (calls phone_is_valid).
   function Is_Valid (Input : String) return Boolean;

   --  Format phone number in E.164 format (calls phone_format_e164).
   function To_E164 (Phone : Phone_Number) return Format_Result;

   --  Format phone number in international format
   --  (calls phone_format_international).
   function To_International (Phone : Phone_Number) return Format_Result;

   --  Get numeric calling code for a country code
   --  (calls phone_get_calling_code).
   function Get_Calling_Code (Code : Country_Code) return Natural;

   --  Get total digit count (calls phone_digit_count).
   function Digit_Count (Phone : Phone_Number) return Natural;

end Proven.Safe_Phone;
