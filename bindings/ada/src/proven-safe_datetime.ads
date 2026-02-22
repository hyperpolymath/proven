--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe datetime operations -- thin FFI wrapper over libproven.
--  ISO 8601 parsing and validation done by the Idris2/Zig core.

package Proven.Safe_Datetime is

   type Datetime is record
      Year              : Integer;
      Month             : Integer range 1 .. 12;
      Day               : Integer range 1 .. 31;
      Hour              : Integer range 0 .. 23;
      Minute            : Integer range 0 .. 59;
      Second            : Integer range 0 .. 59;
      Nanosecond        : Natural;
      Tz_Offset_Minutes : Integer;
   end record;

   type Datetime_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : Datetime;
         when False => Error_Code : Integer;
      end case;
   end record;

   Max_Format_Len : constant := 64;

   type Format_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value : String (1 .. Max_Format_Len);
                       Last  : Natural;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Parse ISO 8601 datetime string (calls proven_datetime_parse).
   function Parse (S : String) return Datetime_Result;

   --  Format datetime as ISO 8601 string (calls proven_datetime_format_iso8601).
   function Format_ISO8601 (DT : Datetime) return Format_Result;

   --  Check if year is a leap year (calls proven_datetime_is_leap_year).
   function Is_Leap_Year (Year : Integer) return Boolean;

   --  Get number of days in a month (calls proven_datetime_days_in_month).
   function Days_In_Month (Year : Integer; Month : Integer) return Natural;

end Proven.Safe_Datetime;
