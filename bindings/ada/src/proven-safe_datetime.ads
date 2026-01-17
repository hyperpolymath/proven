--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe datetime operations with timezone handling.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Proven.Safe_Datetime is
   pragma Pure;

   --  Date components.
   type Date is record
      Year  : Integer range 1 .. 9999;
      Month : Integer range 1 .. 12;
      Day   : Integer range 1 .. 31;
   end record;

   --  Time components.
   type Time is record
      Hour       : Integer range 0 .. 23;
      Minute     : Integer range 0 .. 59;
      Second     : Integer range 0 .. 59;
      Nanosecond : Natural;
   end record;

   --  Combined datetime.
   type Datetime is record
      Date_Part : Date;
      Time_Part : Time;
   end record;

   --  Timezone offset in minutes from UTC.
   subtype Timezone_Offset is Integer range -720 .. 840;

   --  Datetime with timezone.
   type Datetime_With_Zone is record
      DT     : Datetime;
      Offset : Timezone_Offset;
   end record;

   --  Parse results.
   type Date_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Date;
         when False => null;
      end case;
   end record;

   type Time_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Time;
         when False => null;
      end case;
   end record;

   type Datetime_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Datetime;
         when False => null;
      end case;
   end record;

   type Datetime_Zone_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Datetime_With_Zone;
         when False => null;
      end case;
   end record;

   --  Check if year is leap year.
   function Is_Leap_Year (Year : Integer) return Boolean;

   --  Get days in month.
   function Days_In_Month (Year : Integer; Month : Integer) return Integer;

   --  Validate date components.
   function Is_Valid_Date (Year, Month, Day : Integer) return Boolean;

   --  Validate time components.
   function Is_Valid_Time
     (Hour, Minute, Second : Integer; Nanosecond : Natural := 0) return Boolean;

   --  Create date from components.
   function Make_Date (Year, Month, Day : Integer) return Date_Result;

   --  Create time from components.
   function Make_Time
     (Hour, Minute, Second : Integer;
      Nanosecond           : Natural := 0) return Time_Result;

   --  Create datetime from components.
   function Make_Datetime
     (Year, Month, Day     : Integer;
      Hour, Minute, Second : Integer;
      Nanosecond           : Natural := 0) return Datetime_Result;

   --  Parse ISO 8601 date (YYYY-MM-DD).
   function Parse_Date (S : String) return Date_Result;

   --  Parse ISO 8601 time (HH:MM:SS).
   function Parse_Time (S : String) return Time_Result;

   --  Parse ISO 8601 datetime.
   function Parse_Datetime (S : String) return Datetime_Result;

   --  Parse ISO 8601 datetime with timezone.
   function Parse_Datetime_Zone (S : String) return Datetime_Zone_Result;

   --  Format date as ISO 8601.
   function Format_Date (D : Date) return String;

   --  Format time as ISO 8601.
   function Format_Time (T : Time) return String;

   --  Format datetime as ISO 8601.
   function Format_Datetime (DT : Datetime) return String;

   --  Format datetime with timezone as ISO 8601.
   function Format_Datetime_Zone (DTZ : Datetime_With_Zone) return String;

   --  Add days to date.
   function Add_Days (D : Date; Days : Integer) return Date;

   --  Difference between dates in days.
   function Days_Between (D1, D2 : Date) return Integer;

   --  Day of week (1 = Monday, 7 = Sunday).
   function Day_Of_Week (D : Date) return Integer;

   --  Day of year (1-366).
   function Day_Of_Year (D : Date) return Integer;

   --  Week of year (ISO 8601).
   function Week_Of_Year (D : Date) return Integer;

   --  Compare dates.
   function Date_Before (D1, D2 : Date) return Boolean;
   function Date_After (D1, D2 : Date) return Boolean;
   function Date_Equal (D1, D2 : Date) return Boolean;

   --  Compare datetimes.
   function Datetime_Before (DT1, DT2 : Datetime) return Boolean;
   function Datetime_After (DT1, DT2 : Datetime) return Boolean;
   function Datetime_Equal (DT1, DT2 : Datetime) return Boolean;

end Proven.Safe_Datetime;
