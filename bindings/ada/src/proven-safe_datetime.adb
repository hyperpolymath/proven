--  SPDX-License-Identifier: Apache-2.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

package body Proven.Safe_Datetime is

   function Is_Leap_Year (Year : Integer) return Boolean is
   begin
      return (Year mod 4 = 0 and then Year mod 100 /= 0)
             or else Year mod 400 = 0;
   end Is_Leap_Year;

   function Days_In_Month (Year : Integer; Month : Integer) return Integer is
   begin
      case Month is
         when 1 | 3 | 5 | 7 | 8 | 10 | 12 => return 31;
         when 4 | 6 | 9 | 11 => return 30;
         when 2 =>
            if Is_Leap_Year (Year) then
               return 29;
            else
               return 28;
            end if;
         when others => return 0;
      end case;
   end Days_In_Month;

   function Is_Valid_Date (Year, Month, Day : Integer) return Boolean is
   begin
      if Year < 1 or else Year > 9999 then
         return False;
      end if;
      if Month < 1 or else Month > 12 then
         return False;
      end if;
      if Day < 1 or else Day > Days_In_Month (Year, Month) then
         return False;
      end if;
      return True;
   end Is_Valid_Date;

   function Is_Valid_Time
     (Hour, Minute, Second : Integer;
      Nanosecond           : Natural := 0) return Boolean
   is
   begin
      return Hour >= 0 and then Hour <= 23 and then
             Minute >= 0 and then Minute <= 59 and then
             Second >= 0 and then Second <= 59 and then
             Nanosecond < 1_000_000_000;
   end Is_Valid_Time;

   function Make_Date (Year, Month, Day : Integer) return Date_Result is
   begin
      if Is_Valid_Date (Year, Month, Day) then
         return (Valid => True,
                 Value => (Year  => Year,
                          Month => Month,
                          Day   => Day));
      else
         return (Valid => False);
      end if;
   end Make_Date;

   function Make_Time
     (Hour, Minute, Second : Integer;
      Nanosecond           : Natural := 0) return Time_Result
   is
   begin
      if Is_Valid_Time (Hour, Minute, Second, Nanosecond) then
         return (Valid => True,
                 Value => (Hour       => Hour,
                          Minute     => Minute,
                          Second     => Second,
                          Nanosecond => Nanosecond));
      else
         return (Valid => False);
      end if;
   end Make_Time;

   function Make_Datetime
     (Year, Month, Day     : Integer;
      Hour, Minute, Second : Integer;
      Nanosecond           : Natural := 0) return Datetime_Result
   is
   begin
      if Is_Valid_Date (Year, Month, Day) and then
         Is_Valid_Time (Hour, Minute, Second, Nanosecond)
      then
         return (Valid => True,
                 Value => (Date_Part => (Year  => Year,
                                        Month => Month,
                                        Day   => Day),
                          Time_Part => (Hour       => Hour,
                                       Minute     => Minute,
                                       Second     => Second,
                                       Nanosecond => Nanosecond)));
      else
         return (Valid => False);
      end if;
   end Make_Datetime;

   function Parse_Int (S : String; First, Last : Integer) return Integer is
      Result : Integer := 0;
   begin
      for I in First .. Last loop
         if S (I) not in '0' .. '9' then
            return -1;
         end if;
         Result := Result * 10 + (Character'Pos (S (I)) - Character'Pos ('0'));
      end loop;
      return Result;
   end Parse_Int;

   function Parse_Date (S : String) return Date_Result is
      Year, Month, Day : Integer;
   begin
      --  Expected format: YYYY-MM-DD (10 characters)
      if S'Length /= 10 then
         return (Valid => False);
      end if;

      if S (S'First + 4) /= '-' or else S (S'First + 7) /= '-' then
         return (Valid => False);
      end if;

      Year := Parse_Int (S, S'First, S'First + 3);
      Month := Parse_Int (S, S'First + 5, S'First + 6);
      Day := Parse_Int (S, S'First + 8, S'First + 9);

      if Year < 0 or else Month < 0 or else Day < 0 then
         return (Valid => False);
      end if;

      return Make_Date (Year, Month, Day);
   end Parse_Date;

   function Parse_Time (S : String) return Time_Result is
      Hour, Minute, Second : Integer;
   begin
      --  Expected format: HH:MM:SS (8 characters minimum)
      if S'Length < 8 then
         return (Valid => False);
      end if;

      if S (S'First + 2) /= ':' or else S (S'First + 5) /= ':' then
         return (Valid => False);
      end if;

      Hour := Parse_Int (S, S'First, S'First + 1);
      Minute := Parse_Int (S, S'First + 3, S'First + 4);
      Second := Parse_Int (S, S'First + 6, S'First + 7);

      if Hour < 0 or else Minute < 0 or else Second < 0 then
         return (Valid => False);
      end if;

      return Make_Time (Hour, Minute, Second, 0);
   end Parse_Time;

   function Parse_Datetime (S : String) return Datetime_Result is
      Date_Res : Date_Result;
      Time_Res : Time_Result;
      Sep_Pos  : Natural := 0;
   begin
      --  Find T separator
      for I in S'Range loop
         if S (I) = 'T' or else S (I) = ' ' then
            Sep_Pos := I;
            exit;
         end if;
      end loop;

      if Sep_Pos = 0 or else Sep_Pos <= S'First or else Sep_Pos >= S'Last then
         return (Valid => False);
      end if;

      Date_Res := Parse_Date (S (S'First .. Sep_Pos - 1));
      if not Date_Res.Valid then
         return (Valid => False);
      end if;

      Time_Res := Parse_Time (S (Sep_Pos + 1 .. S'Last));
      if not Time_Res.Valid then
         return (Valid => False);
      end if;

      return (Valid => True,
              Value => (Date_Part => Date_Res.Value,
                       Time_Part => Time_Res.Value));
   end Parse_Datetime;

   function Parse_Datetime_Zone (S : String) return Datetime_Zone_Result is
      DT_Res     : Datetime_Result;
      Zone_Start : Natural := 0;
      Offset     : Timezone_Offset := 0;
   begin
      --  Find timezone indicator (Z, +, -)
      for I in reverse S'Range loop
         if S (I) = 'Z' then
            Zone_Start := I;
            Offset := 0;
            exit;
         elsif S (I) = '+' or else S (I) = '-' then
            Zone_Start := I;
            exit;
         end if;
      end loop;

      if Zone_Start = 0 then
         DT_Res := Parse_Datetime (S);
         if DT_Res.Valid then
            return (Valid => True,
                    Value => (DT => DT_Res.Value, Offset => 0));
         else
            return (Valid => False);
         end if;
      end if;

      DT_Res := Parse_Datetime (S (S'First .. Zone_Start - 1));
      if not DT_Res.Valid then
         return (Valid => False);
      end if;

      if S (Zone_Start) = 'Z' then
         Offset := 0;
      else
         --  Parse +HH:MM or -HH:MM
         declare
            Zone_Str : constant String := S (Zone_Start .. S'Last);
            Sign     : Integer := 1;
            Hours    : Integer;
            Minutes  : Integer := 0;
         begin
            if Zone_Str (Zone_Str'First) = '-' then
               Sign := -1;
            end if;

            if Zone_Str'Length >= 3 then
               Hours := Parse_Int (Zone_Str, Zone_Str'First + 1, Zone_Str'First + 2);
               if Hours < 0 then
                  return (Valid => False);
               end if;

               if Zone_Str'Length >= 6 and then Zone_Str (Zone_Str'First + 3) = ':' then
                  Minutes := Parse_Int (Zone_Str, Zone_Str'First + 4, Zone_Str'First + 5);
                  if Minutes < 0 then
                     return (Valid => False);
                  end if;
               end if;

               Offset := Sign * (Hours * 60 + Minutes);
            else
               return (Valid => False);
            end if;
         end;
      end if;

      return (Valid => True,
              Value => (DT => DT_Res.Value, Offset => Offset));
   end Parse_Datetime_Zone;

   function Two_Digit (N : Integer) return String is
      S : constant String := Integer'Image (N);
   begin
      if N < 10 then
         return "0" & S (S'Last);
      else
         return S (S'First + 1 .. S'Last);
      end if;
   end Two_Digit;

   function Four_Digit (N : Integer) return String is
      S : constant String := Integer'Image (N);
      Result : String (1 .. 4) := "0000";
      Len : constant Natural := S'Length - 1;  --  Skip leading space
   begin
      Result (5 - Len .. 4) := S (S'First + 1 .. S'Last);
      return Result;
   end Four_Digit;

   function Format_Date (D : Date) return String is
   begin
      return Four_Digit (D.Year) & "-" &
             Two_Digit (D.Month) & "-" &
             Two_Digit (D.Day);
   end Format_Date;

   function Format_Time (T : Time) return String is
   begin
      return Two_Digit (T.Hour) & ":" &
             Two_Digit (T.Minute) & ":" &
             Two_Digit (T.Second);
   end Format_Time;

   function Format_Datetime (DT : Datetime) return String is
   begin
      return Format_Date (DT.Date_Part) & "T" & Format_Time (DT.Time_Part);
   end Format_Datetime;

   function Format_Datetime_Zone (DTZ : Datetime_With_Zone) return String is
      Base : constant String := Format_Datetime (DTZ.DT);
   begin
      if DTZ.Offset = 0 then
         return Base & "Z";
      else
         declare
            Abs_Offset : constant Natural := abs DTZ.Offset;
            Hours      : constant Natural := Abs_Offset / 60;
            Minutes    : constant Natural := Abs_Offset mod 60;
            Sign       : constant String := (if DTZ.Offset > 0 then "+" else "-");
         begin
            return Base & Sign & Two_Digit (Hours) & ":" & Two_Digit (Minutes);
         end;
      end if;
   end Format_Datetime_Zone;

   function Days_From_Epoch (D : Date) return Integer is
      --  Days since year 1 (simplified calculation)
      Y : constant Integer := D.Year - 1;
      Days : Integer;
   begin
      Days := Y * 365 + Y / 4 - Y / 100 + Y / 400;
      for M in 1 .. D.Month - 1 loop
         Days := Days + Days_In_Month (D.Year, M);
      end loop;
      Days := Days + D.Day;
      return Days;
   end Days_From_Epoch;

   function Date_From_Days (Days : Integer) return Date is
      Remaining : Integer := Days;
      Year      : Integer := 1;
      Month     : Integer := 1;
      Day       : Integer;
      Year_Days : Integer;
   begin
      --  Find year
      loop
         Year_Days := 365;
         if Is_Leap_Year (Year) then
            Year_Days := 366;
         end if;
         if Remaining <= Year_Days then
            exit;
         end if;
         Remaining := Remaining - Year_Days;
         Year := Year + 1;
      end loop;

      --  Find month
      loop
         declare
            Month_Days : constant Integer := Days_In_Month (Year, Month);
         begin
            if Remaining <= Month_Days then
               exit;
            end if;
            Remaining := Remaining - Month_Days;
            Month := Month + 1;
         end;
      end loop;

      Day := Remaining;
      return (Year => Year, Month => Month, Day => Day);
   end Date_From_Days;

   function Add_Days (D : Date; Days : Integer) return Date is
      Current_Days : constant Integer := Days_From_Epoch (D);
   begin
      return Date_From_Days (Current_Days + Days);
   end Add_Days;

   function Days_Between (D1, D2 : Date) return Integer is
   begin
      return Days_From_Epoch (D2) - Days_From_Epoch (D1);
   end Days_Between;

   function Day_Of_Week (D : Date) return Integer is
      --  Zeller's congruence (adjusted for Monday = 1)
      Days : constant Integer := Days_From_Epoch (D);
      Result : Integer;
   begin
      Result := Days mod 7;
      if Result = 0 then
         Result := 7;
      end if;
      return Result;
   end Day_Of_Week;

   function Day_Of_Year (D : Date) return Integer is
      Days : Integer := 0;
   begin
      for M in 1 .. D.Month - 1 loop
         Days := Days + Days_In_Month (D.Year, M);
      end loop;
      return Days + D.Day;
   end Day_Of_Year;

   function Week_Of_Year (D : Date) return Integer is
      DOY : constant Integer := Day_Of_Year (D);
      DOW : constant Integer := Day_Of_Week (D);
   begin
      return (DOY - DOW + 10) / 7;
   end Week_Of_Year;

   function Date_Before (D1, D2 : Date) return Boolean is
   begin
      return Days_From_Epoch (D1) < Days_From_Epoch (D2);
   end Date_Before;

   function Date_After (D1, D2 : Date) return Boolean is
   begin
      return Days_From_Epoch (D1) > Days_From_Epoch (D2);
   end Date_After;

   function Date_Equal (D1, D2 : Date) return Boolean is
   begin
      return D1.Year = D2.Year and then D1.Month = D2.Month and then
             D1.Day = D2.Day;
   end Date_Equal;

   function Datetime_To_Seconds (DT : Datetime) return Long_Long_Integer is
      Days : constant Long_Long_Integer :=
         Long_Long_Integer (Days_From_Epoch (DT.Date_Part));
   begin
      return Days * 86400 +
             Long_Long_Integer (DT.Time_Part.Hour) * 3600 +
             Long_Long_Integer (DT.Time_Part.Minute) * 60 +
             Long_Long_Integer (DT.Time_Part.Second);
   end Datetime_To_Seconds;

   function Datetime_Before (DT1, DT2 : Datetime) return Boolean is
   begin
      return Datetime_To_Seconds (DT1) < Datetime_To_Seconds (DT2);
   end Datetime_Before;

   function Datetime_After (DT1, DT2 : Datetime) return Boolean is
   begin
      return Datetime_To_Seconds (DT1) > Datetime_To_Seconds (DT2);
   end Datetime_After;

   function Datetime_Equal (DT1, DT2 : Datetime) return Boolean is
   begin
      return Date_Equal (DT1.Date_Part, DT2.Date_Part) and then
             DT1.Time_Part.Hour = DT2.Time_Part.Hour and then
             DT1.Time_Part.Minute = DT2.Time_Part.Minute and then
             DT1.Time_Part.Second = DT2.Time_Part.Second and then
             DT1.Time_Part.Nanosecond = DT2.Time_Part.Nanosecond;
   end Datetime_Equal;

end Proven.Safe_Datetime;
