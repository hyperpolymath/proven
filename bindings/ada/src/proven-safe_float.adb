--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

with Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;

package body Proven.Safe_Float is

   function Is_NaN (Value : Long_Float) return Boolean is
   begin
      --  NaN is not equal to itself
      return Value /= Value;
   end Is_NaN;

   function Is_Infinite (Value : Long_Float) return Boolean is
   begin
      return Value = Long_Float'Safe_Last * 2.0 or else
             Value = Long_Float'Safe_First * 2.0;
   exception
      when Constraint_Error =>
         return True;
   end Is_Infinite;

   function Is_Finite (Value : Long_Float) return Boolean is
   begin
      return not Is_NaN (Value) and then not Is_Infinite (Value);
   end Is_Finite;

   function Is_Positive_Infinity (Value : Long_Float) return Boolean is
   begin
      return Is_Infinite (Value) and then Value > 0.0;
   end Is_Positive_Infinity;

   function Is_Negative_Infinity (Value : Long_Float) return Boolean is
   begin
      return Is_Infinite (Value) and then Value < 0.0;
   end Is_Negative_Infinity;

   function Is_Negative_Zero (Value : Long_Float) return Boolean is
   begin
      return Value = 0.0 and then 1.0 / Value < 0.0;
   exception
      when Constraint_Error =>
         return False;
   end Is_Negative_Zero;

   function Is_Normal (Value : Long_Float) return Boolean is
   begin
      if Is_NaN (Value) or else Is_Infinite (Value) or else Value = 0.0 then
         return False;
      end if;
      return abs Value >= Long_Float'Model_Small;
   end Is_Normal;

   function Safe_Div (A, B : Long_Float) return Float_Result is
      Result : Long_Float;
   begin
      if B = 0.0 or else Is_NaN (A) or else Is_NaN (B) then
         return (Valid => False);
      end if;

      Result := A / B;

      if Is_NaN (Result) or else Is_Infinite (Result) then
         return (Valid => False);
      end if;

      return (Valid => True, Value => Result);
   exception
      when Constraint_Error =>
         return (Valid => False);
   end Safe_Div;

   function Safe_Sqrt (Value : Long_Float) return Float_Result is
   begin
      if Value < 0.0 or else Is_NaN (Value) then
         return (Valid => False);
      end if;

      return (Valid => True, Value => Sqrt (Value));
   exception
      when Constraint_Error =>
         return (Valid => False);
   end Safe_Sqrt;

   function Safe_Log (Value : Long_Float) return Float_Result is
   begin
      if Value <= 0.0 or else Is_NaN (Value) then
         return (Valid => False);
      end if;

      return (Valid => True, Value => Log (Value, 10.0));
   exception
      when Constraint_Error =>
         return (Valid => False);
   end Safe_Log;

   function Safe_Ln (Value : Long_Float) return Float_Result is
   begin
      if Value <= 0.0 or else Is_NaN (Value) then
         return (Valid => False);
      end if;

      return (Valid => True, Value => Log (Value));
   exception
      when Constraint_Error =>
         return (Valid => False);
   end Safe_Ln;

   function Safe_Power (Base, Exponent : Long_Float) return Float_Result is
      Result : Long_Float;
   begin
      if Is_NaN (Base) or else Is_NaN (Exponent) then
         return (Valid => False);
      end if;

      --  Handle special cases
      if Base < 0.0 and then Exponent /= Long_Float'Floor (Exponent) then
         return (Valid => False);  --  Negative base with non-integer exponent
      end if;

      if Base = 0.0 and then Exponent < 0.0 then
         return (Valid => False);  --  Division by zero
      end if;

      Result := Base ** Exponent;

      if Is_NaN (Result) or else Is_Infinite (Result) then
         return (Valid => False);
      end if;

      return (Valid => True, Value => Result);
   exception
      when Constraint_Error =>
         return (Valid => False);
   end Safe_Power;

   function Clamp (Value, Min, Max : Long_Float) return Long_Float is
   begin
      if Value < Min then
         return Min;
      elsif Value > Max then
         return Max;
      else
         return Value;
      end if;
   end Clamp;

   function Lerp (A, B, T : Long_Float) return Long_Float is
   begin
      return A + T * (B - A);
   end Lerp;

   function Approx_Equal
     (A, B    : Long_Float;
      Epsilon : Long_Float := 1.0E-10) return Boolean
   is
   begin
      return abs (A - B) <= Epsilon;
   end Approx_Equal;

   function Approx_Zero
     (Value   : Long_Float;
      Epsilon : Long_Float := 1.0E-10) return Boolean
   is
   begin
      return abs Value <= Epsilon;
   end Approx_Zero;

   function Power_Of_10 (N : Natural) return Long_Float is
      Result : Long_Float := 1.0;
   begin
      for I in 1 .. N loop
         Result := Result * 10.0;
      end loop;
      return Result;
   end Power_Of_10;

   function Round_To (Value : Long_Float; Places : Natural) return Long_Float is
      Factor : constant Long_Float := Power_Of_10 (Places);
   begin
      return Long_Float'Rounding (Value * Factor) / Factor;
   end Round_To;

   function Truncate_To (Value : Long_Float; Places : Natural) return Long_Float is
      Factor : constant Long_Float := Power_Of_10 (Places);
   begin
      return Long_Float'Truncation (Value * Factor) / Factor;
   end Truncate_To;

   function Floor_To (Value : Long_Float; Places : Natural) return Long_Float is
      Factor : constant Long_Float := Power_Of_10 (Places);
   begin
      return Long_Float'Floor (Value * Factor) / Factor;
   end Floor_To;

   function Ceil_To (Value : Long_Float; Places : Natural) return Long_Float is
      Factor : constant Long_Float := Power_Of_10 (Places);
   begin
      return Long_Float'Ceiling (Value * Factor) / Factor;
   end Ceil_To;

   function Sign (Value : Long_Float) return Long_Float is
   begin
      if Value > 0.0 then
         return 1.0;
      elsif Value < 0.0 then
         return -1.0;
      else
         return 0.0;
      end if;
   end Sign;

   function Copy_Sign (A, B : Long_Float) return Long_Float is
   begin
      if B >= 0.0 then
         return abs A;
      else
         return -abs A;
      end if;
   end Copy_Sign;

   function Fma (A, B, C : Long_Float) return Long_Float is
   begin
      --  Ada doesn't have native FMA; this is a simple implementation
      return A * B + C;
   end Fma;

   function Min (A, B : Long_Float) return Long_Float is
   begin
      if Is_NaN (A) then
         return B;
      elsif Is_NaN (B) then
         return A;
      elsif A < B then
         return A;
      else
         return B;
      end if;
   end Min;

   function Max (A, B : Long_Float) return Long_Float is
   begin
      if Is_NaN (A) then
         return B;
      elsif Is_NaN (B) then
         return A;
      elsif A > B then
         return A;
      else
         return B;
      end if;
   end Max;

   function To_Integer (Value : Long_Float) return Int_Result is
   begin
      if Is_NaN (Value) or else Is_Infinite (Value) then
         return (Valid => False);
      end if;

      if Value > Long_Float (Long_Long_Integer'Last) or else
         Value < Long_Float (Long_Long_Integer'First)
      then
         return (Valid => False);
      end if;

      return (Valid => True, Value => Long_Long_Integer (Value));
   exception
      when Constraint_Error =>
         return (Valid => False);
   end To_Integer;

   function Parse (S : String) return Float_Result is
   begin
      return (Valid => True, Value => Long_Float'Value (S));
   exception
      when Constraint_Error =>
         return (Valid => False);
   end Parse;

   function Format (Value : Long_Float; Precision : Natural := 6) return String is
   begin
      if Is_NaN (Value) then
         return "NaN";
      elsif Is_Positive_Infinity (Value) then
         return "Infinity";
      elsif Is_Negative_Infinity (Value) then
         return "-Infinity";
      else
         declare
            Formatted : String (1 .. 50);
            Last      : Natural;
         begin
            --  Use Ada's standard formatting
            declare
               Raw : constant String := Long_Float'Image (Value);
            begin
               --  Basic formatting - real implementation would use
               --  Ada.Text_IO.Float_IO for precise control
               return Raw;
            end;
         end;
      end if;
   end Format;

end Proven.Safe_Float;
