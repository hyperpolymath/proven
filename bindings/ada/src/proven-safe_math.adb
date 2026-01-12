-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

package body Proven.Safe_Math is

   function Safe_Div (Numerator, Denominator : Long_Long_Integer)
      return Optional_Int64 is
   begin
      if Denominator = 0 then
         return (Valid => False);
      end if;
      return (Valid => True, Value => Numerator / Denominator);
   end Safe_Div;

   function Safe_Mod (Numerator, Denominator : Long_Long_Integer)
      return Optional_Int64 is
   begin
      if Denominator = 0 then
         return (Valid => False);
      end if;
      return (Valid => True, Value => Numerator mod Denominator);
   end Safe_Mod;

   function Safe_Add (A, B : Long_Long_Integer) return Optional_Int64 is
   begin
      --  Check for overflow
      if B > 0 and then A > Long_Long_Integer'Last - B then
         return (Valid => False);
      end if;
      if B < 0 and then A < Long_Long_Integer'First - B then
         return (Valid => False);
      end if;
      return (Valid => True, Value => A + B);
   end Safe_Add;

   function Safe_Sub (A, B : Long_Long_Integer) return Optional_Int64 is
   begin
      --  Check for overflow
      if B < 0 and then A > Long_Long_Integer'Last + B then
         return (Valid => False);
      end if;
      if B > 0 and then A < Long_Long_Integer'First + B then
         return (Valid => False);
      end if;
      return (Valid => True, Value => A - B);
   end Safe_Sub;

   function Safe_Mul (A, B : Long_Long_Integer) return Optional_Int64 is
   begin
      if A = 0 or else B = 0 then
         return (Valid => True, Value => 0);
      end if;

      --  Check for overflow using division
      if A > 0 then
         if B > 0 then
            if A > Long_Long_Integer'Last / B then
               return (Valid => False);
            end if;
         else
            if B < Long_Long_Integer'First / A then
               return (Valid => False);
            end if;
         end if;
      else
         if B > 0 then
            if A < Long_Long_Integer'First / B then
               return (Valid => False);
            end if;
         else
            if A /= 0 and then B < Long_Long_Integer'Last / A then
               return (Valid => False);
            end if;
         end if;
      end if;

      return (Valid => True, Value => A * B);
   end Safe_Mul;

end Proven.Safe_Math;
