--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe mathematical operations with overflow detection.

package Proven.Safe_Math is
   pragma Pure;

   type Optional_Int64 (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Long_Long_Integer;
         when False => null;
      end case;
   end record;

   --  Safely divide two integers, returning invalid on division by zero.
   function Safe_Div (Numerator, Denominator : Long_Long_Integer)
      return Optional_Int64;

   --  Safely compute modulo, returning invalid on division by zero.
   function Safe_Mod (Numerator, Denominator : Long_Long_Integer)
      return Optional_Int64;

   --  Safely add two integers with overflow detection.
   function Safe_Add (A, B : Long_Long_Integer) return Optional_Int64;

   --  Safely subtract two integers with overflow detection.
   function Safe_Sub (A, B : Long_Long_Integer) return Optional_Int64;

   --  Safely multiply two integers with overflow detection.
   function Safe_Mul (A, B : Long_Long_Integer) return Optional_Int64;

end Proven.Safe_Math;
