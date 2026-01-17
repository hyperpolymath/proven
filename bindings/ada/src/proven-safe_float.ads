--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe floating-point operations with special value handling.

package Proven.Safe_Float is
   pragma Pure;

   --  Result type for float operations that may fail.
   type Float_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Long_Float;
         when False => null;
      end case;
   end record;

   --  Check if value is NaN.
   function Is_NaN (Value : Long_Float) return Boolean;

   --  Check if value is infinite.
   function Is_Infinite (Value : Long_Float) return Boolean;

   --  Check if value is finite (not NaN or infinite).
   function Is_Finite (Value : Long_Float) return Boolean;

   --  Check if value is positive infinity.
   function Is_Positive_Infinity (Value : Long_Float) return Boolean;

   --  Check if value is negative infinity.
   function Is_Negative_Infinity (Value : Long_Float) return Boolean;

   --  Check if value is negative zero.
   function Is_Negative_Zero (Value : Long_Float) return Boolean;

   --  Check if value is normal (not denormalized, zero, NaN, or infinite).
   function Is_Normal (Value : Long_Float) return Boolean;

   --  Safe division (returns Invalid on division by zero or NaN result).
   function Safe_Div (A, B : Long_Float) return Float_Result;

   --  Safe square root (returns Invalid for negative numbers).
   function Safe_Sqrt (Value : Long_Float) return Float_Result;

   --  Safe logarithm (returns Invalid for non-positive numbers).
   function Safe_Log (Value : Long_Float) return Float_Result;

   --  Safe natural logarithm.
   function Safe_Ln (Value : Long_Float) return Float_Result;

   --  Safe power (returns Invalid for invalid combinations).
   function Safe_Power (Base, Exponent : Long_Float) return Float_Result;

   --  Clamp value to range [Min, Max].
   function Clamp
     (Value, Min, Max : Long_Float) return Long_Float;

   --  Linear interpolation between A and B by factor T (0.0 to 1.0).
   function Lerp (A, B, T : Long_Float) return Long_Float;

   --  Approximately equal within epsilon.
   function Approx_Equal
     (A, B    : Long_Float;
      Epsilon : Long_Float := 1.0E-10) return Boolean;

   --  Approximately zero within epsilon.
   function Approx_Zero
     (Value   : Long_Float;
      Epsilon : Long_Float := 1.0E-10) return Boolean;

   --  Round to N decimal places.
   function Round_To (Value : Long_Float; Places : Natural) return Long_Float;

   --  Truncate to N decimal places.
   function Truncate_To (Value : Long_Float; Places : Natural) return Long_Float;

   --  Floor to N decimal places.
   function Floor_To (Value : Long_Float; Places : Natural) return Long_Float;

   --  Ceiling to N decimal places.
   function Ceil_To (Value : Long_Float; Places : Natural) return Long_Float;

   --  Sign function (-1.0, 0.0, or 1.0).
   function Sign (Value : Long_Float) return Long_Float;

   --  Copy sign from B to A.
   function Copy_Sign (A, B : Long_Float) return Long_Float;

   --  Fused multiply-add (A * B + C) with higher precision.
   function Fma (A, B, C : Long_Float) return Long_Float;

   --  Minimum of two values (NaN-aware).
   function Min (A, B : Long_Float) return Long_Float;

   --  Maximum of two values (NaN-aware).
   function Max (A, B : Long_Float) return Long_Float;

   --  Convert to integer with overflow check.
   type Int_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Long_Long_Integer;
         when False => null;
      end case;
   end record;

   function To_Integer (Value : Long_Float) return Int_Result;

   --  Parse string to float.
   function Parse (S : String) return Float_Result;

   --  Format float with specified precision.
   function Format (Value : Long_Float; Precision : Natural := 6) return String;

end Proven.Safe_Float;
