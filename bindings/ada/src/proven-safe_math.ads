--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe mathematical operations -- thin FFI wrapper over libproven.
--  All arithmetic is performed by the formally verified Idris2/Zig core.

package Proven.Safe_Math is

   type Safe_Int_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : Long_Long_Integer;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Safe integer division (calls proven_math_div).
   function Div_Safe (Numerator, Denominator : Long_Long_Integer)
      return Safe_Int_Result;

   --  Safe modulo (calls proven_math_mod).
   function Mod_Safe (Numerator, Denominator : Long_Long_Integer)
      return Safe_Int_Result;

   --  Checked addition with overflow detection (calls proven_math_add_checked).
   function Add_Checked (A, B : Long_Long_Integer) return Safe_Int_Result;

   --  Checked subtraction with underflow detection
   --  (calls proven_math_sub_checked).
   function Sub_Checked (A, B : Long_Long_Integer) return Safe_Int_Result;

   --  Checked multiplication with overflow detection
   --  (calls proven_math_mul_checked).
   function Mul_Checked (A, B : Long_Long_Integer) return Safe_Int_Result;

   --  Checked exponentiation (calls proven_math_pow_checked).
   function Pow_Checked (Base : Long_Long_Integer; Exp : Natural)
      return Safe_Int_Result;

   --  Safe absolute value (calls proven_math_abs_safe).
   function Abs_Safe (N : Long_Long_Integer) return Safe_Int_Result;

   --  Clamp value to range (calls proven_math_clamp).
   function Clamp (Lo, Hi, Value : Long_Long_Integer) return Long_Long_Integer;

end Proven.Safe_Math;
