--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe floating-point operations -- thin FFI wrapper over libproven.
--  NaN/infinity handling performed by the formally verified Idris2/Zig core.

package Proven.Safe_Float is

   type Safe_Float_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : Long_Float;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Safe floating-point division (calls proven_float_div).
   function Div_Safe (A, B : Long_Float) return Safe_Float_Result;

   --  Safe square root (calls proven_float_sqrt).
   function Sqrt_Safe (X : Long_Float) return Safe_Float_Result;

   --  Safe natural logarithm (calls proven_float_ln).
   function Ln_Safe (X : Long_Float) return Safe_Float_Result;

   --  Check if value is finite (calls proven_float_is_finite).
   function Is_Finite (X : Long_Float) return Boolean;

   --  Check if value is NaN (calls proven_float_is_nan).
   function Is_NaN (X : Long_Float) return Boolean;

end Proven.Safe_Float;
