--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C; use Interfaces.C;

package body Proven.Safe_Float is

   function To_Result (R : FFI.Float_Result) return Safe_Float_Result is
   begin
      if R.Status = FFI.PROVEN_OK then
         return (Success => True, Value => Long_Float (R.Value));
      else
         return (Success => False, Error_Code => Integer (R.Status));
      end if;
   end To_Result;

   function Div_Safe (A, B : Long_Float) return Safe_Float_Result is
   begin
      return To_Result (FFI.Float_Div (double (A), double (B)));
   end Div_Safe;

   function Sqrt_Safe (X : Long_Float) return Safe_Float_Result is
   begin
      return To_Result (FFI.Float_Sqrt (double (X)));
   end Sqrt_Safe;

   function Ln_Safe (X : Long_Float) return Safe_Float_Result is
   begin
      return To_Result (FFI.Float_Ln (double (X)));
   end Ln_Safe;

   function Is_Finite (X : Long_Float) return Boolean is
   begin
      return Boolean (FFI.Float_Is_Finite (double (X)));
   end Is_Finite;

   function Is_NaN (X : Long_Float) return Boolean is
   begin
      return Boolean (FFI.Float_Is_NaN (double (X)));
   end Is_NaN;

end Proven.Safe_Float;
