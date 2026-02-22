--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C; use Interfaces.C;

package body Proven.Safe_Math is

   function To_Result (R : FFI.Int_Result) return Safe_Int_Result is
   begin
      if R.Status = FFI.PROVEN_OK then
         return (Success => True, Value => Long_Long_Integer (R.Value));
      else
         return (Success => False, Error_Code => Integer (R.Status));
      end if;
   end To_Result;

   function Div_Safe (Numerator, Denominator : Long_Long_Integer)
      return Safe_Int_Result
   is
   begin
      return To_Result (FFI.Math_Div (long (Numerator), long (Denominator)));
   end Div_Safe;

   function Mod_Safe (Numerator, Denominator : Long_Long_Integer)
      return Safe_Int_Result
   is
   begin
      return To_Result (FFI.Math_Mod (long (Numerator), long (Denominator)));
   end Mod_Safe;

   function Add_Checked (A, B : Long_Long_Integer) return Safe_Int_Result is
   begin
      return To_Result (FFI.Math_Add_Checked (long (A), long (B)));
   end Add_Checked;

   function Sub_Checked (A, B : Long_Long_Integer) return Safe_Int_Result is
   begin
      return To_Result (FFI.Math_Sub_Checked (long (A), long (B)));
   end Sub_Checked;

   function Mul_Checked (A, B : Long_Long_Integer) return Safe_Int_Result is
   begin
      return To_Result (FFI.Math_Mul_Checked (long (A), long (B)));
   end Mul_Checked;

   function Pow_Checked (Base : Long_Long_Integer; Exp : Natural)
      return Safe_Int_Result
   is
   begin
      return To_Result
        (FFI.Math_Pow_Checked (long (Base), unsigned (Exp)));
   end Pow_Checked;

   function Abs_Safe (N : Long_Long_Integer) return Safe_Int_Result is
   begin
      return To_Result (FFI.Math_Abs_Safe (long (N)));
   end Abs_Safe;

   function Clamp (Lo, Hi, Value : Long_Long_Integer)
      return Long_Long_Integer
   is
   begin
      return Long_Long_Integer
        (FFI.Math_Clamp (long (Lo), long (Hi), long (Value)));
   end Clamp;

end Proven.Safe_Math;
