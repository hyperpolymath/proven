--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Proven.Safe_Currency is

   function To_C_Money (M : Money) return FFI.C_Money is
   begin
      return (Minor_Units => long (M.Minor_Units),
              Currency    => int (M.Currency));
   end To_C_Money;

   function To_Ada_Money (C : FFI.C_Money) return Money is
   begin
      return (Minor_Units => Long_Long_Integer (C.Minor_Units),
              Currency    => Currency_Code (C.Currency));
   end To_Ada_Money;

   function To_Money_Result (R : FFI.Money_Result) return Money_Result is
   begin
      if R.Status = FFI.CURRENCY_OK then
         return (Success => True, Value => To_Ada_Money (R.Value));
      else
         return (Success => False, Error_Code => Integer (R.Status));
      end if;
   end To_Money_Result;

   function Parse_Code (S : String) return Code_Result is
      C_Str  : chars_ptr := New_String (S);
      Result : FFI.Currency_Code_Result;
   begin
      Result := FFI.Currency_Parse_Code (C_Str, S'Length);
      Free (C_Str);
      if Result.Status = FFI.CURRENCY_OK then
         return (Success => True, Code => Currency_Code (Result.Code));
      else
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
   end Parse_Code;

   function Is_Valid_Code (S : String) return Boolean is
      C_Str : chars_ptr := New_String (S);
      R     : C_bool;
   begin
      R := FFI.Currency_Is_Valid_Code (C_Str, S'Length);
      Free (C_Str);
      return Boolean (R);
   end Is_Valid_Code;

   function Decimals (Code : Currency_Code) return Natural is
   begin
      return Natural (FFI.Currency_Get_Decimals (int (Code)));
   end Decimals;

   function From_Major
     (Amount : Long_Long_Integer; Code : Currency_Code) return Money
   is
   begin
      return To_Ada_Money (FFI.C_Money_From_Major (long (Amount), int (Code)));
   end From_Major;

   function From_Minor
     (Amount : Long_Long_Integer; Code : Currency_Code) return Money
   is
   begin
      return To_Ada_Money (FFI.C_Money_From_Minor (long (Amount), int (Code)));
   end From_Minor;

   function Zero (Code : Currency_Code) return Money is
   begin
      return To_Ada_Money (FFI.C_Money_Zero (int (Code)));
   end Zero;

   function Add (A, B : Money) return Money_Result is
   begin
      return To_Money_Result (FFI.C_Money_Add (To_C_Money (A), To_C_Money (B)));
   end Add;

   function Subtract (A, B : Money) return Money_Result is
   begin
      return To_Money_Result (FFI.C_Money_Sub (To_C_Money (A), To_C_Money (B)));
   end Subtract;

   function Multiply (M : Money; Scalar : Long_Long_Integer) return Money_Result is
   begin
      return To_Money_Result (FFI.C_Money_Mul (To_C_Money (M), long (Scalar)));
   end Multiply;

   function Divide
     (M : Money; Divisor : Long_Long_Integer) return Money_Result
   is
   begin
      return To_Money_Result (FFI.C_Money_Div (To_C_Money (M), long (Divisor)));
   end Divide;

   function Is_Zero (M : Money) return Boolean is
   begin
      return Boolean (FFI.C_Money_Is_Zero (To_C_Money (M)));
   end Is_Zero;

   function Is_Positive (M : Money) return Boolean is
   begin
      return Boolean (FFI.C_Money_Is_Positive (To_C_Money (M)));
   end Is_Positive;

   function Is_Negative (M : Money) return Boolean is
   begin
      return Boolean (FFI.C_Money_Is_Negative (To_C_Money (M)));
   end Is_Negative;

   function Major (M : Money) return Long_Long_Integer is
   begin
      return Long_Long_Integer (FFI.C_Money_Get_Major (To_C_Money (M)));
   end Major;

   function Minor (M : Money) return Long_Long_Integer is
   begin
      return Long_Long_Integer (FFI.C_Money_Get_Minor (To_C_Money (M)));
   end Minor;

   function Format (M : Money) return Format_Result is
      Buf    : char_array (0 .. size_t (Max_Format_Len));
      C_Buf  : chars_ptr;
      Status : int;
   begin
      C_Buf := New_Char_Array (Buf);
      Status := FFI.C_Money_Format
        (To_C_Money (M), C_Buf, size_t (Max_Format_Len) + 1);
      if Status /= FFI.CURRENCY_OK then
         Free (C_Buf);
         return (Success => False, Error_Code => Integer (Status));
      end if;
      declare
         S   : constant String := Value (C_Buf);
         Res : Format_Result (Success => True);
      begin
         Free (C_Buf);
         Res.Value := (others => ' ');
         Res.Last := Natural'Min (S'Length, Max_Format_Len);
         Res.Value (1 .. Res.Last) := S (S'First .. S'First + Res.Last - 1);
         return Res;
      end;
   end Format;

end Proven.Safe_Currency;
