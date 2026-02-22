--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C; use Interfaces.C;

package body Proven.Safe_Json is

   type Byte_Array is array (Natural range <>) of aliased unsigned_char;

   function To_Bytes (S : String) return Byte_Array is
      Result : Byte_Array (0 .. S'Length - 1);
   begin
      for I in S'Range loop
         Result (I - S'First) := unsigned_char (Character'Pos (S (I)));
      end loop;
      return Result;
   end To_Bytes;

   function Is_Valid (JSON : String) return Safe_Bool_Result is
      Bytes  : aliased Byte_Array := To_Bytes (JSON);
      Result : FFI.Bool_Result;
   begin
      if JSON'Length = 0 then
         return (Success => True, Value => False);
      end if;
      Result := FFI.Json_Is_Valid
        (Bytes (Bytes'First)'Access, Bytes'Length);
      if Result.Status /= FFI.PROVEN_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      return (Success => True, Value => Boolean (Result.Value));
   end Is_Valid;

   function Get_Type (JSON : String) return Safe_Kind_Result is
      Bytes    : aliased Byte_Array := To_Bytes (JSON);
      C_Result : int;
   begin
      if JSON'Length = 0 then
         return (Success => True, Kind => JSON_Invalid);
      end if;
      C_Result := FFI.Json_Get_Type
        (Bytes (Bytes'First)'Access, Bytes'Length);
      case C_Result is
         when FFI.PROVEN_JSON_NULL    => return (Success => True, Kind => JSON_Null);
         when FFI.PROVEN_JSON_BOOL    => return (Success => True, Kind => JSON_Boolean);
         when FFI.PROVEN_JSON_NUMBER  => return (Success => True, Kind => JSON_Number);
         when FFI.PROVEN_JSON_STRING  => return (Success => True, Kind => JSON_String);
         when FFI.PROVEN_JSON_ARRAY   => return (Success => True, Kind => JSON_Array);
         when FFI.PROVEN_JSON_OBJECT  => return (Success => True, Kind => JSON_Object);
         when others                  => return (Success => True, Kind => JSON_Invalid);
      end case;
   end Get_Type;

end Proven.Safe_Json;
