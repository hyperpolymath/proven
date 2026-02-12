--  SPDX-License-Identifier: Apache-2.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Proven.Safe_Path is

   function Has_Traversal (Path : String) return Boolean is
   begin
      return Index (Path, "..") > 0 or else Index (Path, "~") > 0;
   end Has_Traversal;

   function Is_Safe (Path : String) return Boolean is
   begin
      return not Has_Traversal (Path);
   end Is_Safe;

   function Sanitize_Filename (Filename : String) return String is
      Result    : Unbounded_String := Null_Unbounded_String;
      Prev_Dot  : Boolean := False;
   begin
      for C of Filename loop
         case C is
            when '.' =>
               if Prev_Dot then
                  Append (Result, '_');
                  Prev_Dot := False;
               else
                  Prev_Dot := True;
                  Append (Result, '.');
               end if;
            when '/' | '\' | '<' | '>' | ':' | '"' | '|' | '?' | '*' =>
               Prev_Dot := False;
               Append (Result, '_');
            when ASCII.NUL =>
               Prev_Dot := False;
               Append (Result, '_');
            when others =>
               Prev_Dot := False;
               Append (Result, C);
         end case;
      end loop;
      return To_String (Result);
   end Sanitize_Filename;

   function Safe_Join (Base : String; Part : String) return Optional_String is
   begin
      if Has_Traversal (Part) then
         return (Valid => False);
      end if;

      declare
         Sanitized : constant String := Sanitize_Filename (Part);
      begin
         return (Valid => True,
                 Value => To_Unbounded_String (Base & "/" & Sanitized));
      end;
   end Safe_Join;

end Proven.Safe_Path;
