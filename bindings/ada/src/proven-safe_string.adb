--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Proven.Safe_String is

   function Escape_Html (Value : String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for C of Value loop
         case C is
            when '&'    => Append (Result, "&amp;");
            when '<'    => Append (Result, "&lt;");
            when '>'    => Append (Result, "&gt;");
            when '"'    => Append (Result, "&quot;");
            when '''    => Append (Result, "&#x27;");
            when others => Append (Result, C);
         end case;
      end loop;
      return To_String (Result);
   end Escape_Html;

   function Escape_Sql (Value : String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for C of Value loop
         case C is
            when ''' => Append (Result, "''");
            when others => Append (Result, C);
         end case;
      end loop;
      return To_String (Result);
   end Escape_Sql;

   function Escape_Js (Value : String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for C of Value loop
         case C is
            when '\'         => Append (Result, "\\");
            when '"'         => Append (Result, "\""");
            when '''         => Append (Result, "\'");
            when ASCII.LF    => Append (Result, "\n");
            when ASCII.CR    => Append (Result, "\r");
            when ASCII.HT    => Append (Result, "\t");
            when others      => Append (Result, C);
         end case;
      end loop;
      return To_String (Result);
   end Escape_Js;

   Hex_Chars : constant String := "0123456789ABCDEF";

   function Escape_Url (Value : String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for C of Value loop
         case C is
            when 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'
               | '-' | '_' | '.' | '~' =>
               Append (Result, C);
            when others =>
               Append (Result, '%');
               Append (Result, Hex_Chars (Character'Pos (C) / 16 + 1));
               Append (Result, Hex_Chars (Character'Pos (C) mod 16 + 1));
         end case;
      end loop;
      return To_String (Result);
   end Escape_Url;

   function Truncate_Safe
     (Value   : String;
      Max_Len : Positive;
      Suffix  : String := "...") return String is
   begin
      if Value'Length <= Max_Len then
         return Value;
      end if;

      if Max_Len <= Suffix'Length then
         return Suffix (Suffix'First .. Suffix'First + Max_Len - 1);
      end if;

      return Value (Value'First .. Value'First + Max_Len - Suffix'Length - 1)
             & Suffix;
   end Truncate_Safe;

end Proven.Safe_String;
