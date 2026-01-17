--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

package body Proven.Safe_Json is

   function Validate (JSON : String) return Validation_Result is
      Pos    : Natural := JSON'First;
      Line   : Natural := 1;
      Column : Natural := 1;
      Depth  : Natural := 0;

      procedure Skip_Whitespace;
      function Parse_Value return Boolean;
      function Parse_String return Boolean;
      function Parse_Number return Boolean;
      function Parse_Array return Boolean;
      function Parse_Object return Boolean;
      function Parse_Literal (Lit : String) return Boolean;
      function Current_Char return Character;
      procedure Advance;
      function At_End return Boolean;

      function Current_Char return Character is
      begin
         if Pos <= JSON'Last then
            return JSON (Pos);
         else
            return ASCII.NUL;
         end if;
      end Current_Char;

      procedure Advance is
      begin
         if Pos <= JSON'Last then
            if JSON (Pos) = ASCII.LF then
               Line := Line + 1;
               Column := 1;
            else
               Column := Column + 1;
            end if;
            Pos := Pos + 1;
         end if;
      end Advance;

      function At_End return Boolean is
      begin
         return Pos > JSON'Last;
      end At_End;

      procedure Skip_Whitespace is
      begin
         while not At_End and then
               (Current_Char = ' ' or else Current_Char = ASCII.HT or else
                Current_Char = ASCII.CR or else Current_Char = ASCII.LF)
         loop
            Advance;
         end loop;
      end Skip_Whitespace;

      function Parse_Literal (Lit : String) return Boolean is
      begin
         for I in Lit'Range loop
            if At_End or else Current_Char /= Lit (I) then
               return False;
            end if;
            Advance;
         end loop;
         return True;
      end Parse_Literal;

      function Parse_String return Boolean is
      begin
         if Current_Char /= '"' then
            return False;
         end if;
         Advance;

         while not At_End loop
            if Current_Char = '"' then
               Advance;
               return True;
            elsif Current_Char = '\' then
               Advance;
               if At_End then
                  return False;
               end if;
               case Current_Char is
                  when '"' | '\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' =>
                     Advance;
                  when 'u' =>
                     Advance;
                     for I in 1 .. 4 loop
                        if At_End then
                           return False;
                        end if;
                        case Current_Char is
                           when '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' =>
                              Advance;
                           when others =>
                              return False;
                        end case;
                     end loop;
                  when others =>
                     return False;
               end case;
            elsif Character'Pos (Current_Char) < 32 then
               return False;
            else
               Advance;
            end if;
         end loop;
         return False;
      end Parse_String;

      function Parse_Number return Boolean is
         Has_Digit : Boolean := False;
      begin
         --  Optional minus
         if Current_Char = '-' then
            Advance;
         end if;

         --  Integer part
         if Current_Char = '0' then
            Advance;
            Has_Digit := True;
         elsif Current_Char in '1' .. '9' then
            Has_Digit := True;
            while not At_End and then Current_Char in '0' .. '9' loop
               Advance;
            end loop;
         end if;

         if not Has_Digit then
            return False;
         end if;

         --  Optional fraction
         if not At_End and then Current_Char = '.' then
            Advance;
            if At_End or else Current_Char not in '0' .. '9' then
               return False;
            end if;
            while not At_End and then Current_Char in '0' .. '9' loop
               Advance;
            end loop;
         end if;

         --  Optional exponent
         if not At_End and then (Current_Char = 'e' or else Current_Char = 'E')
         then
            Advance;
            if not At_End and then
               (Current_Char = '+' or else Current_Char = '-')
            then
               Advance;
            end if;
            if At_End or else Current_Char not in '0' .. '9' then
               return False;
            end if;
            while not At_End and then Current_Char in '0' .. '9' loop
               Advance;
            end loop;
         end if;

         return True;
      end Parse_Number;

      function Parse_Array return Boolean is
      begin
         if Current_Char /= '[' then
            return False;
         end if;
         Advance;
         Depth := Depth + 1;

         if Depth > Max_Depth then
            return False;
         end if;

         Skip_Whitespace;
         if not At_End and then Current_Char = ']' then
            Advance;
            Depth := Depth - 1;
            return True;
         end if;

         loop
            if not Parse_Value then
               return False;
            end if;
            Skip_Whitespace;
            if At_End then
               return False;
            end if;
            if Current_Char = ']' then
               Advance;
               Depth := Depth - 1;
               return True;
            elsif Current_Char = ',' then
               Advance;
               Skip_Whitespace;
            else
               return False;
            end if;
         end loop;
      end Parse_Array;

      function Parse_Object return Boolean is
      begin
         if Current_Char /= '{' then
            return False;
         end if;
         Advance;
         Depth := Depth + 1;

         if Depth > Max_Depth then
            return False;
         end if;

         Skip_Whitespace;
         if not At_End and then Current_Char = '}' then
            Advance;
            Depth := Depth - 1;
            return True;
         end if;

         loop
            Skip_Whitespace;
            if not Parse_String then
               return False;
            end if;
            Skip_Whitespace;
            if At_End or else Current_Char /= ':' then
               return False;
            end if;
            Advance;
            Skip_Whitespace;
            if not Parse_Value then
               return False;
            end if;
            Skip_Whitespace;
            if At_End then
               return False;
            end if;
            if Current_Char = '}' then
               Advance;
               Depth := Depth - 1;
               return True;
            elsif Current_Char = ',' then
               Advance;
            else
               return False;
            end if;
         end loop;
      end Parse_Object;

      function Parse_Value return Boolean is
      begin
         Skip_Whitespace;
         if At_End then
            return False;
         end if;

         case Current_Char is
            when 'n' => return Parse_Literal ("null");
            when 't' => return Parse_Literal ("true");
            when 'f' => return Parse_Literal ("false");
            when '"' => return Parse_String;
            when '[' => return Parse_Array;
            when '{' => return Parse_Object;
            when '-' | '0' .. '9' => return Parse_Number;
            when others => return False;
         end case;
      end Parse_Value;

   begin
      if JSON'Length = 0 then
         return (Valid   => False,
                 Message => To_Unbounded_String ("Empty JSON"),
                 Line    => 1,
                 Column  => 1);
      end if;

      Skip_Whitespace;
      if not Parse_Value then
         return (Valid   => False,
                 Message => To_Unbounded_String ("Invalid JSON syntax"),
                 Line    => Line,
                 Column  => Column);
      end if;

      Skip_Whitespace;
      if not At_End then
         return (Valid   => False,
                 Message => To_Unbounded_String ("Unexpected content after value"),
                 Line    => Line,
                 Column  => Column);
      end if;

      return (Valid   => True,
              Message => Null_Unbounded_String,
              Line    => 0,
              Column  => 0);
   end Validate;

   function Is_Valid (JSON : String) return Boolean is
   begin
      return Validate (JSON).Valid;
   end Is_Valid;

   function Escape_String (Value : String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      Append (Result, '"');
      for C of Value loop
         case C is
            when '"'      => Append (Result, "\""");
            when '\'      => Append (Result, "\\");
            when ASCII.BS => Append (Result, "\b");
            when ASCII.FF => Append (Result, "\f");
            when ASCII.LF => Append (Result, "\n");
            when ASCII.CR => Append (Result, "\r");
            when ASCII.HT => Append (Result, "\t");
            when others   =>
               if Character'Pos (C) < 32 then
                  --  Control characters as \u00XX
                  declare
                     Hex : constant String := "0123456789abcdef";
                     Val : constant Natural := Character'Pos (C);
                  begin
                     Append (Result, "\u00");
                     Append (Result, Hex (Val / 16 + 1));
                     Append (Result, Hex (Val mod 16 + 1));
                  end;
               else
                  Append (Result, C);
               end if;
         end case;
      end loop;
      Append (Result, '"');
      return To_String (Result);
   end Escape_String;

   function Unescape_String (Value : String) return Unescape_Result is
      Result : Unbounded_String := Null_Unbounded_String;
      I      : Natural := Value'First;

      function Hex_Digit (C : Character) return Natural is
      begin
         case C is
            when '0' .. '9' => return Character'Pos (C) - Character'Pos ('0');
            when 'a' .. 'f' => return Character'Pos (C) - Character'Pos ('a') + 10;
            when 'A' .. 'F' => return Character'Pos (C) - Character'Pos ('A') + 10;
            when others     => return 16;  --  Invalid
         end case;
      end Hex_Digit;

   begin
      while I <= Value'Last loop
         if Value (I) = '\' then
            if I + 1 > Value'Last then
               return (Valid => False);
            end if;
            I := I + 1;
            case Value (I) is
               when '"'  => Append (Result, '"');
               when '\' => Append (Result, '\');
               when '/'  => Append (Result, '/');
               when 'b'  => Append (Result, ASCII.BS);
               when 'f'  => Append (Result, ASCII.FF);
               when 'n'  => Append (Result, ASCII.LF);
               when 'r'  => Append (Result, ASCII.CR);
               when 't'  => Append (Result, ASCII.HT);
               when 'u'  =>
                  if I + 4 > Value'Last then
                     return (Valid => False);
                  end if;
                  declare
                     Codepoint : Natural := 0;
                     D         : Natural;
                  begin
                     for J in 1 .. 4 loop
                        D := Hex_Digit (Value (I + J));
                        if D > 15 then
                           return (Valid => False);
                        end if;
                        Codepoint := Codepoint * 16 + D;
                     end loop;
                     if Codepoint < 256 then
                        Append (Result, Character'Val (Codepoint));
                     else
                        --  Unicode beyond ASCII - simplified handling
                        Append (Result, '?');
                     end if;
                     I := I + 4;
                  end;
               when others =>
                  return (Valid => False);
            end case;
            I := I + 1;
         else
            Append (Result, Value (I));
            I := I + 1;
         end if;
      end loop;
      return (Valid => True, Value => Result);
   end Unescape_String;

   function Pretty_Print
     (JSON : String; Indent : Natural := 2) return Unbounded_String
   is
      Result      : Unbounded_String := Null_Unbounded_String;
      Depth_Level : Natural := 0;
      In_String   : Boolean := False;
      I           : Natural := JSON'First;

      procedure Add_Newline is
      begin
         Append (Result, ASCII.LF);
         for J in 1 .. Depth_Level * Indent loop
            Append (Result, ' ');
         end loop;
      end Add_Newline;

   begin
      if not Is_Valid (JSON) then
         return Result;
      end if;

      while I <= JSON'Last loop
         declare
            C : constant Character := JSON (I);
         begin
            if In_String then
               Append (Result, C);
               if C = '"' then
                  In_String := False;
               elsif C = '\' and then I < JSON'Last then
                  I := I + 1;
                  Append (Result, JSON (I));
               end if;
            else
               case C is
                  when ' ' | ASCII.HT | ASCII.CR | ASCII.LF =>
                     null;  --  Skip whitespace
                  when '"' =>
                     Append (Result, C);
                     In_String := True;
                  when '{' | '[' =>
                     Append (Result, C);
                     Depth_Level := Depth_Level + 1;
                     if I < JSON'Last then
                        --  Check for empty object/array
                        declare
                           J : Natural := I + 1;
                        begin
                           while J <= JSON'Last and then
                                 (JSON (J) = ' ' or else
                                  JSON (J) = ASCII.HT or else
                                  JSON (J) = ASCII.CR or else
                                  JSON (J) = ASCII.LF)
                           loop
                              J := J + 1;
                           end loop;
                           if J <= JSON'Last and then
                              ((C = '{' and then JSON (J) = '}') or else
                               (C = '[' and then JSON (J) = ']'))
                           then
                              null;  --  Don't add newline for empty
                           else
                              Add_Newline;
                           end if;
                        end;
                     end if;
                  when '}' | ']' =>
                     Depth_Level := Depth_Level - 1;
                     Add_Newline;
                     Append (Result, C);
                  when ':' =>
                     Append (Result, ": ");
                  when ',' =>
                     Append (Result, C);
                     Add_Newline;
                  when others =>
                     Append (Result, C);
               end case;
            end if;
            I := I + 1;
         end;
      end loop;

      return Result;
   end Pretty_Print;

   function Minify (JSON : String) return Unbounded_String is
      Result    : Unbounded_String := Null_Unbounded_String;
      In_String : Boolean := False;
   begin
      for I in JSON'Range loop
         declare
            C : constant Character := JSON (I);
         begin
            if In_String then
               Append (Result, C);
               if C = '"' then
                  In_String := False;
               elsif C = '\' and then I < JSON'Last then
                  null;  --  Next char will be added normally
               end if;
            else
               case C is
                  when ' ' | ASCII.HT | ASCII.CR | ASCII.LF =>
                     null;  --  Skip whitespace
                  when '"' =>
                     Append (Result, C);
                     In_String := True;
                  when others =>
                     Append (Result, C);
               end case;
            end if;
         end;
      end loop;
      return Result;
   end Minify;

   function Get_String (JSON : String; Path : String) return String_Result is
      pragma Unreferenced (JSON, Path);
   begin
      --  Simple path extraction not implemented in this version
      return (Valid => False);
   end Get_String;

   function Get_Number (JSON : String; Path : String) return Number_Result is
      pragma Unreferenced (JSON, Path);
   begin
      return (Valid => False);
   end Get_Number;

   function Get_Boolean (JSON : String; Path : String) return Boolean_Result is
      pragma Unreferenced (JSON, Path);
   begin
      return (Valid => False);
   end Get_Boolean;

   function Has_Path (JSON : String; Path : String) return Boolean is
      pragma Unreferenced (JSON, Path);
   begin
      return False;
   end Has_Path;

   function Get_Kind (JSON : String; Path : String) return Kind_Result is
      pragma Unreferenced (JSON, Path);
   begin
      return (Valid => False);
   end Get_Kind;

end Proven.Safe_Json;
