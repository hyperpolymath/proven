--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Proven.Safe_Url is

   Hex_Chars : constant String := "0123456789ABCDEF";

   function Hex_Char_To_Nibble (C : Character) return Integer is
   begin
      case C is
         when '0' .. '9' =>
            return Character'Pos (C) - Character'Pos ('0');
         when 'a' .. 'f' =>
            return Character'Pos (C) - Character'Pos ('a') + 10;
         when 'A' .. 'F' =>
            return Character'Pos (C) - Character'Pos ('A') + 10;
         when others =>
            return -1;
      end case;
   end Hex_Char_To_Nibble;

   function Parse (URL : String) return Parse_Result is
      Scheme_End     : Natural := 0;
      Authority_Start : Natural := 0;
      Authority_End  : Natural := 0;
      Host_Start     : Natural := 0;
      Host_End       : Natural := 0;
      Port_Start     : Natural := 0;
      Path_Start     : Natural := 0;
      Query_Start    : Natural := 0;
      Fragment_Start : Natural := 0;
      Port_Value     : Natural := 0;
      Result         : URL_Components;
   begin
      if URL'Length = 0 then
         return (Valid => False);
      end if;

      --  Find scheme (ends with "://")
      for I in URL'First .. URL'Last - 2 loop
         if URL (I) = ':' and then
            I + 2 <= URL'Last and then
            URL (I + 1) = '/' and then URL (I + 2) = '/'
         then
            Scheme_End := I - 1;
            Authority_Start := I + 3;
            exit;
         end if;
      end loop;

      if Scheme_End = 0 then
         return (Valid => False);
      end if;

      Result.Scheme := To_Unbounded_String (
         To_Lower (URL (URL'First .. Scheme_End)));

      --  Find end of authority (at path, query, or fragment)
      Authority_End := URL'Last;
      Path_Start := 0;

      for I in Authority_Start .. URL'Last loop
         if URL (I) = '/' then
            Authority_End := I - 1;
            Path_Start := I;
            exit;
         elsif URL (I) = '?' then
            Authority_End := I - 1;
            Query_Start := I + 1;
            exit;
         elsif URL (I) = '#' then
            Authority_End := I - 1;
            Fragment_Start := I + 1;
            exit;
         end if;
      end loop;

      --  Parse host and port from authority
      Host_Start := Authority_Start;
      Host_End := Authority_End;

      for I in reverse Authority_Start .. Authority_End loop
         if URL (I) = ':' then
            Host_End := I - 1;
            Port_Start := I + 1;
            exit;
         end if;
      end loop;

      if Host_Start <= Host_End then
         Result.Host := To_Unbounded_String (
            To_Lower (URL (Host_Start .. Host_End)));
      else
         Result.Host := Null_Unbounded_String;
      end if;

      --  Parse port
      if Port_Start > 0 then
         for I in Port_Start .. Authority_End loop
            if URL (I) in '0' .. '9' then
               Port_Value := Port_Value * 10 +
                  (Character'Pos (URL (I)) - Character'Pos ('0'));
            else
               return (Valid => False);
            end if;
         end loop;
      end if;
      Result.Port := Port_Value;

      --  Find query and fragment in path portion
      if Path_Start > 0 and then Query_Start = 0 and then Fragment_Start = 0
      then
         for I in Path_Start .. URL'Last loop
            if URL (I) = '?' and then Query_Start = 0 then
               Query_Start := I + 1;
            elsif URL (I) = '#' and then Fragment_Start = 0 then
               Fragment_Start := I + 1;
               exit;
            end if;
         end loop;
      end if;

      --  Extract path
      if Path_Start > 0 then
         declare
            Path_End : Natural := URL'Last;
         begin
            if Query_Start > 0 then
               Path_End := Query_Start - 2;
            elsif Fragment_Start > 0 then
               Path_End := Fragment_Start - 2;
            end if;
            if Path_Start <= Path_End then
               Result.Path := To_Unbounded_String (URL (Path_Start .. Path_End));
            else
               Result.Path := Null_Unbounded_String;
            end if;
         end;
      else
         Result.Path := Null_Unbounded_String;
      end if;

      --  Extract query
      if Query_Start > 0 then
         declare
            Query_End : Natural := URL'Last;
         begin
            if Fragment_Start > 0 then
               Query_End := Fragment_Start - 2;
            end if;
            if Query_Start <= Query_End then
               Result.Query := To_Unbounded_String (
                  URL (Query_Start .. Query_End));
            else
               Result.Query := Null_Unbounded_String;
            end if;
         end;
      else
         Result.Query := Null_Unbounded_String;
      end if;

      --  Extract fragment
      if Fragment_Start > 0 and then Fragment_Start <= URL'Last then
         Result.Fragment := To_Unbounded_String (
            URL (Fragment_Start .. URL'Last));
      else
         Result.Fragment := Null_Unbounded_String;
      end if;

      return (Valid => True, Components => Result);
   end Parse;

   function Is_Valid (URL : String) return Boolean is
      Result : constant Parse_Result := Parse (URL);
   begin
      return Result.Valid;
   end Is_Valid;

   function Get_Scheme (URL : String) return Optional_String is
      Result : constant Parse_Result := Parse (URL);
   begin
      if Result.Valid then
         return (Valid => True, Value => Result.Components.Scheme);
      else
         return (Valid => False);
      end if;
   end Get_Scheme;

   function Get_Host (URL : String) return Optional_String is
      Result : constant Parse_Result := Parse (URL);
   begin
      if Result.Valid then
         return (Valid => True, Value => Result.Components.Host);
      else
         return (Valid => False);
      end if;
   end Get_Host;

   function Get_Port (URL : String) return Natural is
      Result : constant Parse_Result := Parse (URL);
   begin
      if Result.Valid then
         return Result.Components.Port;
      else
         return 0;
      end if;
   end Get_Port;

   function Get_Path (URL : String) return Optional_String is
      Result : constant Parse_Result := Parse (URL);
   begin
      if Result.Valid then
         return (Valid => True, Value => Result.Components.Path);
      else
         return (Valid => False);
      end if;
   end Get_Path;

   function Get_Query (URL : String) return Optional_String is
      Result : constant Parse_Result := Parse (URL);
   begin
      if Result.Valid then
         return (Valid => True, Value => Result.Components.Query);
      else
         return (Valid => False);
      end if;
   end Get_Query;

   function Get_Fragment (URL : String) return Optional_String is
      Result : constant Parse_Result := Parse (URL);
   begin
      if Result.Valid then
         return (Valid => True, Value => Result.Components.Fragment);
      else
         return (Valid => False);
      end if;
   end Get_Fragment;

   function Encode (Value : String) return String is
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
   end Encode;

   function Decode (Value : String) return Optional_String is
      Result : Unbounded_String := Null_Unbounded_String;
      I      : Natural := Value'First;
      High   : Integer;
      Low    : Integer;
   begin
      while I <= Value'Last loop
         if Value (I) = '%' then
            if I + 2 > Value'Last then
               return (Valid => False);
            end if;
            High := Hex_Char_To_Nibble (Value (I + 1));
            Low := Hex_Char_To_Nibble (Value (I + 2));
            if High < 0 or else Low < 0 then
               return (Valid => False);
            end if;
            Append (Result, Character'Val (High * 16 + Low));
            I := I + 3;
         elsif Value (I) = '+' then
            Append (Result, ' ');
            I := I + 1;
         else
            Append (Result, Value (I));
            I := I + 1;
         end if;
      end loop;
      return (Valid => True, Value => Result);
   end Decode;

   function Is_Https (URL : String) return Boolean is
      Result : constant Optional_String := Get_Scheme (URL);
   begin
      if Result.Valid then
         return To_String (Result.Value) = "https";
      else
         return False;
      end if;
   end Is_Https;

   function Is_Secure (URL : String) return Boolean is
      Result : constant Optional_String := Get_Scheme (URL);
      Scheme : constant String :=
         (if Result.Valid then To_String (Result.Value) else "");
   begin
      return Scheme = "https" or else Scheme = "wss" or else
             Scheme = "ftps" or else Scheme = "ssh";
   end Is_Secure;

   function Normalize (URL : String) return Optional_String is
      Result : constant Parse_Result := Parse (URL);
      Normalized : Unbounded_String;
   begin
      if not Result.Valid then
         return (Valid => False);
      end if;

      Normalized := Result.Components.Scheme & "://";
      Append (Normalized, Result.Components.Host);

      if Result.Components.Port > 0 then
         Append (Normalized, ":" & Natural'Image (Result.Components.Port));
      end if;

      Append (Normalized, Result.Components.Path);

      if Length (Result.Components.Query) > 0 then
         Append (Normalized, "?");
         Append (Normalized, Result.Components.Query);
      end if;

      if Length (Result.Components.Fragment) > 0 then
         Append (Normalized, "#");
         Append (Normalized, Result.Components.Fragment);
      end if;

      return (Valid => True, Value => Normalized);
   end Normalize;

   function Join (Base : String; Relative : String) return Optional_String is
      Base_Result : constant Parse_Result := Parse (Base);
      Joined      : Unbounded_String;
   begin
      if not Base_Result.Valid then
         return (Valid => False);
      end if;

      --  If relative starts with /, it replaces the path
      if Relative'Length > 0 and then Relative (Relative'First) = '/' then
         Joined := Base_Result.Components.Scheme & "://";
         Append (Joined, Base_Result.Components.Host);
         if Base_Result.Components.Port > 0 then
            Append (Joined, ":" &
               Natural'Image (Base_Result.Components.Port));
         end if;
         Append (Joined, Relative);
         return (Valid => True, Value => Joined);
      end if;

      --  Otherwise append to existing path
      Joined := Base_Result.Components.Scheme & "://";
      Append (Joined, Base_Result.Components.Host);
      if Base_Result.Components.Port > 0 then
         Append (Joined, ":" &
            Natural'Image (Base_Result.Components.Port));
      end if;

      declare
         Base_Path : constant String := To_String (Base_Result.Components.Path);
         Last_Slash : Natural := 0;
      begin
         --  Find last slash in base path
         for I in reverse Base_Path'Range loop
            if Base_Path (I) = '/' then
               Last_Slash := I;
               exit;
            end if;
         end loop;

         if Last_Slash > 0 then
            Append (Joined, Base_Path (Base_Path'First .. Last_Slash));
         else
            Append (Joined, "/");
         end if;
         Append (Joined, Relative);
      end;

      return (Valid => True, Value => Joined);
   end Join;

end Proven.Safe_Url;
