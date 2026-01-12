--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Proven.Safe_Network is

   function Parse_IPv4 (Address : String) return Optional_IPv4 is
      Octets    : array (1 .. 4) of Natural;
      Start_Pos : Natural := Address'First;
      Dot_Pos   : Natural;
   begin
      --  Parse first three octets
      for I in 1 .. 3 loop
         Dot_Pos := Index (Address (Start_Pos .. Address'Last), ".");
         if Dot_Pos = 0 then
            return (Valid => False);
         end if;

         begin
            Octets (I) := Natural'Value (Address (Start_Pos .. Dot_Pos - 1));
            if Octets (I) > 255 then
               return (Valid => False);
            end if;
         exception
            when Constraint_Error =>
               return (Valid => False);
         end;

         Start_Pos := Dot_Pos + 1;
      end loop;

      --  Parse fourth octet
      begin
         Octets (4) := Natural'Value (Address (Start_Pos .. Address'Last));
         if Octets (4) > 255 then
            return (Valid => False);
         end if;
      exception
         when Constraint_Error =>
            return (Valid => False);
      end;

      return (Valid => True,
              Value => (A => Octets (1), B => Octets (2),
                        C => Octets (3), D => Octets (4)));
   end Parse_IPv4;

   function Is_Valid_IPv4 (Address : String) return Boolean is
      Result : constant Optional_IPv4 := Parse_IPv4 (Address);
   begin
      return Result.Valid;
   end Is_Valid_IPv4;

   function Is_Private (Address : String) return Boolean is
      Result : constant Optional_IPv4 := Parse_IPv4 (Address);
   begin
      if not Result.Valid then
         return False;
      end if;

      declare
         IP : constant IPv4 := Result.Value;
      begin
         --  10.0.0.0/8
         if IP.A = 10 then
            return True;
         end if;

         --  172.16.0.0/12
         if IP.A = 172 and then IP.B >= 16 and then IP.B <= 31 then
            return True;
         end if;

         --  192.168.0.0/16
         if IP.A = 192 and then IP.B = 168 then
            return True;
         end if;

         return False;
      end;
   end Is_Private;

   function Is_Loopback (Address : String) return Boolean is
      Result : constant Optional_IPv4 := Parse_IPv4 (Address);
   begin
      if not Result.Valid then
         return False;
      end if;
      return Result.Value.A = 127;
   end Is_Loopback;

   function Is_Public (Address : String) return Boolean is
   begin
      return Is_Valid_IPv4 (Address)
             and then not Is_Private (Address)
             and then not Is_Loopback (Address);
   end Is_Public;

   function Format_IPv4 (IP : IPv4) return String is
   begin
      return Trim (Natural'Image (IP.A), Ada.Strings.Left) & "." &
             Trim (Natural'Image (IP.B), Ada.Strings.Left) & "." &
             Trim (Natural'Image (IP.C), Ada.Strings.Left) & "." &
             Trim (Natural'Image (IP.D), Ada.Strings.Left);
   end Format_IPv4;

end Proven.Safe_Network;
