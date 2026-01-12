--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Proven.Safe_Email is

   function Is_Valid (Email : String) return Boolean is
      At_Pos : constant Natural := Index (Email, "@");
   begin
      if At_Pos = 0 then
         return False;
      end if;

      declare
         Local_Part : constant String := Email (Email'First .. At_Pos - 1);
         Domain     : constant String := Email (At_Pos + 1 .. Email'Last);
      begin
         if Local_Part'Length = 0 then
            return False;
         end if;
         if Domain'Length < 3 then
            return False;
         end if;
         if Index (Domain, ".") = 0 then
            return False;
         end if;
         if Domain (Domain'First) = '.' then
            return False;
         end if;
         if Domain (Domain'Last) = '.' then
            return False;
         end if;
         return True;
      end;
   end Is_Valid;

   function Split_Email (Email : String) return Optional_Email_Parts is
   begin
      if not Is_Valid (Email) then
         return (Valid => False);
      end if;

      declare
         At_Pos     : constant Natural := Index (Email, "@");
         Local_Part : constant String := Email (Email'First .. At_Pos - 1);
         Domain     : constant String := Email (At_Pos + 1 .. Email'Last);
      begin
         return (Valid => True,
                 Parts => (Local_Part => To_Unbounded_String (Local_Part),
                           Domain     => To_Unbounded_String (Domain)));
      end;
   end Split_Email;

   function Get_Domain (Email : String) return Optional_String is
      Result : constant Optional_Email_Parts := Split_Email (Email);
   begin
      if not Result.Valid then
         return (Valid => False);
      end if;
      return (Valid => True, Value => Result.Parts.Domain);
   end Get_Domain;

   function Get_Local_Part (Email : String) return Optional_String is
      Result : constant Optional_Email_Parts := Split_Email (Email);
   begin
      if not Result.Valid then
         return (Valid => False);
      end if;
      return (Valid => True, Value => Result.Parts.Local_Part);
   end Get_Local_Part;

   function Normalize (Email : String) return Optional_String is
      Result : constant Optional_Email_Parts := Split_Email (Email);
   begin
      if not Result.Valid then
         return (Valid => False);
      end if;
      declare
         Lower_Domain : constant String :=
            To_Lower (To_String (Result.Parts.Domain));
      begin
         return (Valid => True,
                 Value => Result.Parts.Local_Part &
                          To_Unbounded_String ("@") &
                          To_Unbounded_String (Lower_Domain));
      end;
   end Normalize;

end Proven.Safe_Email;
