--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Proven.Safe_Math;
with Proven.Safe_String;
with Proven.Safe_Path;
with Proven.Safe_Email;
with Proven.Safe_Network;
with Proven.Safe_Crypto;

procedure Test_Proven is

   procedure Assert (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         Put_Line ("FAIL: " & Message);
      else
         Put_Line ("PASS: " & Message);
      end if;
   end Assert;

   use Proven.Safe_Math;
   use Proven.Safe_String;
   use Proven.Safe_Path;
   use Proven.Safe_Email;
   use Proven.Safe_Network;
   use Proven.Safe_Crypto;

begin
   Put_Line ("=== Safe_Math Tests ===");

   declare
      Result : Optional_Int64;
   begin
      Result := Safe_Div (10, 2);
      Assert (Result.Valid and then Result.Value = 5, "10/2=5");

      Result := Safe_Div (10, 0);
      Assert (not Result.Valid, "10/0=invalid");

      Result := Safe_Mod (10, 3);
      Assert (Result.Valid and then Result.Value = 1, "10%3=1");

      Result := Safe_Add (1, 2);
      Assert (Result.Valid and then Result.Value = 3, "1+2=3");

      Result := Safe_Sub (5, 3);
      Assert (Result.Valid and then Result.Value = 2, "5-3=2");

      Result := Safe_Mul (3, 4);
      Assert (Result.Valid and then Result.Value = 12, "3*4=12");
   end;

   Put_Line ("");
   Put_Line ("=== Safe_String Tests ===");

   Assert (Escape_Html ("<script>") = "&lt;script&gt;", "escape HTML <>");
   Assert (Escape_Html ("a & b") = "a &amp; b", "escape HTML &");
   Assert (Escape_Sql ("it's") = "it''s", "escape SQL '");
   Assert (Truncate_Safe ("hello world", 5) = "he...", "truncate with ...");
   Assert (Truncate_Safe ("hi", 10) = "hi", "no truncate needed");

   Put_Line ("");
   Put_Line ("=== Safe_Path Tests ===");

   Assert (Has_Traversal ("../etc") = True, "detect ..");
   Assert (Has_Traversal ("~/file") = True, "detect ~");
   Assert (Has_Traversal ("normal/path") = False, "safe path");
   Assert (Is_Safe ("safe/path") = True, "is_safe true");
   Assert (Is_Safe ("../unsafe") = False, "is_safe false");

   Put_Line ("");
   Put_Line ("=== Safe_Email Tests ===");

   Assert (Is_Valid ("user@example.com") = True, "valid email");
   Assert (Is_Valid ("not-an-email") = False, "invalid email");
   Assert (Is_Valid ("@invalid.com") = False, "missing local part");

   declare
      Norm_Result : constant Proven.Safe_Email.Optional_String :=
                    Normalize ("User@EXAMPLE.COM");
   begin
      Assert (Norm_Result.Valid and then
              To_String (Norm_Result.Value) = "User@example.com",
              "normalize email");
   end;

   Put_Line ("");
   Put_Line ("=== Safe_Network Tests ===");

   Assert (Is_Valid_IPv4 ("192.168.1.1") = True, "valid IPv4");
   Assert (Is_Valid_IPv4 ("256.1.1.1") = False, "invalid IPv4");
   Assert (Is_Private ("192.168.1.1") = True, "192.168 is private");
   Assert (Is_Private ("10.0.0.1") = True, "10.x is private");
   Assert (Is_Private ("8.8.8.8") = False, "8.8.8.8 is public");
   Assert (Is_Loopback ("127.0.0.1") = True, "127 is loopback");
   Assert (Is_Public ("8.8.8.8") = True, "8.8.8.8 is public");

   Put_Line ("");
   Put_Line ("=== Safe_Crypto Tests ===");

   Assert (Constant_Time_Compare ("secret", "secret") = True, "equal strings");
   Assert (Constant_Time_Compare ("secret", "other!") = False, "diff strings");
   Assert (Constant_Time_Compare ("", "") = True, "empty strings");

   declare
      Zeroed : constant String := Secure_Zero (4);
   begin
      Assert (Zeroed'Length = 4, "secure_zero length");
      Assert (Zeroed = (1 .. 4 => ASCII.NUL), "secure_zero content");
   end;

   Put_Line ("");
   Put_Line ("=== All Tests Complete ===");

end Test_Proven;
