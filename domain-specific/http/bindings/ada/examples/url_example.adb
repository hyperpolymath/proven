-------------------------------------------------------------------------------
--  URL Encoding Example
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  Demonstrates URL encoding and decoding with Proven.Safe_HTTP.URL
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Proven.Safe_HTTP.URL;

procedure URL_Example is

   procedure Test_Encode (Input : String) is
      Encoded : constant String := Proven.Safe_HTTP.URL.URL_Encode (Input);
      Decoded : constant String := Proven.Safe_HTTP.URL.URL_Decode (Encoded);
   begin
      Put_Line ("Input:   " & Input);
      Put_Line ("Encoded: " & Encoded);
      Put_Line ("Decoded: " & Decoded);
      Put_Line ("Valid:   " & Boolean'Image (
        Proven.Safe_HTTP.URL.Is_Valid_Encoded (Encoded)));
      New_Line;
   end Test_Encode;

begin
   Put_Line ("=== Proven HTTP URL Encoding Examples ===");
   New_Line;

   Test_Encode ("hello world!");
   Test_Encode ("user@example.com");
   Test_Encode ("/path/to/resource?query=value");
   Test_Encode ("special: #?&=+%");

   Put_Line ("=== Unreserved Character Check ===");
   Put_Line ("'A' unreserved: " & Boolean'Image (
     Proven.Safe_HTTP.URL.Is_Unreserved ('A')));
   Put_Line ("'z' unreserved: " & Boolean'Image (
     Proven.Safe_HTTP.URL.Is_Unreserved ('z')));
   Put_Line ("'0' unreserved: " & Boolean'Image (
     Proven.Safe_HTTP.URL.Is_Unreserved ('0')));
   Put_Line ("'-' unreserved: " & Boolean'Image (
     Proven.Safe_HTTP.URL.Is_Unreserved ('-')));
   Put_Line ("' ' unreserved: " & Boolean'Image (
     Proven.Safe_HTTP.URL.Is_Unreserved (' ')));
   Put_Line ("'!' unreserved: " & Boolean'Image (
     Proven.Safe_HTTP.URL.Is_Unreserved ('!')));

end URL_Example;
