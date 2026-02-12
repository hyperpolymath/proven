--  SPDX-License-Identifier: Apache-2.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe string operations for security-sensitive contexts.

package Proven.Safe_String is

   --  Escape HTML special characters to prevent XSS attacks.
   function Escape_Html (Value : String) return String;

   --  Escape single quotes for SQL strings.
   function Escape_Sql (Value : String) return String;

   --  Escape JavaScript special characters.
   function Escape_Js (Value : String) return String;

   --  URL-encode a string.
   function Escape_Url (Value : String) return String;

   --  Safely truncate a string with a suffix.
   function Truncate_Safe
     (Value   : String;
      Max_Len : Positive;
      Suffix  : String := "...") return String;

end Proven.Safe_String;
