--  SPDX-License-Identifier: Apache-2.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe URL parsing and validation operations.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Proven.Safe_Url is

   --  URL components record.
   type URL_Components is record
      Scheme   : Unbounded_String;
      Host     : Unbounded_String;
      Port     : Natural;
      Path     : Unbounded_String;
      Query    : Unbounded_String;
      Fragment : Unbounded_String;
   end record;

   --  Parse result discriminated record.
   type Parse_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Components : URL_Components;
         when False => null;
      end case;
   end record;

   type Optional_String (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Unbounded_String;
         when False => null;
      end case;
   end record;

   --  Parse a URL into its components.
   function Parse (URL : String) return Parse_Result;

   --  Check if a URL is valid.
   function Is_Valid (URL : String) return Boolean;

   --  Get the scheme (protocol) from a URL.
   function Get_Scheme (URL : String) return Optional_String;

   --  Get the host from a URL.
   function Get_Host (URL : String) return Optional_String;

   --  Get the port from a URL (0 if not specified).
   function Get_Port (URL : String) return Natural;

   --  Get the path from a URL.
   function Get_Path (URL : String) return Optional_String;

   --  Get the query string from a URL.
   function Get_Query (URL : String) return Optional_String;

   --  Get the fragment from a URL.
   function Get_Fragment (URL : String) return Optional_String;

   --  Encode a string for use in a URL.
   function Encode (Value : String) return String;

   --  Decode a URL-encoded string.
   function Decode (Value : String) return Optional_String;

   --  Check if a URL is using HTTPS.
   function Is_Https (URL : String) return Boolean;

   --  Check if a URL is using a secure protocol.
   function Is_Secure (URL : String) return Boolean;

   --  Normalize a URL (lowercase scheme and host).
   function Normalize (URL : String) return Optional_String;

   --  Join a base URL with a relative path.
   function Join (Base : String; Relative : String) return Optional_String;

   --  Exception for URL operations.
   URL_Error : exception;

end Proven.Safe_Url;
