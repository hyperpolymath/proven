--  SPDX-License-Identifier: PMPL-1.0-or-later
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe JSON parsing and generation with validation.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Proven.Safe_Json is

   --  JSON value types.
   type JSON_Kind is (
      JSON_Null,
      JSON_Boolean,
      JSON_Number,
      JSON_String,
      JSON_Array,
      JSON_Object
   );

   --  Maximum depth for parsing (prevents stack overflow attacks).
   Max_Depth : constant := 32;

   --  Maximum string length.
   Max_String_Length : constant := 65536;

   --  Validation result.
   type Validation_Result is record
      Valid   : Boolean;
      Message : Unbounded_String;
      Line    : Natural;
      Column  : Natural;
   end record;

   --  Validate JSON string syntax.
   function Validate (JSON : String) return Validation_Result;

   --  Check if JSON string is valid.
   function Is_Valid (JSON : String) return Boolean;

   --  Escape a string for JSON output.
   function Escape_String (Value : String) return String;

   --  Unescape a JSON string value.
   type Unescape_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Unbounded_String;
         when False => null;
      end case;
   end record;

   function Unescape_String (Value : String) return Unescape_Result;

   --  Format a JSON string with indentation.
   function Pretty_Print
     (JSON : String; Indent : Natural := 2) return Unbounded_String;

   --  Minify a JSON string (remove whitespace).
   function Minify (JSON : String) return Unbounded_String;

   --  Extract a string value at a JSON path (e.g., "$.foo.bar").
   type String_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Unbounded_String;
         when False => null;
      end case;
   end record;

   function Get_String (JSON : String; Path : String) return String_Result;

   --  Extract a number value at a JSON path.
   type Number_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Long_Float;
         when False => null;
      end case;
   end record;

   function Get_Number (JSON : String; Path : String) return Number_Result;

   --  Extract a boolean value at a JSON path.
   type Boolean_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Boolean;
         when False => null;
      end case;
   end record;

   function Get_Boolean (JSON : String; Path : String) return Boolean_Result;

   --  Check if a path exists in the JSON.
   function Has_Path (JSON : String; Path : String) return Boolean;

   --  Get the type of value at a path.
   type Kind_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Kind : JSON_Kind;
         when False => null;
      end case;
   end record;

   function Get_Kind (JSON : String; Path : String) return Kind_Result;

   --  Exception for JSON operations.
   JSON_Error : exception;

end Proven.Safe_Json;
