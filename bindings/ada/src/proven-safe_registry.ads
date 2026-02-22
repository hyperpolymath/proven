--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe OCI registry reference parsing -- thin FFI wrapper over libproven.
--  Registry parsing done by the Idris2/Zig core.

package Proven.Safe_Registry is

   Max_Component_Len : constant := 512;

   type Image_Reference is record
      Registry       : String (1 .. Max_Component_Len);
      Registry_Last  : Natural;
      Repository     : String (1 .. Max_Component_Len);
      Repository_Last : Natural;
      Tag            : String (1 .. Max_Component_Len);
      Tag_Last       : Natural;
      Digest         : String (1 .. Max_Component_Len);
      Digest_Last    : Natural;
   end record;

   type Parse_Result (Success : Boolean := False) is record
      case Success is
         when True  => Reference  : Image_Reference;
         when False => Error_Code : Integer;
      end case;
   end record;

   Default_Registry : constant String := "docker.io";
   Default_Tag      : constant String := "latest";

   --  Parse OCI image reference string (calls proven_registry_parse).
   function Parse (Reference : String) return Parse_Result;

end Proven.Safe_Registry;
