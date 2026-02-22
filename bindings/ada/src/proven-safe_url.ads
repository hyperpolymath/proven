--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe URL parsing -- thin FFI wrapper over libproven.
--  URL parsing performed by the formally verified Idris2/Zig core.

package Proven.Safe_Url is

   Max_Component_Len : constant := 2048;

   type URL_Components is record
      Scheme   : String (1 .. Max_Component_Len);
      Scheme_Last : Natural;
      Host     : String (1 .. Max_Component_Len);
      Host_Last : Natural;
      Port     : Natural;
      Has_Port : Boolean;
      Path     : String (1 .. Max_Component_Len);
      Path_Last : Natural;
      Query    : String (1 .. Max_Component_Len);
      Query_Last : Natural;
      Fragment : String (1 .. Max_Component_Len);
      Fragment_Last : Natural;
   end record;

   type Parse_Result (Success : Boolean := False) is record
      case Success is
         when True  => Components : URL_Components;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Parse a URL into its components (calls proven_url_parse).
   function Parse (URL : String) return Parse_Result;

end Proven.Safe_Url;
