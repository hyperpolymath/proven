--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe email validation -- thin FFI wrapper over libproven.
--  Email validation performed by the formally verified Idris2/Zig core.

package Proven.Safe_Email is

   type Safe_Bool_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : Boolean;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Check if an email address is valid (RFC 5321)
   --  (calls proven_email_is_valid).
   function Is_Valid (Email : String) return Safe_Bool_Result;

end Proven.Safe_Email;
