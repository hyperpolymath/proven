--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Proven.Safe_Url is

   type Byte_Array is array (Natural range <>) of aliased unsigned_char;

   function To_Bytes (S : String) return Byte_Array is
      Result : Byte_Array (0 .. S'Length - 1);
   begin
      for I in S'Range loop
         Result (I - S'First) := unsigned_char (Character'Pos (S (I)));
      end loop;
      return Result;
   end To_Bytes;

   function Safe_Value (Ptr : chars_ptr; Len : size_t) return String is
   begin
      if Ptr = Null_Ptr or else Len = 0 then
         return "";
      end if;
      return Value (Ptr, Len);
   end Safe_Value;

   function Parse (URL : String) return Parse_Result is
      Bytes  : aliased Byte_Array := To_Bytes (URL);
      Result : FFI.Url_Result;
   begin
      if URL'Length = 0 then
         return (Success => False,
                 Error_Code => Integer (FFI.PROVEN_ERR_INVALID_ARGUMENT));
      end if;
      Result := FFI.Url_Parse (Bytes (Bytes'First)'Access, Bytes'Length);
      if Result.Status /= FFI.PROVEN_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      declare
         Comps  : URL_Components;
         Scheme : constant String :=
            Safe_Value (Result.Components.Scheme, Result.Components.Scheme_Len);
         Host   : constant String :=
            Safe_Value (Result.Components.Host, Result.Components.Host_Len);
         Path   : constant String :=
            Safe_Value (Result.Components.Path, Result.Components.Path_Len);
         Query  : constant String :=
            Safe_Value (Result.Components.Query, Result.Components.Query_Len);
         Frag   : constant String :=
            Safe_Value (Result.Components.Fragment,
                        Result.Components.Fragment_Len);
      begin
         Comps.Scheme := (others => ' ');
         Comps.Scheme_Last := Natural'Min (Scheme'Length, Max_Component_Len);
         Comps.Scheme (1 .. Comps.Scheme_Last) :=
            Scheme (Scheme'First .. Scheme'First + Comps.Scheme_Last - 1);

         Comps.Host := (others => ' ');
         Comps.Host_Last := Natural'Min (Host'Length, Max_Component_Len);
         Comps.Host (1 .. Comps.Host_Last) :=
            Host (Host'First .. Host'First + Comps.Host_Last - 1);

         Comps.Port := Natural (Result.Components.Port);
         Comps.Has_Port := Boolean (Result.Components.Has_Port);

         Comps.Path := (others => ' ');
         Comps.Path_Last := Natural'Min (Path'Length, Max_Component_Len);
         Comps.Path (1 .. Comps.Path_Last) :=
            Path (Path'First .. Path'First + Comps.Path_Last - 1);

         Comps.Query := (others => ' ');
         Comps.Query_Last := Natural'Min (Query'Length, Max_Component_Len);
         Comps.Query (1 .. Comps.Query_Last) :=
            Query (Query'First .. Query'First + Comps.Query_Last - 1);

         Comps.Fragment := (others => ' ');
         Comps.Fragment_Last := Natural'Min (Frag'Length, Max_Component_Len);
         Comps.Fragment (1 .. Comps.Fragment_Last) :=
            Frag (Frag'First .. Frag'First + Comps.Fragment_Last - 1);

         --  Free C-allocated URL components
         declare
            Free_Comps : aliased FFI.Url_Components := Result.Components;
         begin
            FFI.Proven_Url_Free (Free_Comps'Access);
         end;

         return (Success => True, Components => Comps);
      end;
   end Parse;

end Proven.Safe_Url;
