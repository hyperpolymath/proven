--  SPDX-License-Identifier: PMPL-1.0-or-later
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe OCI registry reference parsing and validation.
--  Formally verified via Idris2 Proven.SafeRegistry module.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Proven.Safe_Registry is

   --  Parsed OCI image reference
   type Image_Reference is record
      Registry   : Unbounded_String;  -- e.g., "ghcr.io", "docker.io"
      Repository : Unbounded_String;  -- e.g., "user/repo", "library/nginx"
      Tag        : Unbounded_String;  -- e.g., "latest", "v1.0"
      Digest     : Unbounded_String;  -- e.g., "sha256:abc..."
   end record;

   --  Parse result discriminated record
   type Parse_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Reference : Image_Reference;
         when False => null;
      end case;
   end record;

   --  Parse OCI image reference string
   --  Format: [registry/]repository[:tag][@digest]
   --
   --  Examples:
   --    "nginx" -> docker.io/library/nginx:latest
   --    "nginx:1.25" -> docker.io/library/nginx:1.25
   --    "ghcr.io/user/repo:v1.0" -> as-is
   --    "ghcr.io/user/repo@sha256:abc..." -> with digest
   function Parse (Reference : String) return Parse_Result;

   --  Convert image reference back to string
   function To_String (Ref : Image_Reference) return String;

   --  Convert to canonical form (all defaults explicit)
   function To_Canonical (Ref : Image_Reference) return String;

   --  Check if reference has explicit registry
   function Has_Registry (Ref : Image_Reference) return Boolean;

   --  Check if reference has explicit tag
   function Has_Tag (Ref : Image_Reference) return Boolean;

   --  Check if reference has digest
   function Has_Digest (Ref : Image_Reference) return Boolean;

   --  Extract registry (with default if not specified)
   function Get_Registry (Ref : Image_Reference) return String;

   --  Extract repository
   function Get_Repository (Ref : Image_Reference) return String;

   --  Extract tag (with default if not specified)
   function Get_Tag (Ref : Image_Reference) return String;

   --  Extract digest (empty if not specified)
   function Get_Digest (Ref : Image_Reference) return String;

   --  Default registry (Docker Hub)
   Default_Registry : constant String := "docker.io";

   --  Default tag
   Default_Tag : constant String := "latest";

   --  Exception for parse errors
   Registry_Parse_Error : exception;

end Proven.Safe_Registry;
