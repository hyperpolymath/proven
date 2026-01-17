--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

package body Proven.Safe_Version is

   function Make
     (Major, Minor, Patch : Natural;
      Prerelease          : String := "";
      Build               : String := "") return Version
   is
   begin
      return (Major      => Major,
              Minor      => Minor,
              Patch      => Patch,
              Prerelease => To_Unbounded_String (Prerelease),
              Build      => To_Unbounded_String (Build));
   end Make;

   function Parse_Nat (S : String; First, Last : Natural) return Integer is
      Result : Natural := 0;
   begin
      if First > Last then
         return -1;
      end if;

      --  No leading zeros allowed (except for zero itself)
      if Last > First and then S (First) = '0' then
         return -1;
      end if;

      for I in First .. Last loop
         if S (I) not in '0' .. '9' then
            return -1;
         end if;
         Result := Result * 10 + (Character'Pos (S (I)) - Character'Pos ('0'));
      end loop;

      return Result;
   end Parse_Nat;

   function Parse (S : String) return Version_Result is
      Major_End   : Natural := 0;
      Minor_Start : Natural := 0;
      Minor_End   : Natural := 0;
      Patch_Start : Natural := 0;
      Patch_End   : Natural := 0;
      Pre_Start   : Natural := 0;
      Build_Start : Natural := 0;
      Major_Val   : Integer;
      Minor_Val   : Integer;
      Patch_Val   : Integer;
   begin
      if S'Length = 0 then
         return (Valid => False);
      end if;

      --  Find first dot (end of major)
      for I in S'Range loop
         if S (I) = '.' then
            Major_End := I - 1;
            Minor_Start := I + 1;
            exit;
         end if;
      end loop;

      if Major_End = 0 or else Minor_Start > S'Last then
         return (Valid => False);
      end if;

      --  Find second dot (end of minor)
      for I in Minor_Start .. S'Last loop
         if S (I) = '.' then
            Minor_End := I - 1;
            Patch_Start := I + 1;
            exit;
         end if;
      end loop;

      if Minor_End = 0 or else Patch_Start > S'Last then
         return (Valid => False);
      end if;

      --  Find end of patch (may end at hyphen, plus, or end of string)
      Patch_End := S'Last;
      for I in Patch_Start .. S'Last loop
         if S (I) = '-' then
            Patch_End := I - 1;
            Pre_Start := I + 1;
            exit;
         elsif S (I) = '+' then
            Patch_End := I - 1;
            Build_Start := I + 1;
            exit;
         end if;
      end loop;

      --  Check for build metadata after prerelease
      if Pre_Start > 0 then
         for I in Pre_Start .. S'Last loop
            if S (I) = '+' then
               Build_Start := I + 1;
               exit;
            end if;
         end loop;
      end if;

      --  Parse numeric components
      Major_Val := Parse_Nat (S, S'First, Major_End);
      Minor_Val := Parse_Nat (S, Minor_Start, Minor_End);
      Patch_Val := Parse_Nat (S, Patch_Start, Patch_End);

      if Major_Val < 0 or else Minor_Val < 0 or else Patch_Val < 0 then
         return (Valid => False);
      end if;

      declare
         Result : Version;
      begin
         Result.Major := Major_Val;
         Result.Minor := Minor_Val;
         Result.Patch := Patch_Val;

         if Pre_Start > 0 then
            declare
               Pre_End : Natural := S'Last;
            begin
               if Build_Start > 0 then
                  Pre_End := Build_Start - 2;
               end if;
               if Pre_Start <= Pre_End then
                  Result.Prerelease := To_Unbounded_String (S (Pre_Start .. Pre_End));
               end if;
            end;
         else
            Result.Prerelease := Null_Unbounded_String;
         end if;

         if Build_Start > 0 and then Build_Start <= S'Last then
            Result.Build := To_Unbounded_String (S (Build_Start .. S'Last));
         else
            Result.Build := Null_Unbounded_String;
         end if;

         return (Valid => True, Value => Result);
      end;
   end Parse;

   function Parse_Or_Raise (S : String) return Version is
      Result : constant Version_Result := Parse (S);
   begin
      if not Result.Valid then
         raise Version_Error with "Invalid version: " & S;
      end if;
      return Result.Value;
   end Parse_Or_Raise;

   function Is_Valid (S : String) return Boolean is
   begin
      return Parse (S).Valid;
   end Is_Valid;

   function Format (V : Version) return String is
      Result : Unbounded_String;
   begin
      Result := To_Unbounded_String (
         Natural'Image (V.Major)(2 .. Natural'Image (V.Major)'Last) & "." &
         Natural'Image (V.Minor)(2 .. Natural'Image (V.Minor)'Last) & "." &
         Natural'Image (V.Patch)(2 .. Natural'Image (V.Patch)'Last));

      if Length (V.Prerelease) > 0 then
         Append (Result, "-");
         Append (Result, V.Prerelease);
      end if;

      if Length (V.Build) > 0 then
         Append (Result, "+");
         Append (Result, V.Build);
      end if;

      return To_String (Result);
   end Format;

   function Compare_Prerelease (A, B : Unbounded_String) return Integer is
      Str_A : constant String := To_String (A);
      Str_B : constant String := To_String (B);
   begin
      --  No prerelease has higher precedence
      if Str_A'Length = 0 and then Str_B'Length = 0 then
         return 0;
      elsif Str_A'Length = 0 then
         return 1;  --  A > B (no prerelease beats prerelease)
      elsif Str_B'Length = 0 then
         return -1;  --  A < B
      end if;

      --  Simple string comparison for prerelease identifiers
      if Str_A < Str_B then
         return -1;
      elsif Str_A > Str_B then
         return 1;
      else
         return 0;
      end if;
   end Compare_Prerelease;

   function Compare (A, B : Version) return Integer is
   begin
      --  Compare major
      if A.Major < B.Major then
         return -1;
      elsif A.Major > B.Major then
         return 1;
      end if;

      --  Compare minor
      if A.Minor < B.Minor then
         return -1;
      elsif A.Minor > B.Minor then
         return 1;
      end if;

      --  Compare patch
      if A.Patch < B.Patch then
         return -1;
      elsif A.Patch > B.Patch then
         return 1;
      end if;

      --  Compare prerelease
      return Compare_Prerelease (A.Prerelease, B.Prerelease);
   end Compare;

   function Equal (A, B : Version) return Boolean is
   begin
      return Compare (A, B) = 0;
   end Equal;

   function Less_Than (A, B : Version) return Boolean is
   begin
      return Compare (A, B) < 0;
   end Less_Than;

   function Greater_Than (A, B : Version) return Boolean is
   begin
      return Compare (A, B) > 0;
   end Greater_Than;

   function Less_Or_Equal (A, B : Version) return Boolean is
   begin
      return Compare (A, B) <= 0;
   end Less_Or_Equal;

   function Greater_Or_Equal (A, B : Version) return Boolean is
   begin
      return Compare (A, B) >= 0;
   end Greater_Or_Equal;

   function Is_Prerelease (V : Version) return Boolean is
   begin
      return Length (V.Prerelease) > 0;
   end Is_Prerelease;

   function Is_Stable (V : Version) return Boolean is
   begin
      return V.Major >= 1 and then Length (V.Prerelease) = 0;
   end Is_Stable;

   function Increment_Major (V : Version) return Version is
   begin
      return Make (V.Major + 1, 0, 0);
   end Increment_Major;

   function Increment_Minor (V : Version) return Version is
   begin
      return Make (V.Major, V.Minor + 1, 0);
   end Increment_Minor;

   function Increment_Patch (V : Version) return Version is
   begin
      return Make (V.Major, V.Minor, V.Patch + 1);
   end Increment_Patch;

   function Core_Version (V : Version) return Version is
   begin
      return Make (V.Major, V.Minor, V.Patch);
   end Core_Version;

   function Satisfies (V : Version; Range_Spec : String) return Boolean is
      Spec_Start : Natural := Range_Spec'First;
      Target     : Version_Result;
   begin
      if Range_Spec'Length = 0 then
         return True;  --  Empty range matches everything
      end if;

      --  Skip operator prefix
      case Range_Spec (Spec_Start) is
         when '^' =>
            --  Caret range: compatible with version
            Spec_Start := Spec_Start + 1;
            Target := Parse (Range_Spec (Spec_Start .. Range_Spec'Last));
            if not Target.Valid then
               return False;
            end if;
            --  Must be >= target and < next major (for major > 0)
            if V.Major /= Target.Value.Major then
               return False;
            end if;
            return Compare (V, Target.Value) >= 0;

         when '~' =>
            --  Tilde range: same major.minor
            Spec_Start := Spec_Start + 1;
            Target := Parse (Range_Spec (Spec_Start .. Range_Spec'Last));
            if not Target.Valid then
               return False;
            end if;
            return V.Major = Target.Value.Major and then
                   V.Minor = Target.Value.Minor and then
                   Compare (V, Target.Value) >= 0;

         when '>' =>
            if Range_Spec'Length > 1 and then
               Range_Spec (Spec_Start + 1) = '='
            then
               Spec_Start := Spec_Start + 2;
               Target := Parse (Range_Spec (Spec_Start .. Range_Spec'Last));
               if not Target.Valid then
                  return False;
               end if;
               return Greater_Or_Equal (V, Target.Value);
            else
               Spec_Start := Spec_Start + 1;
               Target := Parse (Range_Spec (Spec_Start .. Range_Spec'Last));
               if not Target.Valid then
                  return False;
               end if;
               return Greater_Than (V, Target.Value);
            end if;

         when '<' =>
            if Range_Spec'Length > 1 and then
               Range_Spec (Spec_Start + 1) = '='
            then
               Spec_Start := Spec_Start + 2;
               Target := Parse (Range_Spec (Spec_Start .. Range_Spec'Last));
               if not Target.Valid then
                  return False;
               end if;
               return Less_Or_Equal (V, Target.Value);
            else
               Spec_Start := Spec_Start + 1;
               Target := Parse (Range_Spec (Spec_Start .. Range_Spec'Last));
               if not Target.Valid then
                  return False;
               end if;
               return Less_Than (V, Target.Value);
            end if;

         when '=' =>
            Spec_Start := Spec_Start + 1;
            Target := Parse (Range_Spec (Spec_Start .. Range_Spec'Last));
            if not Target.Valid then
               return False;
            end if;
            return Equal (V, Target.Value);

         when others =>
            --  Exact match
            Target := Parse (Range_Spec);
            if not Target.Valid then
               return False;
            end if;
            return Equal (V, Target.Value);
      end case;
   end Satisfies;

end Proven.Safe_Version;
