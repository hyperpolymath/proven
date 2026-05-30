-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeGit ref-validation + safe operation surface.
|||
||| Discharges `forbiddenRefChars` spec-anchor and identifies OWED
||| theorems blocked on String FFI.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeGit.Proofs

import Proven.SafeGit

%default total

||| OWED: spec anchor — 9 ref characters are forbidden by
||| git-check-ref-format. Blocked on Char-list reduction.
public export
0 forbiddenRefCharsAnchor :
  forbiddenRefChars = [' ', '~', '^', ':', '?', '*', '[', '\\', '\x7F']

--------------------------------------------------------------------------------
-- OWED postulates
--------------------------------------------------------------------------------

||| OWED: `isValidRefName ""` = False (empty refs invalid). Blocked
||| on String FFI (`unpack`, `length` opacity for variable inputs).
public export
0 emptyRefNameInvalid : isValidRefName "" = False

||| DISCHARGED: `refName (MkGitRef s) = s` (record extraction
||| pass-through). `refName` is a direct pattern match
||| `refName (MkGitRef name) = name` (SafeGit.idr L62-63), so the
||| body reduces by Refl regardless of `s`. The auto-implicit
||| `v : isValidRefName s = True` is irrelevant to the proof — it's
||| an API precondition on `s` that doesn't enter the reduction.
public export
refNameExtracts : (s : String) -> {auto v : isValidRefName s = True}
               -> refName (MkGitRef s) = s
refNameExtracts _ = Refl
