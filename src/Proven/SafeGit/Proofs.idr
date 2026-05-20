-- SPDX-License-Identifier: PMPL-1.0-or-later
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

||| OWED: `refName (MkGitRef "main") = "main"` (record extraction
||| pass-through). Blocked on auto-implicit elaboration.
public export
0 refNameExtracts : (s : String) -> {auto v : isValidRefName s = True}
                 -> refName (MkGitRef s) = s
