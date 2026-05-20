-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeInput keystroke / character-class surface.
|||
||| `CharClass` has no `Eq` instance (the `Custom` constructor carries
||| a function, which would block decidable equality). All Proofs here
||| are OWED until a downstream Eq is provided.
module Proven.SafeInput.Proofs

import Proven.SafeInput

%default total

||| OWED: characterise Alphanumeric / Numeric / Printable etc. once
||| an `Eq CharClass` constrained to the nullary constructors is
||| provided.
public export
0 safeInputProofsAwaitEqCharClass : ()
