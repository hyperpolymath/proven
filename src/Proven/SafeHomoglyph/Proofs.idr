-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeHomoglyph script-classification surface.
module Proven.SafeHomoglyph.Proofs

import Proven.SafeHomoglyph

%default total

-- Script Show anchors (11 constructors)
public export
latinShow : show Latin = "Latin"
latinShow = Refl

public export
cyrillicShow : show Cyrillic = "Cyrillic"
cyrillicShow = Refl

public export
greekShow : show Greek = "Greek"
greekShow = Refl

public export
armenianShow : show Armenian = "Armenian"
armenianShow = Refl

public export
georgianShow : show Georgian = "Georgian"
georgianShow = Refl

public export
arabicShow : show Arabic = "Arabic"
arabicShow = Refl

public export
hebrewShow : show Hebrew = "Hebrew"
hebrewShow = Refl

public export
devanagariShow : show Devanagari = "Devanagari"
devanagariShow = Refl

public export
cjkShow : show CJK = "CJK"
cjkShow = Refl

public export
commonShow : show Common = "Common"
commonShow = Refl

public export
unknownShow : show Unknown = "Unknown"
unknownShow = Refl

-- Script self-equality
public export
latinSelfEq : Latin == Latin = True
latinSelfEq = Refl

public export
cyrillicSelfEq : Cyrillic == Cyrillic = True
cyrillicSelfEq = Refl

-- HomoglyphResult projections
public export
mkResultMixed :
  (m : Bool) -> (ss : List Script) -> (cc, rl : Nat)
  -> (MkHomoglyphResult m ss cc rl).isMixedScript = m
mkResultMixed m ss cc rl = Refl

public export
mkResultScripts :
  (m : Bool) -> (ss : List Script) -> (cc, rl : Nat)
  -> (MkHomoglyphResult m ss cc rl).scripts = ss
mkResultScripts m ss cc rl = Refl
