-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeI18n internationalisation surface.
module Proven.SafeI18n.Proofs

import Proven.SafeI18n

%default total

public export
ltrSelfEq : LTR == LTR = True
ltrSelfEq = Refl

public export
rtlSelfEq : RTL == RTL = True
rtlSelfEq = Refl

public export
ltrNotRtl : LTR == RTL = False
ltrNotRtl = Refl

public export
mkLanguageTagLang :
  (l : String) -> (r, s : Maybe String)
  -> (MkLanguageTag l r s).language = l
mkLanguageTagLang l r s = Refl

public export
mkLanguageTagRegion :
  (l : String) -> (r, s : Maybe String)
  -> (MkLanguageTag l r s).region = r
mkLanguageTagRegion l r s = Refl

public export
mkLanguageTagScript :
  (l : String) -> (r, s : Maybe String)
  -> (MkLanguageTag l r s).script = s
mkLanguageTagScript l r s = Refl
