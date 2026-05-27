-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeMediaType IANA media-type surface.
module Proven.SafeMediaType.Proofs

import Proven.SafeMediaType

%default total

public export
applicationShow : show Application = "application"
applicationShow = Refl

public export
audioShow : show Audio = "audio"
audioShow = Refl

public export
fontShow : show Font = "font"
fontShow = Refl

public export
exampleShow : show Example = "example"
exampleShow = Refl

public export
imageShow : show Image = "image"
imageShow = Refl

public export
messageShow : show Message = "message"
messageShow = Refl

public export
modelShow : show Model = "model"
modelShow = Refl

public export
applicationSelfEq : Application == Application = True
applicationSelfEq = Refl

public export
audioSelfEq : Audio == Audio = True
audioSelfEq = Refl

public export
applicationNotAudio : Application == Audio = False
applicationNotAudio = Refl
