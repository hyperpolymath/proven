-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeDocker container-image reference parsing.
|||
||| Discharges constructor-projection theorems for Registry / Repository
||| / ImageTag / Digest / ImageRef, plus Show wire-format anchors.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeDocker.Proofs

import Proven.SafeDocker

%default total

public export
mkRegistryShow : (h : String) -> show (MkRegistry h) = h
mkRegistryShow h = Refl

public export
mkRepositoryShow : (n : String) -> show (MkRepository n) = n
mkRepositoryShow n = Refl

public export
mkImageTagShow : (t : String) -> show (MkImageTag t) = t
mkImageTagShow t = Refl

public export
mkDigestShow :
  (alg, hex : String) -> show (MkDigest alg hex) = alg ++ ":" ++ hex
mkDigestShow alg hex = Refl

public export
mkImageRefRegistry :
  (reg : Maybe Registry) -> (repo : Repository)
  -> (tag : Maybe ImageTag) -> (dig : Maybe Digest)
  -> (MkImageRef reg repo tag dig).registry = reg
mkImageRefRegistry reg repo tag dig = Refl

public export
mkImageRefRepository :
  (reg : Maybe Registry) -> (repo : Repository)
  -> (tag : Maybe ImageTag) -> (dig : Maybe Digest)
  -> (MkImageRef reg repo tag dig).repository = repo
mkImageRefRepository reg repo tag dig = Refl

public export
mkImageRefTag :
  (reg : Maybe Registry) -> (repo : Repository)
  -> (tag : Maybe ImageTag) -> (dig : Maybe Digest)
  -> (MkImageRef reg repo tag dig).tag = tag
mkImageRefTag reg repo tag dig = Refl

public export
mkImageRefDigest :
  (reg : Maybe Registry) -> (repo : Repository)
  -> (tag : Maybe ImageTag) -> (dig : Maybe Digest)
  -> (MkImageRef reg repo tag dig).digest = dig
mkImageRefDigest reg repo tag dig = Refl
