-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeGeo geographic-coordinate surface.
|||
||| Discharges GeoCoord / GeoBBox record-projection theorems.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeGeo.Proofs

import Proven.SafeGeo

%default total

public export
mkGeoCoordLat : (lat, lon : Double) -> (MkGeoCoord lat lon).latitude = lat
mkGeoCoordLat lat lon = Refl

public export
mkGeoCoordLon : (lat, lon : Double) -> (MkGeoCoord lat lon).longitude = lon
mkGeoCoordLon lat lon = Refl

public export
mkGeoBBoxMinLat :
  (mla, mlo, xla, xlo : Double)
  -> (MkGeoBBox mla mlo xla xlo).minLat = mla
mkGeoBBoxMinLat mla mlo xla xlo = Refl

public export
mkGeoBBoxMinLon :
  (mla, mlo, xla, xlo : Double)
  -> (MkGeoBBox mla mlo xla xlo).minLon = mlo
mkGeoBBoxMinLon mla mlo xla xlo = Refl

public export
mkGeoBBoxMaxLat :
  (mla, mlo, xla, xlo : Double)
  -> (MkGeoBBox mla mlo xla xlo).maxLat = xla
mkGeoBBoxMaxLat mla mlo xla xlo = Refl

public export
mkGeoBBoxMaxLon :
  (mla, mlo, xla, xlo : Double)
  -> (MkGeoBBox mla mlo xla xlo).maxLon = xlo
mkGeoBBoxMaxLon mla mlo xla xlo = Refl
