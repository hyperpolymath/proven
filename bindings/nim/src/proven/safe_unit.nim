# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe physical unit conversions.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

proc convertLength*(value: float64, fromUnit, toUnit: LengthUnit): Option[float64] =
  ## Convert length between units.
  ## Returns None if the conversion fails.
  let res = provenUnitConvertLength(value, int32(ord(fromUnit)), int32(ord(toUnit)))
  if res.status == PROVEN_OK:
    return some(res.value)
  none(float64)

proc convertTemperature*(value: float64, fromUnit, toUnit: TempUnit): Option[float64] =
  ## Convert temperature between units.
  ## Returns None if the conversion fails.
  let res = provenUnitConvertTemp(value, int32(ord(fromUnit)), int32(ord(toUnit)))
  if res.status == PROVEN_OK:
    return some(res.value)
  none(float64)
