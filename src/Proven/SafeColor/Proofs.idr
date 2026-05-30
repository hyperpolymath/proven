-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeColor RGB/RGBA/HSL color manipulation.
|||
||| `Proven.SafeColor` ships record-projection RGB/RGBA/HSL types and
||| conversions. This file machine-checks the structural anchors:
|||
|||   * `MkRGB` records its R/G/B values exactly.
|||   * `MkRGBA` records its R/G/B/A values exactly.
|||   * `MkHSL` records its hue/saturation/lightness exactly.
|||   * `parseHex ""` returns Nothing (empty input rejected).
|||
||| OWED: Double comparisons in WCAG luminance / contrast ratio,
||| `unpack`-based parsing in `parseHex`.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeColor.Proofs

import Proven.SafeColor

%default total

--------------------------------------------------------------------------------
-- `MkRGB` record projections
--------------------------------------------------------------------------------

public export
mkRGBRed : (r, g, b : Nat) -> (MkRGB r g b).red = r
mkRGBRed r g b = Refl

public export
mkRGBGreen : (r, g, b : Nat) -> (MkRGB r g b).green = g
mkRGBGreen r g b = Refl

public export
mkRGBBlue : (r, g, b : Nat) -> (MkRGB r g b).blue = b
mkRGBBlue r g b = Refl

--------------------------------------------------------------------------------
-- `MkRGBA` record projections
--------------------------------------------------------------------------------

public export
mkRGBARed : (r, g, b, a : Nat) -> (MkRGBA r g b a).red = r
mkRGBARed r g b a = Refl

public export
mkRGBAGreen : (r, g, b, a : Nat) -> (MkRGBA r g b a).green = g
mkRGBAGreen r g b a = Refl

public export
mkRGBABlue : (r, g, b, a : Nat) -> (MkRGBA r g b a).blue = b
mkRGBABlue r g b a = Refl

public export
mkRGBAAlpha : (r, g, b, a : Nat) -> (MkRGBA r g b a).alpha = a
mkRGBAAlpha r g b a = Refl

--------------------------------------------------------------------------------
-- `MkHSL` record projections
--------------------------------------------------------------------------------

public export
mkHSLHue : (h, s, l : Double) -> (MkHSL h s l).hue = h
mkHSLHue h s l = Refl

public export
mkHSLSaturation : (h, s, l : Double) -> (MkHSL h s l).saturation = s
mkHSLSaturation h s l = Refl

public export
mkHSLLightness : (h, s, l : Double) -> (MkHSL h s l).lightness = l
mkHSLLightness h s l = Refl

--------------------------------------------------------------------------------
-- `parseHex` boundary
--------------------------------------------------------------------------------

||| Empty string rejected — empty hex has no R/G/B components.
public export
parseHexEmptyRejected : parseHex "" = Nothing
parseHexEmptyRejected = Refl

--------------------------------------------------------------------------------
-- OWED markers
--------------------------------------------------------------------------------

||| DISCHARGED: WCAG-AA threshold = 4.5. `meetsWCAG_AA` is `public
||| export` with body `contrastRatio c1 c2 >= 4.5` (SafeColor.idr
||| L188-189), so the LHS unfolds directly to the RHS for `Refl`. No
||| Double-comparison reduction is needed — both sides are literally
||| the same expression.
public export
wcagAAThreshold :
  (c1, c2 : RGB) -> meetsWCAG_AA c1 c2 = (contrastRatio c1 c2 >= 4.5)
wcagAAThreshold _ _ = Refl

||| DISCHARGED: WCAG-AAA threshold = 7.0. `meetsWCAG_AAA` is `public
||| export` with body `contrastRatio c1 c2 >= 7.0` (SafeColor.idr
||| L193-194) — same unfolding pattern as `wcagAAThreshold`.
public export
wcagAAAThreshold :
  (c1, c2 : RGB) -> meetsWCAG_AAA c1 c2 = (contrastRatio c1 c2 >= 7.0)
wcagAAAThreshold _ _ = Refl
