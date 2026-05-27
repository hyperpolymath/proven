-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeCurrency money / ISO-4217 currency surface.
|||
||| Discharges enum self-equality + Show wire-format anchors for the
||| 11 well-known currency codes, plus minor-unit precision anchors
||| (JPY=0, BTC=8, ETH=18). Other currencies = 2 (default cents).
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeCurrency.Proofs

import Proven.SafeCurrency

%default total

--------------------------------------------------------------------------------
-- CurrencyCode self-equality (11 named + 1 Other)
--------------------------------------------------------------------------------

public export
usdSelfEq : USD == USD = True
usdSelfEq = Refl

public export
eurSelfEq : EUR == EUR = True
eurSelfEq = Refl

public export
gbpSelfEq : GBP == GBP = True
gbpSelfEq = Refl

public export
jpySelfEq : JPY == JPY = True
jpySelfEq = Refl

public export
chfSelfEq : CHF == CHF = True
chfSelfEq = Refl

public export
cadSelfEq : CAD == CAD = True
cadSelfEq = Refl

public export
audSelfEq : AUD == AUD = True
audSelfEq = Refl

public export
cnySelfEq : CNY == CNY = True
cnySelfEq = Refl

public export
inrSelfEq : INR == INR = True
inrSelfEq = Refl

public export
btcSelfEq : BTC == BTC = True
btcSelfEq = Refl

public export
ethSelfEq : ETH == ETH = True
ethSelfEq = Refl

public export
usdNotEur : USD == EUR = False
usdNotEur = Refl

--------------------------------------------------------------------------------
-- CurrencyCode Show anchors (ISO-4217 alpha codes)
--------------------------------------------------------------------------------

public export
usdShow : show USD = "USD"
usdShow = Refl

public export
eurShow : show EUR = "EUR"
eurShow = Refl

public export
gbpShow : show GBP = "GBP"
gbpShow = Refl

public export
jpyShow : show JPY = "JPY"
jpyShow = Refl

public export
chfShow : show CHF = "CHF"
chfShow = Refl

public export
cadShow : show CAD = "CAD"
cadShow = Refl

public export
audShow : show AUD = "AUD"
audShow = Refl

public export
cnyShow : show CNY = "CNY"
cnyShow = Refl

public export
inrShow : show INR = "INR"
inrShow = Refl

public export
btcShow : show BTC = "BTC"
btcShow = Refl

public export
ethShow : show ETH = "ETH"
ethShow = Refl

--------------------------------------------------------------------------------
-- minorUnits precision anchors
--------------------------------------------------------------------------------

||| JPY has no minor unit (whole yen only).
public export
jpyZeroMinor : minorUnits JPY = 0
jpyZeroMinor = Refl

||| BTC uses 8 decimal places (satoshis).
public export
btcEightMinor : minorUnits BTC = 8
btcEightMinor = Refl

||| ETH uses 18 decimal places (wei).
public export
ethEighteenMinor : minorUnits ETH = 18
ethEighteenMinor = Refl

||| USD uses 2 decimal places (cents).
public export
usdTwoMinor : minorUnits USD = 2
usdTwoMinor = Refl

||| EUR uses 2 decimal places.
public export
eurTwoMinor : minorUnits EUR = 2
eurTwoMinor = Refl
