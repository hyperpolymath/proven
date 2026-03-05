-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for proven-bfd.
||| Re-exports BFD.Types and provides protocol constants.
module BFD

import public BFD.Types

%default total

---------------------------------------------------------------------------
-- Protocol Constants (RFC 5880)
---------------------------------------------------------------------------

||| Default BFD control port.
public export
bfdPort : Nat
bfdPort = 3784

||| Default BFD echo port.
public export
bfdEchoPort : Nat
bfdEchoPort = 3785

||| Default desired minimum TX interval in microseconds.
public export
defaultDesiredMinTx : Nat
defaultDesiredMinTx = 1000000

||| Default required minimum RX interval in microseconds.
public export
defaultRequiredMinRx : Nat
defaultRequiredMinRx = 1000000

||| Default detection multiplier.
public export
defaultDetectMult : Nat
defaultDetectMult = 3
