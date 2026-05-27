-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeHex hex-encoding surface.
|||
||| OWED: hexToNibble uses Char comparisons via ord/`>=` which go
||| through opaque Char/String FFI.
module Proven.SafeHex.Proofs

import Proven.SafeHex

%default total

||| OWED: invalid character rejected (e.g. 'g' is not a hex digit).
||| Blocked on Char FFI (`ord`, `>=`).
public export
0 nonHexCharRejected : hexToNibble 'g' = Nothing
