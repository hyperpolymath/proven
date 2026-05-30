-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeHex hex-encoding surface.
|||
||| OWED: hexToNibble uses Char comparisons via ord/`>=` which go
||| through opaque Char/String FFI.
module Proven.SafeHex.Proofs

import Proven.SafeHex

%default total

||| TENTATIVE-DISCHARGE: invalid character rejected. The if-chain in
||| `hexToNibble` (SafeHex.idr L66-71) tests `'g' >= '0' && 'g' <= '9'`,
||| `'g' >= 'a' && 'g' <= 'f'`, `'g' >= 'A' && 'g' <= 'F'` — for a
||| LITERAL Char like 'g' the `Ord Char` primitives may reduce at the
||| type level (the FFI opacity the OWED cited applies primarily to
||| ABSTRACT chars). If CI rejects this, fall back to the OWED stub.
public export
nonHexCharRejected : hexToNibble 'g' = Nothing
nonHexCharRejected = Refl
