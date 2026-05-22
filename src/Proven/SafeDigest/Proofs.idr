-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeDigest operations
|||
||| SafeDigest's module header claims "formally verified digest parsing".
||| Until now no theorem backed that claim (the four theorems in the
||| module body are body-less de-facto axioms). This module discharges
||| the foundational one: algorithm-name parsing is a bijection on the
||| 4-element `HashAlgorithm` enum — `parseAlgorithm . show = Just`. That
||| is the minimal soundness fact under content-digest dispatch
||| (`parseDigest`/`makeDigest`/`verifyDigest`) and the SRI/Docker
||| content-addressable verification that re-exports SafeDigest.
|||
||| Refs hyperpolymath/standards#132 / standards#124.
module Proven.SafeDigest.Proofs

import Proven.SafeDigest

%default total

--------------------------------------------------------------------------------
-- Algorithm dispatch is a bijection
--------------------------------------------------------------------------------

||| Parsing the rendered name of any algorithm recovers that algorithm.
||| Both `show` and `parseAlgorithm` are closed clause-matching functions
||| over a 4-constructor enum, so each case reduces to a string literal by
||| iota reduction and `Refl` type-checks definitionally — no FFI, no
||| `prim__` opacity in the dispatch path.
public export
parseAlgorithmRoundtrip : (alg : HashAlgorithm) -> parseAlgorithm (show alg) = Just alg
parseAlgorithmRoundtrip SHA256 = Refl
parseAlgorithmRoundtrip SHA384 = Refl
parseAlgorithmRoundtrip SHA512 = Refl
parseAlgorithmRoundtrip Blake3 = Refl
