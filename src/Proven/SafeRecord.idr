-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
|||
||| SafeRecord — Verified record subset destructuring
|||
||| Provides formally proven record partitioning: given a record schema
||| and a subset of fields to extract, produces the remainder ("rest")
||| with compile-time proofs of:
|||
|||   - Exhaustiveness: every field lands in exactly one partition
|||   - Disjointness: no field appears in both extracted and rest
|||   - Completeness: |extracted| + |rest| = |full|
|||
||| This is the formal foundation for:
|||   - Ephapax linear rest-patterns: `let { a ⊸ x, ...rest } = record`
|||   - ReScript PPX: `@rest let { className, ?children, ...otherProps } = props`
|||
||| The proofs here are strictly more powerful than anything expressible
||| in ReScript, TypeScript, OCaml, or Haskell (without extensions).
||| They require dependent types for the theorems and totality checking
||| for the proofs. Ephapax's linear types will additionally enforce
||| single-use consumption at the value level.
|||
module Proven.SafeRecord

import public Proven.SafeRecord.Types
import public Proven.SafeRecord.Proofs

%default total
