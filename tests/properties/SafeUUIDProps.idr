-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeUUIDProps

import Proven.Core
import Proven.SafeUUID

%default total

||| Property: Valid UUID v4 parses
prop_validUUIDv4Parses : isOk (parseUUID "550e8400-e29b-41d4-a716-446655440000") = True
prop_validUUIDv4Parses = Refl

||| Property: Uppercase UUID parses
prop_uppercaseUUIDParses : isOk (parseUUID "550E8400-E29B-41D4-A716-446655440000") = True
prop_uppercaseUUIDParses = Refl

||| Property: Invalid format fails
prop_invalidFormatFails : isErr (parseUUID "not-a-uuid") = True
prop_invalidFormatFails = Refl

||| Property: Wrong length fails
prop_wrongLengthFails : isErr (parseUUID "550e8400-e29b-41d4-a716") = True
prop_wrongLengthFails = Refl

||| Property: Nil UUID parses
prop_nilUUIDParses : isOk (parseUUID "00000000-0000-0000-0000-000000000000") = True
prop_nilUUIDParses = Refl

||| OWED: Generated UUID is valid
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_generatedUUIDValid_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_generatedUUIDValid : isOk (parseUUID (toString (generateUUIDv4 seed))) = True

||| OWED: UUID version is extractable
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_versionExtractable_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_versionExtractable : (uuid : ValidUUID) -> getVersion uuid >= 1 && getVersion uuid <= 5

||| OWED: UUID formatting is consistent
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_formatConsistent_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_formatConsistent : (uuid : ValidUUID) ->
                          parseUUID (toString uuid) = Ok uuid

||| Property: UUID comparison is reflexive
prop_uuidEqReflexive : (uuid : ValidUUID) -> uuid == uuid = True
prop_uuidEqReflexive uuid = Refl

||| OWED: UUID bytes roundtrip
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_bytesRoundtrip_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_bytesRoundtrip : (uuid : ValidUUID) ->
                        fromBytes (toBytes uuid) = Ok uuid

||| Test runner for UUID properties
export
runUUIDProperties : IO ()
runUUIDProperties = do
  putStrLn "SafeUUID Property Tests"
  putStrLn "======================="
  putStrLn "prop_validUUIDv4Parses: PASS (proven by type)"
  putStrLn "prop_uppercaseUUIDParses: PASS (proven by type)"
  putStrLn "prop_invalidFormatFails: PASS (proven by type)"
  putStrLn "prop_wrongLengthFails: PASS (proven by type)"
  putStrLn "prop_nilUUIDParses: PASS (proven by type)"
  putStrLn "prop_uuidEqReflexive: PASS (proven by type)"
  putStrLn ""
