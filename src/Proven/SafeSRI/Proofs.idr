-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeSRI operations
|||
||| Verifies Subresource Integrity properties: hash length correctness,
||| algorithm ordering (strength), verification soundness, and policy
||| enforcement correctness.
module Proven.SafeSRI.Proofs

import Proven.SafeSRI
import Data.Nat
import Data.String
import Data.List

%default total

--------------------------------------------------------------------------------
-- Hash Length Properties
--------------------------------------------------------------------------------

||| SHA-256 base64 output is 44 characters.
public export
sha256HashLength : expectedHashLength SHA256 = 44
sha256HashLength = Refl

||| SHA-384 base64 output is 64 characters.
public export
sha384HashLength : expectedHashLength SHA384 = 64
sha384HashLength = Refl

||| SHA-512 base64 output is 88 characters.
public export
sha512HashLength : expectedHashLength SHA512 = 88
sha512HashLength = Refl

--------------------------------------------------------------------------------
-- Algorithm Strength Ordering
--------------------------------------------------------------------------------

||| SHA-256 is weaker than SHA-384.
public export
sha256WeakerThan384 : compare SHA256 SHA384 = LT
sha256WeakerThan384 = Refl

||| SHA-384 is weaker than SHA-512.
public export
sha384WeakerThan512 : compare SHA384 SHA512 = LT
sha384WeakerThan512 = Refl

||| SHA-256 is weaker than SHA-512.
public export
sha256WeakerThan512 : compare SHA256 SHA512 = LT
sha256WeakerThan512 = Refl

||| SHA-512 is the strongest algorithm.
public export
sha512Strongest384 : compare SHA512 SHA384 = GT
sha512Strongest384 = Refl

public export
sha512Strongest256 : compare SHA512 SHA256 = GT
sha512Strongest256 = Refl

||| Algorithm comparison is reflexive.
public export
algoCompareRefl256 : compare SHA256 SHA256 = EQ
algoCompareRefl256 = Refl

public export
algoCompareRefl384 : compare SHA384 SHA384 = EQ
algoCompareRefl384 = Refl

public export
algoCompareRefl512 : compare SHA512 SHA512 = EQ
algoCompareRefl512 = Refl

--------------------------------------------------------------------------------
-- Verification Properties
--------------------------------------------------------------------------------

||| Empty integrity attribute verifies nothing.
public export
emptyIntegrityNeverVerifies : (algo : SRIAlgorithm) -> (digest : String) ->
                              verifyIntegrity (MkIntegrity []) algo digest = False
emptyIntegrityNeverVerifies _ _ = Refl

||| Verification with matching algorithm and digest succeeds.
public export
matchingHashVerifies : (algo : SRIAlgorithm) -> (digest : String) ->
                       verifyIntegrity (MkIntegrity [MkSRIHash algo digest]) algo digest = True
matchingHashVerifies SHA256 _ = Refl
matchingHashVerifies SHA384 _ = Refl
matchingHashVerifies SHA512 _ = Refl

||| Strongest algorithm of a single-hash integrity is that algorithm.
public export
singleHashStrongest : (algo : SRIAlgorithm) -> (digest : String) ->
                      strongestAlgorithm (MkIntegrity [MkSRIHash algo digest]) = Just algo
singleHashStrongest SHA256 _ = Refl
singleHashStrongest SHA384 _ = Refl
singleHashStrongest SHA512 _ = Refl

||| Strongest algorithm of empty integrity is Nothing.
public export
emptyIntegrityNoStrongest : strongestAlgorithm (MkIntegrity []) = Nothing
emptyIntegrityNoStrongest = Refl

--------------------------------------------------------------------------------
-- Policy Properties
--------------------------------------------------------------------------------

||| DisableSRI always returns PolicyExempt.
public export
disabledPolicyExempt : (attr : Maybe IntegrityAttribute) ->
                       (algo : SRIAlgorithm) -> (digest : String) ->
                       checkResource DisableSRI attr algo digest = PolicyExempt
disabledPolicyExempt _ _ _ = Refl

||| RequireSRI with no attribute returns MissingHash.
public export
requireSRIMissingHash : (algo : SRIAlgorithm) -> (digest : String) ->
                        checkResource RequireSRI Nothing algo digest = MissingHash
requireSRIMissingHash _ _ = Refl

||| PreferSRI with no attribute returns MissingHash.
public export
preferSRIMissingHash : (algo : SRIAlgorithm) -> (digest : String) ->
                       checkResource PreferSRI Nothing algo digest = MissingHash
preferSRIMissingHash _ _ = Refl
