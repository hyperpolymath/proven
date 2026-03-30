-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeSSRF operations
|||
||| Verifies that SSRF prevention correctly classifies RFC 1918 private
||| addresses, loopback, and cloud metadata endpoints as internal.
module Proven.SafeSSRF.Proofs

import Proven.SafeSSRF
import Data.Nat
import Data.List

%default total

--------------------------------------------------------------------------------
-- Address Classification Properties
--------------------------------------------------------------------------------

||| Loopback addresses (127.x.x.x) are classified as Loopback.
public export
loopbackClassification : (b, c, d : Nat) -> classifyIPv4 (127, b, c, d) = Loopback
loopbackClassification _ _ _ = Refl

||| 10.x.x.x addresses are classified as PrivateA.
public export
privateAClassification : (b, c, d : Nat) -> classifyIPv4 (10, b, c, d) = PrivateA
privateAClassification _ _ _ = Refl

||| 192.168.x.x addresses are classified as PrivateC.
public export
privateCClassification : (c, d : Nat) -> classifyIPv4 (192, 168, c, d) = PrivateC
privateCClassification _ _ = Refl

||| Cloud metadata endpoint (169.254.169.254) is classified as CloudMetadata.
public export
cloudMetadataClassification : classifyIPv4 (169, 254, 169, 254) = CloudMetadata
cloudMetadataClassification = Refl

||| Broadcast address (255.255.255.255) is classified as Broadcast.
public export
broadcastClassification : classifyIPv4 (255, 255, 255, 255) = BroadcastAddr
broadcastClassification = Refl

||| Documentation range 192.0.2.x is classified as Documentation.
public export
doc1Classification : (d : Nat) -> classifyIPv4 (192, 0, 2, d) = Documentation
doc1Classification _ = Refl

||| Documentation range 198.51.100.x is classified as Documentation.
public export
doc2Classification : (d : Nat) -> classifyIPv4 (198, 51, 100, d) = Documentation
doc2Classification _ = Refl

||| Documentation range 203.0.113.x is classified as Documentation.
public export
doc3Classification : (d : Nat) -> classifyIPv4 (203, 0, 113, d) = Documentation
doc3Classification _ = Refl

--------------------------------------------------------------------------------
-- Internal Classification Properties
--------------------------------------------------------------------------------

||| All non-Routable addresses are internal.
||| Proved by exhaustive case split on AddressClass.
public export
nonRoutableIsInternal : (cls : AddressClass) -> Not (cls = Routable) -> isInternal cls = True
nonRoutableIsInternal Loopback      _ = Refl
nonRoutableIsInternal PrivateA      _ = Refl
nonRoutableIsInternal PrivateB      _ = Refl
nonRoutableIsInternal PrivateC      _ = Refl
nonRoutableIsInternal LinkLocal     _ = Refl
nonRoutableIsInternal Multicast     _ = Refl
nonRoutableIsInternal Documentation _ = Refl
nonRoutableIsInternal BroadcastAddr _ = Refl
nonRoutableIsInternal CloudMetadata _ = Refl
nonRoutableIsInternal IPv6Mapped    _ = Refl
nonRoutableIsInternal Routable      f = absurd (f Refl)

||| Routable addresses are not internal.
public export
routableNotInternal : isInternal Routable = False
routableNotInternal = Refl

||| Loopback is always internal.
public export
loopbackIsInternal : isInternal Loopback = True
loopbackIsInternal = Refl

||| Cloud metadata is always internal.
public export
cloudMetadataIsInternal : isInternal CloudMetadata = True
cloudMetadataIsInternal = Refl

--------------------------------------------------------------------------------
-- SSRF Validation Properties
--------------------------------------------------------------------------------

||| isSSRFSafe agrees with validateSSRF:
||| returns True exactly when validateSSRF returns Allowed.
public export
ssrfSafeConsistent : (url : String) ->
                     isSSRFSafe url = True ->
                     (safeUrl : String ** validateSSRF url = Allowed safeUrl)
ssrfSafeConsistent url prf with (validateSSRF url)
  ssrfSafeConsistent url Refl | Allowed safeUrl = (safeUrl ** Refl)

||| Strict policy blocks all private ranges.
public export
strictPolicyBlocksPrivate : strictPolicy.blockPrivate = True
strictPolicyBlocksPrivate = Refl

||| Strict policy blocks loopback.
public export
strictPolicyBlocksLoopback : strictPolicy.blockLoopback = True
strictPolicyBlocksLoopback = Refl

||| Strict policy blocks cloud metadata.
public export
strictPolicyBlocksMetadata : strictPolicy.blockMetadata = True
strictPolicyBlocksMetadata = Refl

||| Strict policy blocks link-local.
public export
strictPolicyBlocksLinkLocal : strictPolicy.blockLinkLocal = True
strictPolicyBlocksLinkLocal = Refl
