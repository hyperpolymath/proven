-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeWebhook - Webhook parsing and replay-tracking types
|||
||| Authentication is unavailable until a real cryptographic backend is
||| connected. Verification fails closed and never records an unverified ID.
module Proven.SafeWebhook

import Data.String
import Data.List
import Data.Nat
import Data.Maybe

%default total

||| Recognized signature algorithm names (backends are not implemented yet).
public export
data SignatureAlgorithm = HMAC_SHA256 | HMAC_SHA512 | Ed25519Sig

public export
Show SignatureAlgorithm where
  show HMAC_SHA256 = "hmac-sha256"
  show HMAC_SHA512 = "hmac-sha512"
  show Ed25519Sig = "ed25519"

public export
Eq SignatureAlgorithm where
  HMAC_SHA256 == HMAC_SHA256 = True
  HMAC_SHA512 == HMAC_SHA512 = True
  Ed25519Sig == Ed25519Sig = True
  _ == _ = False

||| A webhook signature
public export
record WebhookSignature where
  constructor MkSignature
  sigAlgorithm : SignatureAlgorithm
  sigValue     : String   -- Hex-encoded signature

||| A webhook delivery ID (for idempotency)
public export
data DeliveryId : Type where
  MkDeliveryId : (id : String) -> DeliveryId

public export
Eq DeliveryId where
  MkDeliveryId a == MkDeliveryId b = a == b

public export
Show DeliveryId where
  show (MkDeliveryId id) = id

||| A webhook event
public export
record WebhookEvent where
  constructor MkWebhookEvent
  deliveryId : DeliveryId
  eventType  : String
  timestamp  : Nat      -- Unix timestamp
  payload    : String
  signature  : WebhookSignature

||| Webhook verification result
public export
data VerifyResult =
    Verified
  | InvalidSignature
  | ExpiredTimestamp
  | DuplicateDelivery
  | MissingSignature
  | UnsupportedAlgorithm

public export
Eq VerifyResult where
  Verified == Verified = True
  InvalidSignature == InvalidSignature = True
  ExpiredTimestamp == ExpiredTimestamp = True
  DuplicateDelivery == DuplicateDelivery = True
  MissingSignature == MissingSignature = True
  UnsupportedAlgorithm == UnsupportedAlgorithm = True
  _ == _ = False

||| Maximum age of a webhook (prevent replay attacks) - 5 minutes
public export
maxWebhookAge : Nat
maxWebhookAge = 300

||| Check if webhook timestamp is recent enough
public export
isTimestampValid : Nat -> Nat -> Bool
isTimestampValid eventTime currentTime =
  let diff = if currentTime >= eventTime
               then minus currentTime eventTime
               else minus eventTime currentTime
  in diff <= maxWebhookAge

||| String comparison. No compiler/runtime timing guarantee is established.
public export
constantTimeEq : String -> String -> Bool
constantTimeEq a b =
  let aChars = unpack a
      bChars = unpack b
  in length aChars == length bChars &&
     foldl (\acc, (x, y) => acc && ord x == ord y) True (zip aChars bChars)

||| Compatibility Boolean API: no signature algorithm is implemented yet.
||| Syntax is not authentication; callers must treat False as rejection.
public export
verifySignature : String -> String -> WebhookSignature -> Bool
verifySignature secret payload sig = False

||| All keys, payloads, algorithms and signature strings fail closed.
public export
unimplementedSignatureRejected : (secret, payload : String) ->
                                 (sig : WebhookSignature) ->
                                 verifySignature secret payload sig = False
unimplementedSignatureRejected secret payload sig = Refl

||| Idempotency tracker
public export
record IdempotencyTracker where
  constructor MkTracker
  seenIds   : List DeliveryId
  maxSize   : Nat

||| Create a new tracker
public export
newTracker : Nat -> IdempotencyTracker
newTracker maxSz = MkTracker [] maxSz

||| Check and record a delivery
public export
checkDelivery : DeliveryId -> IdempotencyTracker -> (Bool, IdempotencyTracker)
checkDelivery did tracker =
  if elem did (seenIds tracker)
    then (False, tracker)  -- Duplicate
    else let newIds = did :: take (minus (maxSize tracker) 1) (seenIds tracker)
         in (True, { seenIds := newIds } tracker)

||| Preserve timestamp diagnostics while authentication is unavailable.
public export
unavailableVerification : Bool -> VerifyResult
unavailableVerification True = UnsupportedAlgorithm
unavailableVerification False = ExpiredTimestamp

||| Authentication fails closed. No delivery is recorded before verification.
public export
verifyWebhook : String -> WebhookEvent -> Nat -> IdempotencyTracker ->
                (VerifyResult, IdempotencyTracker)
verifyWebhook secret event currentTime tracker =
  (unavailableVerification (isTimestampValid (timestamp event) currentTime), tracker)

public export
unavailableNeverVerified : (fresh : Bool) ->
                           unavailableVerification fresh = Verified -> Void
unavailableNeverVerified True Refl impossible
unavailableNeverVerified False Refl impossible

||| Generic rejection theorem, including fresh forged and duplicate events.
public export
webhookNeverVerified : (secret : String) -> (event : WebhookEvent) ->
                       (now : Nat) -> (tracker : IdempotencyTracker) ->
                       fst (verifyWebhook secret event now tracker) = Verified -> Void
webhookNeverVerified secret event now tracker accepted =
  unavailableNeverVerified (isTimestampValid (timestamp event) now) accepted

||| Failed authentication cannot poison the idempotency tracker.
public export
webhookPreservesTracker : (secret : String) -> (event : WebhookEvent) ->
                          (now : Nat) -> (tracker : IdempotencyTracker) ->
                          snd (verifyWebhook secret event now tracker) = tracker
webhookPreservesTracker secret event now tracker = Refl

||| Parse a webhook signature header
||| Recognizes sha256= and sha512= prefixes; this does not authenticate a digest.
public export
parseSignatureHeader : String -> Maybe WebhookSignature
parseSignatureHeader s =
  if isPrefixOf "sha256=" s
    then Just (MkSignature HMAC_SHA256 (substr 7 (length s) s))
    else if isPrefixOf "sha512=" s
      then Just (MkSignature HMAC_SHA512 (substr 7 (length s) s))
      else Nothing

-- ----------------------------------------------------------------
-- Proof types
-- ----------------------------------------------------------------

||| Evidence must refer to successful execution on the exact request/state.
||| Uninhabited while authentication is unavailable (webhookNeverVerified).
public export
data WebhookVerified : String -> WebhookEvent -> Nat -> IdempotencyTracker -> Type where
  MkWebhookVerified : fst (verifyWebhook secret event now tracker) = Verified ->
                      WebhookVerified secret event now tracker

||| Proof that timestamp is within acceptable window
public export
data FreshTimestamp : Nat -> Nat -> Type where
  MkFreshTimestamp : isTimestampValid t1 t2 = True -> FreshTimestamp t1 t2

||| Proof that delivery is not a replay
public export
data UniqueDelivery : DeliveryId -> IdempotencyTracker -> Type where
  MkUniqueDelivery : not (elem did (seenIds tracker)) = True ->
                     UniqueDelivery did tracker
