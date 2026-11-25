-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module Main

import Proven.ECHIDNA.Validator
import Proven.ECHIDNA.ProofTerm
import Proven.SafePassword.Hash
import Proven.SafeWebhook
import Proven.FFI.SafeWebhook
import Data.List
import Data.String
import System

%default total

accepted : ValidationResult -> Bool
accepted (IsValid _) = Prelude.True
accepted (IsInvalid _) = Prelude.False

numeral : Nat -> ProofTerm
numeral Z = Zero
numeral (S n) = Succ (numeral n)

natCases : List (String, Bool)
natCases = do
  x <- [0..12]
  y <- [0..12]
  pure ("Nat equality " ++ show x ++ "/" ++ show y,
        accepted (validateProof ProofTerm.Refl (Eq (numeral x) (numeral y))) == (x == y))

unsupported : List ProofTerm
unsupported =
  [ Var "free", TyNat, TyBool, TyProp, Zero, Succ Zero
  , ProofTerm.True, ProofTerm.False, Forall "x" TyNat TyProp
  , Exists "x" TyNat TyProp, Implies TyProp TyProp, And TyProp TyProp
  , Or TyProp TyProp, Not TyProp, Eq Zero Zero, App Zero Zero
  , Lambda "x" TyNat (Var "x"), Axiom "untrusted", Intro "x" ProofTerm.Refl
  , Apply (Axiom "untrusted") (Eq Zero Zero)
  ]

kernelCases : List (String, Bool)
kernelCases =
  [ ("false Refl goal", not (accepted (validateProof ProofTerm.Refl (Eq Zero (Succ Zero)))))
  , ("untrusted axiom", not (accepted (validateProof (Axiom "untrusted") (Eq Zero (Succ Zero)))))
  , ("comprehensive false goal", not (accepted (validateComprehensive ProofTerm.Refl (Eq Zero (Succ Zero)))))
  , ("comprehensive true goal", accepted (validateComprehensive ProofTerm.Refl (Eq Zero Zero)))
  , ("true = true", accepted (validateProof ProofTerm.Refl (Eq ProofTerm.True ProofTerm.True)))
  , ("false = false", accepted (validateProof ProofTerm.Refl (Eq ProofTerm.False ProofTerm.False)))
  , ("true != false", not (accepted (validateProof ProofTerm.Refl (Eq ProofTerm.True ProofTerm.False))))
  , ("false != true", not (accepted (validateProof ProofTerm.Refl (Eq ProofTerm.False ProofTerm.True))))
  , ("mixed types", not (accepted (validateProof ProofTerm.Refl (Eq Zero ProofTerm.False))))
  , ("ill-typed successor", not (accepted (validateProof ProofTerm.Refl (Eq (Succ ProofTerm.True) (Succ ProofTerm.True)))))
  , ("open reflexivity", not (accepted (validateProof ProofTerm.Refl (Eq (Var "x") (Var "x")))))
  , ("proposition is not proof", not (accepted (validateProof (Eq Zero Zero) (Eq Zero Zero))))
  , ("Prop goal unsupported", not (accepted (validateProof ProofTerm.Refl TyProp)))
  ] ++ map (\p => ("unsupported proof " ++ show p, not (accepted (validateProof p (Eq Zero Zero))))) unsupported

unicodeCases : List (String, Bool)
unicodeCases =
  [ ("U+0100 differs from NUL", not (constantTimeStringCompare (pack [chr 256]) (pack [chr 0])))
  , ("U+0141 differs from A", not (constantTimeStringCompare (pack [chr 321]) "A"))
  , ("supplementary differs from BMP", not (constantTimeStringCompare (pack [chr 128512]) (pack [chr 62976])))
  , ("embedded NUL retained", not (constantTimeStringCompare (pack ['a', chr 0, 'b']) "ab"))
  , ("different lengths", not (constantTimeStringCompare "a" "aa"))
  , ("ASCII equal", constantTimeStringCompare "password" "password")
  , ("empty equal", constantTimeStringCompare "" "")
  , ("non-ASCII equal", constantTimeStringCompare (pack [chr 256, chr 128512]) (pack [chr 256, chr 128512]))
  , ("no implicit normalization", not (constantTimeStringCompare (pack [chr 233]) (pack ['e', chr 769])))
  ] ++ do
    x <- [0, 1, 65, 255, 256, 321, 65535, 128512]
    y <- [0, 1, 65, 255, 256, 321, 65535, 128512]
    pure ("Unicode pair " ++ show x ++ "/" ++ show y,
          constantTimeStringCompare (pack [chr x]) (pack [chr y]) == (x == y))

webhookCases : List (String, Bool)
webhookCases = do
  alg <- [HMAC_SHA256, HMAC_SHA512, Ed25519Sig]
  sig <- ["AAAA", "", "abcdef0123456789", "not a signature"]
  key <- ["secret", "other"]
  body <- ["payload", "tampered"]
  let signature = MkSignature alg sig
  let event = MkWebhookEvent (MkDeliveryId "attack") "test" 1000 body signature
  let tracker = MkTracker [MkDeliveryId "existing"] 8
  let (result, after) = verifyWebhook key event 1000 tracker
  [ ("reject signature " ++ show alg ++ "/" ++ sig, not (verifySignature key body signature))
  , ("unavailable algorithm", result == UnsupportedAlgorithm)
  , ("preserve delivery IDs", seenIds after == seenIds tracker)
  , ("preserve tracker capacity", maxSize after == maxSize tracker)
  , ("expired timestamp", fst (verifyWebhook key event 2000 tracker) == ExpiredTimestamp)
  ]

tests : List (String, Bool)
tests = kernelCases ++ natCases ++ unicodeCases ++ webhookCases ++
  map (\header => ("Idris webhook export rejects " ++ header,
       proven_idris_webhook_verify_signature "secret" "payload" header == 0))
      ["sha256=AAAA", "sha512=AAAA", "sha256=", "unknown=AAAA"]

main : IO ()
main = do
  let failures = filter (not . snd) tests
  traverse_ (\(name, _) => putStrLn ("FAIL: " ++ name)) failures
  if null failures
    then putStrLn ("PASS: " ++ show (length tests) ++ " critical-remediation runtime assertions")
    else do
      putStrLn (show (length failures) ++ " regression assertion(s) failed")
      exitFailure
