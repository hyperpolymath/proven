-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Main : Entry point for proven-kms. Prints server identity and
-- enumerates all protocol type constructors.

module Main

import KMS

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

allObjectTypes : List ObjectType
allObjectTypes = [SymmetricKey, PublicKey, PrivateKey, SecretData, Certificate, OpaqueData]

allOperations : List Operation
allOperations =
  [ Create, Get, Activate, Revoke, Destroy, Locate
  , Register, Rekey, Encrypt, Decrypt, Sign, Verify, Wrap, Unwrap, MAC
  ]

allKeyStates : List KeyState
allKeyStates = [PreActive, Active, Deactivated, Compromised, Destroyed, DestroyedCompromised]

allAlgorithms : List Algorithm
allAlgorithms =
  [ AES128, AES256, RSA2048, RSA4096
  , ECDSA_P256, ECDSA_P384, Ed25519, ChaCha20Poly1305, HMAC_SHA256
  ]

main : IO ()
main = do
  putStrLn "proven-kms : Key Management Server"
  putStrLn $ "  Port: " ++ show kmsPort
  putStrLn $ "  Max key size: " ++ show maxKeySize ++ " bytes"
  putStrLn $ "  ObjectTypes:   " ++ show allObjectTypes
  putStrLn $ "  Operations:    " ++ show allOperations
  putStrLn $ "  KeyStates:     " ++ show allKeyStates
  putStrLn $ "  Algorithms:    " ++ show allAlgorithms
