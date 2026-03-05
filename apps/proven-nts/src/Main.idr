-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for the proven-nts skeleton.
-- | Prints the server name and demonstrates type constructors.

module Main

import NTS

%default total

||| All NTS-KE record type constructors for demonstration.
allRecordTypes : List RecordType
allRecordTypes =
  [ EndOfMessage, NextProtocol, Error, Warning, AEADAlgorithm
  , Cookie, CookiePlaceholder, NTSKEServer, NTSKEPort ]

||| All NTS-KE error code constructors for demonstration.
allErrorCodes : List ErrorCode
allErrorCodes = [UnrecognizedCritical, BadRequest, InternalError]

||| All AEAD algorithm constructors for demonstration.
allAEADAlgorithms : List AEADAlgorithm
allAEADAlgorithms = [AEAD_AES_128_GCM, AEAD_AES_256_GCM, AEAD_AES_SIV_CMAC_256]

||| All handshake state constructors for demonstration.
allHandshakeStates : List HandshakeState
allHandshakeStates = [Initial, Negotiating, Established, Failed]

main : IO ()
main = do
  putStrLn "proven-nts: RFC 8915 Network Time Security"
  putStrLn $ "  NTS-KE port:    " ++ show ntskePort
  putStrLn $ "  Cookie count:   " ++ show defaultCookieCount
  putStrLn $ "  Record types:     " ++ show allRecordTypes
  putStrLn $ "  Error codes:      " ++ show allErrorCodes
  putStrLn $ "  AEAD algorithms:  " ++ show allAEADAlgorithms
  putStrLn $ "  Handshake states: " ++ show allHandshakeStates
