-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Main : Entry point for proven-ca. Prints server identity and
-- enumerates all protocol type constructors.

module Main

import CA

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

allCertTypes : List CertType
allCertTypes = [Root, Intermediate, EndEntity, CodeSigning, EmailProtection, OCSPSigning]

allKeyAlgorithms : List KeyAlgorithm
allKeyAlgorithms = [RSA2048, RSA4096, ECDSA_P256, ECDSA_P384, Ed25519, Ed448]

allCertStatuses : List CertStatus
allCertStatuses = [Valid, Revoked, Expired, Suspended]

allRevocationReasons : List RevocationReason
allRevocationReasons =
  [ Unspecified, KeyCompromise, CACompromise
  , AffiliationChanged, Superseded, CessationOfOperation, CertificateHold
  ]

allExtensions : List Extension
allExtensions =
  [ BasicConstraints, KeyUsage, ExtKeyUsage
  , SubjectAltName, AuthorityInfoAccess, CRLDistributionPoints
  ]

main : IO ()
main = do
  putStrLn "proven-ca : Certificate Authority server"
  putStrLn $ "  Max path length: " ++ show maxPathLength
  putStrLn $ "  Default validity: " ++ show defaultValidityDays ++ " days"
  putStrLn $ "  CRL update interval: " ++ show crlUpdateHours ++ " hours"
  putStrLn $ "  CertTypes:          " ++ show allCertTypes
  putStrLn $ "  KeyAlgorithms:      " ++ show allKeyAlgorithms
  putStrLn $ "  CertStatuses:       " ++ show allCertStatuses
  putStrLn $ "  RevocationReasons:  " ++ show allRevocationReasons
  putStrLn $ "  Extensions:         " ++ show allExtensions
