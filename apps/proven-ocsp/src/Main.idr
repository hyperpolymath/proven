-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Main : Entry point for proven-ocsp. Prints server identity and
-- enumerates all protocol type constructors.

module Main

import OCSP

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

allCertStatuses : List CertStatus
allCertStatuses = [Good, Revoked, Unknown]

allResponseStatuses : List ResponseStatus
allResponseStatuses =
  [Successful, MalformedRequest, InternalError, TryLater, SigRequired, Unauthorized]

allHashAlgorithms : List HashAlgorithm
allHashAlgorithms = [SHA1, SHA256, SHA384, SHA512]

main : IO ()
main = do
  putStrLn "proven-ocsp : RFC 6960 OCSP responder"
  putStrLn $ "  Port: " ++ show ocspPort
  putStrLn $ "  Max request size: " ++ show maxRequestSize ++ " bytes"
  putStrLn $ "  Response TTL: " ++ show defaultResponseTTL ++ " seconds"
  putStrLn $ "  CertStatuses:      " ++ show allCertStatuses
  putStrLn $ "  ResponseStatuses:  " ++ show allResponseStatuses
  putStrLn $ "  HashAlgorithms:    " ++ show allHashAlgorithms
