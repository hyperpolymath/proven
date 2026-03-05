-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for the proven-kerberos skeleton.
-- | Prints the server name and demonstrates type constructors.

module Main

import Kerberos

%default total

||| All Kerberos message type constructors for demonstration.
allMessageTypes : List MessageType
allMessageTypes =
  [ AS_REQ, AS_REP, TGS_REQ, TGS_REP, AP_REQ
  , AP_REP, KRB_ERROR, KRB_SAFE, KRB_PRIV, KRB_CRED ]

||| All Kerberos encryption type constructors for demonstration.
allEncryptionTypes : List EncryptionType
allEncryptionTypes =
  [ AES256_CTS_HMAC_SHA1, AES128_CTS_HMAC_SHA1
  , AES256_CTS_HMAC_SHA384, RC4_HMAC, DES3_CBC_SHA1 ]

||| All Kerberos error code constructors for demonstration.
allErrorCodes : List ErrorCode
allErrorCodes =
  [ KDC_ERR_NONE, KDC_ERR_NAME_EXP, KDC_ERR_SERVICE_EXP
  , KDC_ERR_BAD_PVNO, KDC_ERR_C_OLD_MAST_KVNO, KDC_ERR_S_OLD_MAST_KVNO
  , KDC_ERR_C_PRINCIPAL_UNKNOWN, KDC_ERR_S_PRINCIPAL_UNKNOWN
  , KDC_ERR_PREAUTH_FAILED, KDC_ERR_PREAUTH_REQUIRED ]

||| All Kerberos ticket flag constructors for demonstration.
allTicketFlags : List TicketFlag
allTicketFlags =
  [Forwardable, Forwarded, Proxiable, Proxy, Renewable, PreAuthent, HWAuthent]

main : IO ()
main = do
  putStrLn "proven-kerberos: RFC 4120 Kerberos V5"
  putStrLn $ "  KDC port:            " ++ show kdcPort
  putStrLn $ "  kpasswd port:        " ++ show kpasswdPort
  putStrLn $ "  Ticket lifetime:     " ++ show defaultTicketLifetime ++ "s"
  putStrLn $ "  Renewable lifetime:  " ++ show maxRenewableLifetime ++ "s"
  putStrLn $ "  Message types:       " ++ show allMessageTypes
  putStrLn $ "  Encryption types:    " ++ show allEncryptionTypes
  putStrLn $ "  Error codes:         " ++ show allErrorCodes
  putStrLn $ "  Ticket flags:        " ++ show allTicketFlags
