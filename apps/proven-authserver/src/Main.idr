-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-authserver: Main entry point.
--
-- Minimal main that prints the server name and demonstrates the core
-- type constructors defined in Authserver.Types.
--
-- Usage:
--   idris2 --build proven-authserver.ipkg
--   ./build/exec/proven-authserver

module Main

import Authserver

%default total

||| Print all constructors of each core sum type for verification.
covering
main : IO ()
main = do
  putStrLn "proven-authserver v0.1.0 -- authentication server"
  putStrLn ""
  putStrLn $ "Auth port:           " ++ show authPort
  putStrLn $ "Token TTL:           " ++ show tokenTTL ++ " seconds"
  putStrLn $ "Refresh TTL:         " ++ show refreshTTL ++ " seconds"
  putStrLn $ "Max login attempts:  " ++ show maxLoginAttempts

  putStrLn "\n--- AuthMethod ---"
  putStrLn $ "  " ++ show Password
  putStrLn $ "  " ++ show Certificate
  putStrLn $ "  " ++ show OAuth2
  putStrLn $ "  " ++ show SAML
  putStrLn $ "  " ++ show FIDO2
  putStrLn $ "  " ++ show Kerberos
  putStrLn $ "  " ++ show LDAP
  putStrLn $ "  " ++ show RADIUS

  putStrLn "\n--- TokenType ---"
  putStrLn $ "  " ++ show Access
  putStrLn $ "  " ++ show Refresh
  putStrLn $ "  " ++ show ID
  putStrLn $ "  " ++ show API

  putStrLn "\n--- AuthResult ---"
  putStrLn $ "  " ++ show Success
  putStrLn $ "  " ++ show InvalidCredentials
  putStrLn $ "  " ++ show AccountLocked
  putStrLn $ "  " ++ show AccountExpired
  putStrLn $ "  " ++ show MFARequired
  putStrLn $ "  " ++ show IPBlocked

  putStrLn "\n--- MFAMethod ---"
  putStrLn $ "  " ++ show TOTP
  putStrLn $ "  " ++ show SMS
  putStrLn $ "  " ++ show Push
  putStrLn $ "  " ++ show FIDO2_MFA
  putStrLn $ "  " ++ show Email

  putStrLn "\n--- SessionState ---"
  putStrLn $ "  " ++ show Active
  putStrLn $ "  " ++ show Expired
  putStrLn $ "  " ++ show Revoked
  putStrLn $ "  " ++ show Locked
