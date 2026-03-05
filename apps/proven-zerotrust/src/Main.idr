-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Main : Entry point for proven-zerotrust. Prints server identity and
-- enumerates all protocol type constructors.

module Main

import ZeroTrust

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

allAuthFactors : List AuthFactor
allAuthFactors = [Certificate, Token, Biometric, FIDO2, TOTP, Push]

allTrustLevels : List TrustLevel
allTrustLevels = [None, Low, Medium, High, Full]

allPolicyDecisions : List PolicyDecision
allPolicyDecisions = [Allow, Deny, Challenge, StepUp, Quarantine]

allContextSignals : List ContextSignal
allContextSignals = [DeviceHealth, NetworkLocation, UserBehavior, TimeOfDay, GeoLocation, RiskScore]

allSessionStates : List SessionState
allSessionStates = [Unauthenticated, PartialAuth, Authenticated, Elevated, Locked]

main : IO ()
main = do
  putStrLn "proven-zerotrust : Zero Trust authentication server"
  putStrLn $ "  Max session duration: " ++ show maxSessionDuration ++ " seconds"
  putStrLn $ "  Reauth interval: " ++ show reauthInterval ++ " seconds"
  putStrLn $ "  AuthFactors:       " ++ show allAuthFactors
  putStrLn $ "  TrustLevels:       " ++ show allTrustLevels
  putStrLn $ "  PolicyDecisions:   " ++ show allPolicyDecisions
  putStrLn $ "  ContextSignals:    " ++ show allContextSignals
  putStrLn $ "  SessionStates:     " ++ show allSessionStates
