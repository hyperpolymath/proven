-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
-- Baseline-only probe: the remediation deliberately changes the return type.
module BeforeNonce
import Proven.SafeCrypto.Random
import Data.Vect
import System
%default total
main : IO ()
main = do
  let pfx = MkByteVec [0,0,0,0,0,0,0,0]
  let MkByteVec zero = counterNonce pfx 0
  let MkByteVec overflow = counterNonce pfx 4294967296
  if zero == overflow
    then do
      putStrLn "FAIL: baseline nonce(0) equals nonce(2^32)"
      exitFailure
    else putStrLn "PASS: distinct counter nonces"
