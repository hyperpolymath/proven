-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module NativeNonceFixture
import Data.Vect
import Proven.SafeCrypto.Random
import System
%default total

||| Test adapter only: actual counter encoding/validation stays in production.
public export
%noinline
counterResult : Bits64 -> Bits64
counterResult counter =
  case counterNonce (MkByteVec [0,0,0,0,0,0,0,0]) counter of
    Left _ => 18446744073709551615
    Right nonce => nonceCounter nonce

public export
%noinline
entropyResult : Bits64 -> Bits64
entropyResult size =
  case randomBytes (cast size) of
    Left _ => 0
    Right _ => 1

main : IO ()
main = do
  args <- getArgs
  printLn (counterResult (cast (length args)))
  printLn (entropyResult (cast (length args)))
