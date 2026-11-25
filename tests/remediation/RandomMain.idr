-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module RandomMain
import Proven.SafeCrypto.Random
import Data.Vect
import Data.List
import System
%default total

prefixes : List (ByteVec 8)
prefixes = [MkByteVec [0,0,0,0,0,0,0,0], MkByteVec [1,2,3,4,5,6,7,8],
            MkByteVec [255,0,128,1,254,2,127,3]]

acceptedCounters : Nat -> List Bits64
acceptedCounters count = map cast (the (List Integer) [0..cast count]) ++
  [65535, 65536, 16777215, 16777216, 4294967294, 4294967295]

exhaustedCounters : List Bits64
exhaustedCounters = [4294967296,4294967297,4294967551,8589934591,
                    9223372036854775808,18446744073709551615]

||| Independent division/remainder oracle; production uses shifts and casts.
expectedBytes : ByteVec 8 -> Bits64 -> List Bits8
expectedBytes (MkByteVec p) c =
  let i : Integer = cast c
  in toList p ++ map cast [(i `div` 16777216) `mod` 256,
                           (i `div` 65536) `mod` 256,
                           (i `div` 256) `mod` 256, i `mod` 256]

nonceCases : Nat -> List (String, Bool)
nonceCases count =
  (do p <- prefixes
      c <- acceptedCounters count
      [("accepted wire bytes " ++ show c, case counterNonce p c of
          Right (MkByteVec bytes) => toList bytes == expectedBytes p c
          Left _ => False),
       ("accepted round trip " ++ show c, case counterNonce p c of
          Right nonce => nonceCounter nonce == c
          Left _ => False)]) ++
  (do p <- prefixes
      c <- exhaustedCounters
      pure ("exhaustion " ++ show c, case counterNonce p c of
        Left CounterExhausted => True
        _ => False)) ++
  [("reject old 0/2^32 collision", case (counterNonce (MkByteVec [0,0,0,0,0,0,0,0]) 0,
                                     counterNonce (MkByteVec [0,0,0,0,0,0,0,0]) 4294967296) of
       (Right _, Left CounterExhausted) => True
       _ => False)]

isUnavailable : Either RandomError a -> Bool
isUnavailable (Left EntropyUnavailable) = True
isUnavailable _ = False

randomCases : Nat -> List (String, Bool)
randomCases limit =
  (do n <- [0,1,12,16,32,limit]
      [("bytes unavailable", isUnavailable (randomBytes n)),
       ("byte list unavailable", isUnavailable (randomByteList n)),
       ("fresh nonce unavailable", isUnavailable (freshNonce n)),
       ("seed unavailable", isUnavailable (generateSeed n)),
       ("token unavailable", isUnavailable (randomToken n)),
       ("hex unavailable", isUnavailable (randomHex n))]) ++
  [("oversized request distinguished", case randomBytes (S limit) of
       Left (RequestTooLarge actual) => actual == S limit
       _ => False),
   ("natural unavailable", isUnavailable (randomNat 256)),
   ("natural range unavailable", isUnavailable (randomNatRange 0 10)),
   ("integer unavailable", isUnavailable (randomInt (-10) 10)),
   ("equal integer range unavailable", isUnavailable (randomInt 7 7)),
   ("inverted range rejected", case randomInt 10 1 of
       Left (InvalidRange 10 1) => True
       _ => False),
   ("double unavailable", isUnavailable randomDouble),
   ("Boolean unavailable", isUnavailable randomBool),
   ("UUID unavailable", isUnavailable randomUUID),
   ("shuffle two unavailable", isUnavailable (shuffle [1,2])),
   ("shuffle many unavailable", isUnavailable (shuffle [1..100])),
   ("partial sample unavailable", isUnavailable (sample 1 [1,2,3])),
   ("choice unavailable", isUnavailable (choice [1,2,3])),
   ("empty shuffle succeeds", case shuffle {a=Nat} [] of Right [] => True; _ => False),
   ("singleton shuffle succeeds", case shuffle [7] of Right [7] => True; _ => False),
   ("zero sample succeeds", case sample 0 [1,2,3] of Right [] => True; _ => False),
   ("whole selection succeeds", case sample 3 [1,2,3] of Right [1,2,3] => True; _ => False),
   ("oversized selection returns collection", case sample 4 [1,2,3] of Right [1,2,3] => True; _ => False)]

main : IO ()
main = do
  args <- getArgs
  -- Keep the large range at runtime instead of compiler constant expansion.
  let cases = nonceCases (1023 + length args) ++ randomCases (999999 + length args)
  let failures = filter (not . snd) cases
  traverse_ (\(name, _) => putStrLn ("FAIL: " ++ name)) failures
  if null failures
    then putStrLn ("PASS: " ++ show (length cases) ++ " nonce/entropy assertions")
    else do
      putStrLn (show (length failures) ++ " nonce/entropy assertion(s) failed")
      exitFailure
