-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
module Main
import Proven.FFI.RefCBits8
%default total

emit : Bits64 -> IO ()
emit w = putStrLn (show w ++ " " ++ case decodeWord w of
  Nothing => "invalid"
  Just b => show b)

rows : Nat -> Nat -> IO ()
rows Z index = pure ()
rows (S fuel) index = do
  let w = encodeWord (cast index)
  emit w
  emit (w - 1)
  emit (w + 2)
  emit (w + 1099511627776)
  rows fuel (S index)

main : IO ()
main = do
  rows 256 0
  emit 0
  emit 18446744073709551615
