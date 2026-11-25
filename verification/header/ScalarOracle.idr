-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
module Main
import Proven.FFI.RefCBits8
%default total

rows : Nat -> Nat -> IO ()
rows Z index = pure ()
rows (S fuel) index = do
  putStrLn (show index ++ " " ++ show (encodeWord (cast index)))
  rows fuel (S index)

main : IO ()
main = rows 256 0
