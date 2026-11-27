-- SPDX-License-Identifier: MPL-2.0
module PathMain
import Proven.SafePath.ByteKernel
import Data.List
import System
%default total

-- Independent oracle: split whole byte lists, then compare whole components.
prepend : Bits8 -> List (List Bits8) -> List (List Bits8)
prepend b [] = [[b]]
prepend b (x :: xs) = (b :: x) :: xs

components : List Bits8 -> List (List Bits8)
components [] = [[]]
components (b :: bs) =
  if b == 0 || b == 47 || b == 92 then [] :: components bs
  else prepend b (components bs)

oracle : List Bits8 -> Bool
oracle bs = any (== [46, 46]) (components bs)

words : Nat -> List (List Bits8)
words Z = [[]]
words (S n) = concatMap (\b => map (b ::) (words n)) [0, 46, 47, 92, 255]

check : List Bits8 -> Bool
check bs = detect bs == oracle bs &&
  pathFinish (scanBytes 0 bs) == boolCode (oracle bs)

checks : List Bool
checks =
  map check (concatMap words [0..5]) ++
  map (\n => pathStep (cast n) 46 == 255 && pathFinish (cast n) == 255) [5..255] ++
  map (\n => check [cast n, 46, 46, 92] && check [92, 46, 46, cast n]) [0..255]

main : IO ()
main =
  if all id checks
    then putStrLn ("PASS: " ++ show (length checks) ++ " path assertions")
    else do
      putStrLn "FAIL: production path policy differs from component oracle"
      exitFailure
