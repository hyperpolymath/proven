-- SPDX-License-Identifier: PMPL-1.0
module Proven.Provenance.Utils

import Data.List

%default total

public export
uniqueCons : Eq a => a -> List a -> List a
uniqueCons x xs = if elem x xs then xs else x :: xs

public export
nub' : Eq a => List a -> List a
nub' [] = []
nub' (y :: ys) = y :: nub' (filter (/= y) ys)

public export
any' : (a -> Bool) -> List a -> Bool
any' _ [] = False
any' p (x :: xs) = p x || any' p xs

public export
safeHead : List a -> Maybe a
safeHead [] = Nothing
safeHead (x :: _) = Just x
