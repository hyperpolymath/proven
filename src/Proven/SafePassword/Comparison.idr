-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module Proven.SafePassword.Comparison

import Decidable.Equality

%default total

||| Functional character equality without narrowing codepoints to bytes.
||| This is not a claim about compiler-generated execution time.
public export
charEqual : Char -> Char -> Bool
charEqual x y = case decEq x y of
  Yes _ => True
  No _ => False

public export
charEqualSound : (x, y : Char) -> charEqual x y = True -> x = y
charEqualSound x y accepted with (decEq x y)
  charEqualSound x y accepted | Yes equal = equal
  charEqualSound x y Refl | No _ impossible

public export
charEqualSelf : (x : Char) -> charEqual x x = True
charEqualSelf x with (decEq x x)
  charEqualSelf x | Yes _ = Refl
  charEqualSelf x | No different = void (different Refl)

public export
both : Bool -> Bool -> Bool
both True True = True
both _ _ = False

bothLeft : (a, b : Bool) -> both a b = True -> a = True
bothLeft True True accepted = Refl
bothLeft True False Refl impossible
bothLeft False _ Refl impossible

bothRight : (a, b : Bool) -> both a b = True -> b = True
bothRight True True accepted = Refl
bothRight True False Refl impossible
bothRight False _ Refl impossible

||| Compare full characters, retaining the recursive comparison as an argument.
||| Length-dependent work and target-level timing remain outside this contract.
public export
charsEqual : List Char -> List Char -> Bool
charsEqual [] [] = True
charsEqual (x :: xs) (y :: ys) = both (charEqual x y) (charsEqual xs ys)
charsEqual _ _ = False

public export
charsEqualSound : (xs, ys : List Char) -> charsEqual xs ys = True -> xs = ys
charsEqualSound [] [] accepted = Refl
charsEqualSound [] (_ :: _) Refl impossible
charsEqualSound (_ :: _) [] Refl impossible
charsEqualSound (x :: xs) (y :: ys) accepted =
  cong2 (::)
    (charEqualSound x y (bothLeft (charEqual x y) (charsEqual xs ys) accepted))
    (charsEqualSound xs ys (bothRight (charEqual x y) (charsEqual xs ys) accepted))

public export
charsEqualSelf : (xs : List Char) -> charsEqual xs xs = True
charsEqualSelf [] = Refl
charsEqualSelf (x :: xs) =
  rewrite charEqualSelf x in rewrite charsEqualSelf xs in Refl

public export
charsEqualComplete : (xs, ys : List Char) -> xs = ys -> charsEqual xs ys = True
charsEqualComplete xs xs Refl = charsEqualSelf xs

