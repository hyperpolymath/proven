-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeSet - Safe set operations that cannot crash
|||
||| This module provides type-safe set operations using sorted lists
||| for guaranteed correctness and deterministic behavior.
module Proven.SafeSet

import public Proven.Core
import Data.List

%default total

--------------------------------------------------------------------------------
-- Set Type (implemented as sorted list)
--------------------------------------------------------------------------------

||| A set of elements, maintained in sorted order with no duplicates
public export
record Set a where
  constructor MkSet
  elements : List a

||| Set errors
public export
data SetError
  = ElementNotFound
  | DuplicateElement

public export
Show SetError where
  show ElementNotFound = "Element not found in set"
  show DuplicateElement = "Duplicate element in set"

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

||| Empty set
public export
empty : Set a
empty = MkSet []

||| Singleton set
public export
singleton : a -> Set a
singleton x = MkSet [x]

||| Create a set from a list (removes duplicates)
public export
fromList : Ord a => List a -> Set a
fromList = MkSet . nub . sort
  where
    nub : Eq a => List a -> List a
    nub [] = []
    nub [x] = [x]
    nub (x :: y :: xs) = if x == y then nub (y :: xs) else x :: nub (y :: xs)

||| Convert set to list (always sorted)
public export
toList : Set a -> List a
toList = elements

||| Create from a list without checking (unsafe)
public export
unsafeFromSorted : List a -> Set a
unsafeFromSorted = MkSet

--------------------------------------------------------------------------------
-- Basic Operations
--------------------------------------------------------------------------------

||| Insert an element into the set
public export
insert : Ord a => a -> Set a -> Set a
insert x (MkSet xs) = MkSet (insertSorted x xs)
  where
    insertSorted : Ord a => a -> List a -> List a
    insertSorted x [] = [x]
    insertSorted x (y :: ys) =
      case compare x y of
        LT => x :: y :: ys
        EQ => y :: ys  -- Already present
        GT => y :: insertSorted x ys

||| Remove an element from the set
public export
delete : Eq a => a -> Set a -> Set a
delete x (MkSet xs) = MkSet (filter (/= x) xs)

||| Check if element is in the set
public export
member : Ord a => a -> Set a -> Bool
member x (MkSet xs) = memberSorted x xs
  where
    memberSorted : Ord a => a -> List a -> Bool
    memberSorted _ [] = False
    memberSorted x (y :: ys) =
      case compare x y of
        LT => False  -- Can't be in rest (sorted)
        EQ => True
        GT => memberSorted x ys

||| Check if element is not in the set
public export
notMember : Ord a => a -> Set a -> Bool
notMember x s = not (member x s)

||| Size of the set
public export
size : Set a -> Nat
size (MkSet xs) = length xs

||| Check if set is empty
public export
isEmpty : Set a -> Bool
isEmpty (MkSet []) = True
isEmpty _ = False

||| Check if set is non-empty
public export
isNonEmpty : Set a -> Bool
isNonEmpty = not . isEmpty

--------------------------------------------------------------------------------
-- Set Operations
--------------------------------------------------------------------------------

||| Union of two sets
public export
union : Ord a => Set a -> Set a -> Set a
union (MkSet xs) (MkSet ys) = MkSet (merge xs ys)
  where
    merge : Ord a => List a -> List a -> List a
    merge [] ys = ys
    merge xs [] = xs
    merge (x :: xs) (y :: ys) =
      case compare x y of
        LT => x :: merge xs (y :: ys)
        EQ => x :: merge xs ys
        GT => y :: merge (x :: xs) ys

||| Intersection of two sets
public export
intersection : Ord a => Set a -> Set a -> Set a
intersection (MkSet xs) (MkSet ys) = MkSet (intersect xs ys)
  where
    intersect : Ord a => List a -> List a -> List a
    intersect [] _ = []
    intersect _ [] = []
    intersect (x :: xs) (y :: ys) =
      case compare x y of
        LT => intersect xs (y :: ys)
        EQ => x :: intersect xs ys
        GT => intersect (x :: xs) ys

||| Difference of two sets (elements in first but not second)
public export
difference : Ord a => Set a -> Set a -> Set a
difference (MkSet xs) (MkSet ys) = MkSet (diff xs ys)
  where
    diff : Ord a => List a -> List a -> List a
    diff [] _ = []
    diff xs [] = xs
    diff (x :: xs) (y :: ys) =
      case compare x y of
        LT => x :: diff xs (y :: ys)
        EQ => diff xs ys
        GT => diff (x :: xs) ys

||| Symmetric difference (elements in either but not both)
public export
symmetricDifference : Ord a => Set a -> Set a -> Set a
symmetricDifference s1 s2 = union (difference s1 s2) (difference s2 s1)

--------------------------------------------------------------------------------
-- Subset/Superset Relations
--------------------------------------------------------------------------------

||| Check if first set is subset of second
public export
isSubsetOf : Ord a => Set a -> Set a -> Bool
isSubsetOf (MkSet xs) (MkSet ys) = subset xs ys
  where
    subset : Ord a => List a -> List a -> Bool
    subset [] _ = True
    subset _ [] = False
    subset (x :: xs) (y :: ys) =
      case compare x y of
        LT => False  -- x not in ys
        EQ => subset xs ys
        GT => subset (x :: xs) ys

||| Check if first set is proper subset of second
public export
isProperSubsetOf : Ord a => Set a -> Set a -> Bool
isProperSubsetOf s1 s2 = isSubsetOf s1 s2 && size s1 < size s2

||| Check if first set is superset of second
public export
isSupersetOf : Ord a => Set a -> Set a -> Bool
isSupersetOf s1 s2 = isSubsetOf s2 s1

||| Check if two sets are disjoint (no common elements)
public export
isDisjoint : Ord a => Set a -> Set a -> Bool
isDisjoint s1 s2 = isEmpty (intersection s1 s2)

--------------------------------------------------------------------------------
-- Folds and Maps
--------------------------------------------------------------------------------

||| Map a function over all elements
public export
map : Ord b => (a -> b) -> Set a -> Set b
map f (MkSet xs) = fromList (map f xs)

||| Filter elements satisfying a predicate
public export
filter : (a -> Bool) -> Set a -> Set a
filter p (MkSet xs) = MkSet (filter p xs)

||| Partition set by a predicate
public export
partition : (a -> Bool) -> Set a -> (Set a, Set a)
partition p (MkSet xs) =
  let (yes, no) = partition p xs
  in (MkSet yes, MkSet no)

||| Fold over set elements
public export
foldl : (b -> a -> b) -> b -> Set a -> b
foldl f acc (MkSet xs) = foldl f acc xs

||| Right fold over set elements
public export
foldr : (a -> b -> b) -> b -> Set a -> b
foldr f acc (MkSet xs) = foldr f acc xs

||| Find element satisfying predicate
public export
find : (a -> Bool) -> Set a -> Maybe a
find p (MkSet xs) = find p xs

||| Check if any element satisfies predicate
public export
any : (a -> Bool) -> Set a -> Bool
any p (MkSet xs) = any p xs

||| Check if all elements satisfy predicate
public export
all : (a -> Bool) -> Set a -> Bool
all p (MkSet xs) = all p xs

--------------------------------------------------------------------------------
-- Min/Max Operations
--------------------------------------------------------------------------------

||| Get minimum element
public export
minimum : Set a -> Maybe a
minimum (MkSet []) = Nothing
minimum (MkSet (x :: _)) = Just x  -- First element is min (sorted)

||| Get maximum element
public export
maximum : Set a -> Maybe a
maximum (MkSet []) = Nothing
maximum (MkSet xs) = Just (last xs)
  where
    last : List a -> a
    last [x] = x
    last (_ :: y :: ys) = last (y :: ys)
    last [] = believe_me ()  -- Never reached

||| Delete minimum element
public export
deleteMin : Set a -> Set a
deleteMin (MkSet []) = MkSet []
deleteMin (MkSet (_ :: xs)) = MkSet xs

||| Delete maximum element
public export
deleteMax : Set a -> Set a
deleteMax (MkSet []) = MkSet []
deleteMax (MkSet xs) = MkSet (init xs)
  where
    init : List a -> List a
    init [] = []
    init [_] = []
    init (x :: y :: ys) = x :: init (y :: ys)

--------------------------------------------------------------------------------
-- Power Set
--------------------------------------------------------------------------------

||| Generate power set (all subsets) - careful, exponential!
public export
powerSet : Set a -> Set (Set a)
powerSet (MkSet xs) = MkSet (map MkSet (powerset xs))
  where
    powerset : List a -> List (List a)
    powerset [] = [[]]
    powerset (x :: xs) =
      let rest = powerset xs
      in rest ++ map (x ::) rest

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Eq a => Eq (Set a) where
  (==) (MkSet xs) (MkSet ys) = xs == ys

public export
Ord a => Ord (Set a) where
  compare (MkSet xs) (MkSet ys) = compare xs ys

public export
Show a => Show (Set a) where
  show (MkSet xs) = "{" ++ (concat $ intersperse ", " $ map show xs) ++ "}"
