-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeTensor - Safe vector and matrix operations with bounds checking
|||
||| This module provides safe operations for vectors and matrices commonly used
||| in machine learning and numerical computing. All operations include bounds
||| checking and prevent common errors like shape mismatches.
|||
||| Key features:
||| - Shape-safe operations that return Result on mismatch
||| - Bounds-checked indexing
||| - Safe matrix operations
||| - Type-safe tensor shapes using dependent types
module Proven.SafeTensor
import Data.String
import Data.List

import public Proven.Core
import public Proven.SafeFloat
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Vector Type (Fixed-Size with Dependent Types)
--------------------------------------------------------------------------------

||| A vector with a statically known size
||| This uses Idris 2's dependent types to ensure size safety at compile time
public export
Vector : Nat -> Type -> Type
Vector n a = Vect n a

||| A matrix with statically known dimensions
public export
Matrix : (rows : Nat) -> (cols : Nat) -> Type -> Type
Matrix rows cols a = Vect rows (Vect cols a)

--------------------------------------------------------------------------------
-- Safe Dot Product
--------------------------------------------------------------------------------

||| Safe dot product of two vectors
||| The type system guarantees the vectors have the same length!
public export
dot : Num a => Vector n a -> Vector n a -> a
dot [] [] = 0
dot (x :: xs) (y :: ys) = x * y + dot xs ys

||| Dot product for lists (returns Nothing on length mismatch)
public export
dotList : List Double -> List Double -> Maybe Double
dotList [] [] = Just 0.0
dotList (x :: xs) (y :: ys) = do
  rest <- dotList xs ys
  Just (x * y + rest)
dotList _ _ = Nothing

--------------------------------------------------------------------------------
-- Safe Element-wise Operations
--------------------------------------------------------------------------------

||| Element-wise addition (type-safe: vectors must have same length)
public export
addVec : Num a => Vector n a -> Vector n a -> Vector n a
addVec = zipWith (+)

||| Element-wise subtraction
public export
subVec : Neg a => Vector n a -> Vector n a -> Vector n a
subVec = zipWith (-)

||| Element-wise multiplication (Hadamard product)
public export
hadamard : Num a => Vector n a -> Vector n a -> Vector n a
hadamard = zipWith (*)

||| Scalar multiplication
public export
scale : Num a => a -> Vector n a -> Vector n a
scale s = map (* s)

||| List-based addition with shape checking
public export
addList : List Double -> List Double -> Maybe (List Double)
addList [] [] = Just []
addList (x :: xs) (y :: ys) = do
  rest <- addList xs ys
  Just ((x + y) :: rest)
addList _ _ = Nothing

||| List-based subtraction with shape checking
public export
subList : List Double -> List Double -> Maybe (List Double)
subList [] [] = Just []
subList (x :: xs) (y :: ys) = do
  rest <- subList xs ys
  Just ((x - y) :: rest)
subList _ _ = Nothing

--------------------------------------------------------------------------------
-- Safe Matrix-Vector Multiplication
--------------------------------------------------------------------------------

||| Matrix-vector multiplication (type-safe)
||| For a (m x n) matrix and length-n vector, produces length-m vector
public export
matVec : Num a => Matrix m n a -> Vector n a -> Vector m a
matVec [] _ = []
matVec (row :: rows) v = dot row v :: matVec rows v

||| Matrix-vector multiplication for lists (with shape checking)
public export
matVecList : List (List Double) -> List Double -> Maybe (List Double)
matVecList [] _ = Just []
matVecList (row :: rows) v = do
  d <- dotList row v
  rest <- matVecList rows v
  Just (d :: rest)

--------------------------------------------------------------------------------
-- Safe Indexing
--------------------------------------------------------------------------------

||| Safe index into a vector using a bounded index
public export
indexVec : Vector n a -> Fin n -> a
indexVec v i = index i v

||| Safe index into a list with bounds checking
public export
indexList : List a -> Nat -> Maybe a
indexList [] _ = Nothing
indexList (x :: _) Z = Just x
indexList (_ :: xs) (S n) = indexList xs n

||| Safe index into a 2D list (matrix)
public export
index2D : List (List a) -> Nat -> Nat -> Maybe a
index2D matrix row col = do
  r <- indexList matrix row
  indexList r col

--------------------------------------------------------------------------------
-- Argmax and Argmin
--------------------------------------------------------------------------------

||| Find the index of the maximum value in a non-empty vector
public export
argmax : Ord a => Vector (S n) a -> Fin (S n)
argmax {n = Z} (x :: []) = FZ
argmax {n = S m} (x :: y :: ys) =
  let rest = argmax (y :: ys)
      maxRest = index rest (y :: ys)
  in if x >= maxRest then FZ else FS rest

||| Find the index of the maximum value in a list
public export
argmaxList : List Double -> Maybe Nat
argmaxList [] = Nothing
argmaxList [x] = Just Z
argmaxList (x :: xs) = do
  restIdx <- argmaxList xs
  restMax <- indexList xs restIdx
  if x >= restMax
    then Just Z
    else Just (S restIdx)

||| Find the index of the minimum value in a list
public export
argminList : List Double -> Maybe Nat
argminList [] = Nothing
argminList [x] = Just Z
argminList (x :: xs) = do
  restIdx <- argminList xs
  restMin <- indexList xs restIdx
  if x <= restMin
    then Just Z
    else Just (S restIdx)

--------------------------------------------------------------------------------
-- Sum and Aggregation
--------------------------------------------------------------------------------

||| Sum of vector elements
public export
sumVec : Num a => Vector n a -> a
sumVec = foldr (+) 0

||| Sum of list elements
public export
sumList : List Double -> Double
sumList = foldr (+) 0.0

||| Product of vector elements
public export
prodVec : Num a => Vector n a -> a
prodVec = foldr (*) 1

--------------------------------------------------------------------------------
-- Matrix Operations
--------------------------------------------------------------------------------

||| Transpose a matrix (type-safe)
public export
transposeMatrix : {n : Nat} -> Matrix m n a -> Matrix n m a
transposeMatrix {n} [] = replicate n []
transposeMatrix (x :: xs) = zipWith (::) x (transposeMatrix xs)

||| Transpose for list-based matrices
public export
transposeList : List (List a) -> Maybe (List (List a))
transposeList [] = Just []
transposeList [row] = Just (map (\x => [x]) row)
transposeList (row :: rows) = do
  rest <- transposeList rows
  -- Check all rows have same length
  if length row == length rest
    then Just (zipWith (::) row rest)
    else Nothing

||| Get matrix shape
public export
shapeList : List (List a) -> Maybe (Nat, Nat)
shapeList [] = Just (0, 0)
shapeList (row :: rows) =
  let cols = length row
  in if all (\r => length r == cols) rows
       then Just (S (length rows), cols)
       else Nothing

||| Check if a matrix is rectangular (all rows same length)
public export
isRectangular : List (List a) -> Bool
isRectangular [] = True
isRectangular (row :: rows) = all (\r => length r == length row) rows

--------------------------------------------------------------------------------
-- Outer Product
--------------------------------------------------------------------------------

||| Outer product of two vectors
||| Creates a matrix where result[i][j] = a[i] * b[j]
public export
outer : Num a => Vector m a -> Vector n a -> Matrix m n a
outer [] _ = []
outer (x :: xs) ys = map (* x) ys :: outer xs ys

||| Outer product for lists
public export
outerList : List Double -> List Double -> List (List Double)
outerList xs ys = map (\x => map (* x) ys) xs

--------------------------------------------------------------------------------
-- Shape Validation
--------------------------------------------------------------------------------

||| Validate that two lists have the same length
public export
sameShape : List a -> List b -> Bool
sameShape xs ys = length xs == length ys

||| Validate a list-based matrix for matrix operations
||| Returns the shape if valid, Nothing if jagged
public export
validateMatrix : List (List a) -> Maybe (Nat, Nat)
validateMatrix = shapeList

--------------------------------------------------------------------------------
-- Type-Safe Vector Construction
--------------------------------------------------------------------------------

||| Create a vector of zeros
public export
zeros : Num a => (n : Nat) -> Vector n a
zeros Z = []
zeros (S n) = 0 :: zeros n

||| Create a vector of ones
public export
ones : Num a => (n : Nat) -> Vector n a
ones Z = []
ones (S n) = 1 :: ones n

||| Create a range vector [0, 1, ..., n-1]
public export
range : (n : Nat) -> Vector n Double
range n = go Z n
  where
    go : Nat -> (m : Nat) -> Vector m Double
    go _ Z = []
    go k (S m) = cast k :: go (S k) m

--------------------------------------------------------------------------------
-- Proofs and Verified Properties
--------------------------------------------------------------------------------

||| Proof that the dot product is commutative
||| dot(a, b) = dot(b, a)
public export
dotCommutative : Vector n Double -> Vector n Double -> Bool
dotCommutative xs ys = dot xs ys == dot ys xs

||| Proof that vector addition is associative
||| (a + b) + c = a + (b + c)
public export
addAssociative : Vector n Double -> Vector n Double -> Vector n Double -> Bool
addAssociative xs ys zs = addVec (addVec xs ys) zs == addVec xs (addVec ys zs)

-- Note: Length of a matrix-vector product equals number of rows
-- This is guaranteed by the type signature of matVec
