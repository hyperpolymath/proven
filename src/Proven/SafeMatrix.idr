-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeMatrix - Safe matrix operations that cannot crash
|||
||| This module provides type-safe matrix operations with dimension checking.
||| Operations that could fail (like inversion) return Maybe/Either types.
module Proven.SafeMatrix
import Data.String
import Data.List

import public Proven.Core
import Data.Vect
import Data.List

%default total

--------------------------------------------------------------------------------
-- Matrix Types
--------------------------------------------------------------------------------

||| A matrix with compile-time known dimensions
||| @ rows Number of rows
||| @ cols Number of columns
public export
Matrix : (rows : Nat) -> (cols : Nat) -> Type -> Type
Matrix rows cols a = Vect rows (Vect cols a)

||| Matrix errors
public export
data MatrixError
  = DimensionMismatch
  | SingularMatrix
  | InvalidIndex Nat Nat
  | EmptyMatrix

public export
Show MatrixError where
  show DimensionMismatch = "Matrix dimension mismatch"
  show SingularMatrix = "Matrix is singular (not invertible)"
  show (InvalidIndex r c) = "Invalid index: (" ++ show r ++ ", " ++ show c ++ ")"
  show EmptyMatrix = "Empty matrix"

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

||| Create a matrix filled with a value
public export
fill : (rows : Nat) -> (cols : Nat) -> a -> Matrix rows cols a
fill rows cols val = replicate rows (replicate cols val)

||| Create a zero matrix
public export
zeros : Num a => (rows : Nat) -> (cols : Nat) -> Matrix rows cols a
zeros rows cols = fill rows cols 0

||| Create a ones matrix
public export
ones : Num a => (rows : Nat) -> (cols : Nat) -> Matrix rows cols a
ones rows cols = fill rows cols 1

||| Create an identity matrix
public export
identity : Num a => (n : Nat) -> Matrix n n a
identity n = tabulate n (\i => tabulate n (\j => if finToNat i == finToNat j then 1 else 0))
  where
    tabulate : (m : Nat) -> (Fin m -> b) -> Vect m b
    tabulate Z _ = []
    tabulate (S k) f = f FZ :: tabulate k (f . FS)

||| Create a diagonal matrix from a vector
public export
diagonal : Num a => Vect n a -> Matrix n n a
diagonal vals = zipWithIndex vals
  where
    zipWithIndex : Vect m a -> Matrix m m a
    zipWithIndex {m = Z} [] = []
    zipWithIndex {m = S k} (x :: xs) =
      (x :: replicate k 0) :: map (0 ::) (zipWithIndex xs)

--------------------------------------------------------------------------------
-- Access
--------------------------------------------------------------------------------

||| Get element at position (row, col) - compile-time bounds checked
public export
index : Matrix rows cols a -> Fin rows -> Fin cols -> a
index mat r c = index c (index r mat)

||| Get a row
public export
getRow : Matrix rows cols a -> Fin rows -> Vect cols a
getRow mat r = index r mat

||| Get a column
public export
getCol : Matrix rows cols a -> Fin cols -> Vect rows a
getCol mat c = map (\row => index c row) mat

||| Set element at position
public export
setIndex : Matrix rows cols a -> Fin rows -> Fin cols -> a -> Matrix rows cols a
setIndex mat r c val = updateAt r (\row => updateAt c (const val) row) mat

--------------------------------------------------------------------------------
-- Basic Operations
--------------------------------------------------------------------------------

||| Transpose a matrix
public export
transpose : Matrix rows cols a -> Matrix cols rows a
transpose {cols = Z} _ = []
transpose {cols = S k} mat = head <$> mat :: transpose (tail <$> mat)

||| Element-wise addition
public export
add : Num a => Matrix rows cols a -> Matrix rows cols a -> Matrix rows cols a
add = zipWith (zipWith (+))

||| Element-wise subtraction
public export
sub : Num a => Matrix rows cols a -> Matrix rows cols a -> Matrix rows cols a
sub = zipWith (zipWith (-))

||| Scalar multiplication
public export
scale : Num a => a -> Matrix rows cols a -> Matrix rows cols a
scale s = map (map (* s))

||| Element-wise multiplication (Hadamard product)
public export
hadamard : Num a => Matrix rows cols a -> Matrix rows cols a -> Matrix rows cols a
hadamard = zipWith (zipWith (*))

||| Matrix multiplication
public export
mul : Num a => Matrix m n a -> Matrix n p a -> Matrix m p a
mul a b =
  let bt = transpose b
  in map (\row => map (dot row) bt) a
  where
    dot : Num a => Vect n a -> Vect n a -> a
    dot xs ys = sum (zipWith (*) xs ys)

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

||| Get the dimensions as a pair
public export
dimensions : Matrix rows cols a -> (Nat, Nat)
dimensions {rows} {cols} _ = (rows, cols)

||| Check if matrix is square
public export
isSquare : Matrix rows cols a -> Bool
isSquare {rows} {cols} _ = rows == cols

||| Check if matrix is symmetric
public export
isSymmetric : Eq a => Matrix n n a -> Bool
isSymmetric mat = mat == transpose mat

||| Trace of a square matrix (sum of diagonal elements)
public export
trace : Num a => Matrix n n a -> a
trace mat = sum (diag mat)
  where
    diag : Matrix m m a -> Vect m a
    diag {m = Z} [] = []
    diag {m = S k} ((x :: _) :: rest) = x :: diag (map tail rest)

||| Sum of all elements
public export
total : Num a => Matrix rows cols a -> a
total = sum . map sum

||| Frobenius norm squared
public export
frobeniusNormSq : Num a => Matrix rows cols a -> a
frobeniusNormSq mat = total (hadamard mat mat)

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

||| Apply a function to all elements
public export
mapMatrix : (a -> b) -> Matrix rows cols a -> Matrix rows cols b
mapMatrix f = map (map f)

||| Flatten a matrix to a list (row-major order)
public export
flatten : Matrix rows cols a -> List a
flatten = concatMap toList

||| Negate all elements
public export
negate : Neg a => Matrix rows cols a -> Matrix rows cols a
negate = mapMatrix negate

||| Get the absolute value of all elements
public export
absMatrix : Abs a => Matrix rows cols a -> Matrix rows cols a
absMatrix = mapMatrix abs

--------------------------------------------------------------------------------
-- Row Operations (for Gaussian elimination)
--------------------------------------------------------------------------------

||| Swap two rows
public export
swapRows : Fin rows -> Fin rows -> Matrix rows cols a -> Matrix rows cols a
swapRows i j mat =
  let ri = index i mat
      rj = index j mat
  in updateAt i (const rj) (updateAt j (const ri) mat)

||| Add a scaled row to another row
public export
addScaledRow : Num a => a -> Fin rows -> Fin rows -> Matrix rows cols a -> Matrix rows cols a
addScaledRow scalar src dst mat =
  let srcRow = index src mat
  in updateAt dst (\dstRow => zipWith (+) dstRow (map (* scalar) srcRow)) mat

||| Scale a row by a factor
public export
scaleRow : Num a => a -> Fin rows -> Matrix rows cols a -> Matrix rows cols a
scaleRow scalar r mat = updateAt r (map (* scalar)) mat

--------------------------------------------------------------------------------
-- Decompositions (returning Maybe for potential failure)
--------------------------------------------------------------------------------

||| LU decomposition result
public export
record LU (n : Nat) a where
  constructor MkLU
  lower : Matrix n n a
  upper : Matrix n n a

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Show a => Show (Matrix rows cols a) where
  show mat = unlines (toList (map showRow mat))
    where
      showRow : Vect m a -> String
      showRow row = "[" ++ (concat $ intersperse ", " $ toList $ map show row) ++ "]"
