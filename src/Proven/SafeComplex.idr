-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeComplex - Safe complex number arithmetic
|||
||| This module provides complex number operations with proper handling
||| of edge cases like division by zero and overflow.
module Proven.SafeComplex
import Data.String
import Data.List

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Complex Number Type
--------------------------------------------------------------------------------

||| A complex number with real and imaginary parts
public export
record Complex a where
  constructor MkComplex
  real : a
  imag : a

||| Shorthand notation
public export
C : Type -> Type
C = Complex

||| Complex number errors
public export
data ComplexError
  = DivisionByZero
  | Overflow
  | InvalidOperation String

public export
Show ComplexError where
  show DivisionByZero = "Division by zero in complex arithmetic"
  show Overflow = "Complex number overflow"
  show (InvalidOperation s) = "Invalid complex operation: " ++ s

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

||| Create a complex number from real and imaginary parts
public export
complex : a -> a -> Complex a
complex = MkComplex

||| Create a purely real complex number
public export
fromReal : Num a => a -> Complex a
fromReal r = MkComplex r 0

||| Create a purely imaginary complex number
public export
fromImag : Num a => a -> Complex a
fromImag i = MkComplex 0 i

||| The imaginary unit i
public export
i : Num a => Complex a
i = MkComplex 0 1

||| Zero complex number
public export
zero : Num a => Complex a
zero = MkComplex 0 0

||| One as a complex number
public export
one : Num a => Complex a
one = MkComplex 1 0

||| Create from polar form (r, theta)
public export
fromPolar : (r : Double) -> (theta : Double) -> Complex Double
fromPolar r theta = MkComplex (r * cos theta) (r * sin theta)

--------------------------------------------------------------------------------
-- Basic Arithmetic
--------------------------------------------------------------------------------

||| Addition
public export
Num a => Num (Complex a) where
  (+) (MkComplex a b) (MkComplex c d) = MkComplex (a + c) (b + d)
  (*) (MkComplex a b) (MkComplex c d) = MkComplex (a * c - b * d) (a * d + b * c)
  fromInteger n = MkComplex (fromInteger n) 0

||| Negation
public export
Neg a => Neg (Complex a) where
  negate (MkComplex a b) = MkComplex (negate a) (negate b)
  (-) (MkComplex a b) (MkComplex c d) = MkComplex (a - c) (b - d)

||| Absolute value (modulus) squared - avoids sqrt
public export
Num a => absSquared : Complex a -> a
absSquared (MkComplex a b) = a * a + b * b

||| Absolute value (modulus)
public export
magnitude : Complex Double -> Double
magnitude z = sqrt (absSquared z)

||| Alias for magnitude
public export
abs : Complex Double -> Double
abs = magnitude

||| Argument (phase angle) in radians
public export
arg : Complex Double -> Double
arg (MkComplex a b) = atan2 b a

||| Conjugate
public export
Neg a => conjugate : Complex a -> Complex a
conjugate (MkComplex a b) = MkComplex a (negate b)

--------------------------------------------------------------------------------
-- Safe Division
--------------------------------------------------------------------------------

||| Safe division with explicit error handling
public export
divSafe : Complex Double -> Complex Double -> Either ComplexError (Complex Double)
divSafe num (MkComplex c d) =
  let denom = c * c + d * d
  in if denom == 0
       then Left DivisionByZero
       else let MkComplex a b = num
                realPart = (a * c + b * d) / denom
                imagPart = (b * c - a * d) / denom
            in Right (MkComplex realPart imagPart)

||| Division returning Maybe
public export
div : Complex Double -> Complex Double -> Maybe (Complex Double)
div num den =
  case divSafe num den of
    Left _ => Nothing
    Right r => Right r

||| Division with default value for zero divisor
public export
divOr : Complex Double -> Complex Double -> Complex Double -> Complex Double
divOr def num den =
  case div num den of
    Nothing => def
    Just r => r

--------------------------------------------------------------------------------
-- Exponential and Logarithm
--------------------------------------------------------------------------------

||| Exponential e^z
public export
exp : Complex Double -> Complex Double
exp (MkComplex a b) =
  let ea = exp a
  in MkComplex (ea * cos b) (ea * sin b)

||| Natural logarithm (principal value)
public export
ln : Complex Double -> Maybe (Complex Double)
ln z =
  let r = magnitude z
  in if r == 0
       then Nothing
       else Just (MkComplex (log r) (arg z))

||| Safe logarithm with error
public export
lnSafe : Complex Double -> Either ComplexError (Complex Double)
lnSafe z =
  case ln z of
    Nothing => Left (InvalidOperation "logarithm of zero")
    Just r => Right r

||| Power: z^w = e^(w * ln(z))
public export
pow : Complex Double -> Complex Double -> Maybe (Complex Double)
pow base expo = do
  logBase <- ln base
  pure (exp (expo * logBase))

--------------------------------------------------------------------------------
-- Trigonometric Functions
--------------------------------------------------------------------------------

||| Sine
public export
sin : Complex Double -> Complex Double
sin (MkComplex a b) = MkComplex (sin a * cosh b) (cos a * sinh b)
  where
    cosh : Double -> Double
    cosh x = (exp x + exp (-x)) / 2
    sinh : Double -> Double
    sinh x = (exp x - exp (-x)) / 2

||| Cosine
public export
cos : Complex Double -> Complex Double
cos (MkComplex a b) = MkComplex (cos a * cosh b) (-(sin a * sinh b))
  where
    cosh : Double -> Double
    cosh x = (exp x + exp (-x)) / 2
    sinh : Double -> Double
    sinh x = (exp x - exp (-x)) / 2

||| Tangent
public export
tan : Complex Double -> Maybe (Complex Double)
tan z = div (sin z) (cos z)

--------------------------------------------------------------------------------
-- Hyperbolic Functions
--------------------------------------------------------------------------------

||| Hyperbolic sine
public export
sinh : Complex Double -> Complex Double
sinh z =
  let ez = exp z
      emz = exp (negate z)
  in MkComplex ((ez.real - emz.real) / 2) ((ez.imag - emz.imag) / 2)

||| Hyperbolic cosine
public export
cosh : Complex Double -> Complex Double
cosh z =
  let ez = exp z
      emz = exp (negate z)
  in MkComplex ((ez.real + emz.real) / 2) ((ez.imag + emz.imag) / 2)

||| Hyperbolic tangent
public export
tanh : Complex Double -> Maybe (Complex Double)
tanh z = div (sinh z) (cosh z)

--------------------------------------------------------------------------------
-- Roots
--------------------------------------------------------------------------------

||| Square root (principal value)
public export
sqrt : Complex Double -> Complex Double
sqrt z =
  let r = magnitude z
      theta = arg z
  in fromPolar (sqrt r) (theta / 2)

||| nth root (principal value)
public export
nthRoot : (n : Nat) -> Complex Double -> Maybe (Complex Double)
nthRoot Z _ = Nothing  -- 0th root is undefined
nthRoot n z =
  let r = magnitude z
      theta = arg z
      nf = cast n
  in Just (fromPolar (pow r (1 / nf)) (theta / nf))
  where
    pow : Double -> Double -> Double
    pow b e = exp (e * log b)

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

||| Check if complex number is real (imaginary part is zero)
public export
isReal : Eq a => Num a => Complex a -> Bool
isReal (MkComplex _ b) = b == 0

||| Check if complex number is purely imaginary
public export
isImaginary : Eq a => Num a => Complex a -> Bool
isImaginary (MkComplex a _) = a == 0

||| Check if complex number is zero
public export
isZero : Eq a => Num a => Complex a -> Bool
isZero (MkComplex a b) = a == 0 && b == 0

||| Check if two complex numbers are approximately equal
public export
approxEqual : (eps : Double) -> Complex Double -> Complex Double -> Bool
approxEqual eps (MkComplex a1 b1) (MkComplex a2 b2) =
  abs (a1 - a2) < eps && abs (b1 - b2) < eps

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

||| Convert to polar form (r, theta)
public export
toPolar : Complex Double -> (Double, Double)
toPolar z = (magnitude z, arg z)

||| Extract real part
public export
getRe : Complex a -> a
getRe = real

||| Extract imaginary part
public export
getIm : Complex a -> a
getIm = imag

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Eq a => Eq (Complex a) where
  (==) (MkComplex a1 b1) (MkComplex a2 b2) = a1 == a2 && b1 == b2

public export
Show a => Ord a => Num a => Show (Complex a) where
  show (MkComplex r i) =
    if i >= 0
      then show r ++ " + " ++ show i ++ "i"
      else show r ++ " - " ++ show (abs i) ++ "i"
    where
      abs : Num a => Ord a => a -> a
      abs x = if x >= 0 then x else negate x
