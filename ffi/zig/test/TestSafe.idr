-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- TestSafe.idr - Simple Idris 2 module for FFI testing
-- Compile with: idris2 --codegen refc -o testsafe TestSafe.idr

module TestSafe

-- Safe division that returns Maybe
public export
safeDiv : Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- Safe modulo that returns Maybe
public export
safeMod : Int -> Int -> Maybe Int
safeMod _ 0 = Nothing
safeMod x y = Just (x `mod` y)

-- Checked addition using Either for error reporting
public export
data MathError = Overflow | Underflow | DivByZero

public export
checkedAdd : Int -> Int -> Either MathError Int
checkedAdd x y =
  let result = x + y
  in if x > 0 && y > 0 && result < 0
     then Left Overflow
     else if x < 0 && y < 0 && result > 0
          then Left Underflow
          else Right result

-- String validation
public export
isValidEmail : String -> Bool
isValidEmail s =
  let chars = unpack s
  in hasAtSign chars && hasLocalPart chars && hasDomain chars
  where
    hasAtSign : List Char -> Bool
    hasAtSign cs = '@' `elem` cs

    hasLocalPart : List Char -> Bool
    hasLocalPart cs = case break (== '@') cs of
      (local, _) => length local > 0

    hasDomain : List Char -> Bool
    hasDomain cs = case break (== '@') cs of
      (_, '@' :: domain) => length domain > 0 && '.' `elem` domain
      _ => False

-- Path traversal check
public export
hasPathTraversal : String -> Bool
hasPathTraversal s = isInfixOf ".." s

-- Simple identity function for testing FFI marshalling
public export
identity : Int -> Int
identity x = x

-- String length (for testing string marshalling)
public export
stringLen : String -> Int
stringLen s = cast (length s)

-- FFI Exports for C ABI
%foreign "C:testsafe_div,libidris2_testsafe"
prim__safeDiv : Int -> Int -> Int

%foreign "C:testsafe_identity,libidris2_testsafe"
prim__identity : Int -> Int

-- Main for testing
main : IO ()
main = do
  putStrLn "Testing safeDiv:"
  printLn (safeDiv 10 2)   -- Just 5
  printLn (safeDiv 10 0)   -- Nothing

  putStrLn "\nTesting checkedAdd:"
  printLn (checkedAdd 5 3)  -- Right 8

  putStrLn "\nTesting isValidEmail:"
  printLn (isValidEmail "user@example.com")  -- True
  printLn (isValidEmail "invalid")           -- False

  putStrLn "\nTesting hasPathTraversal:"
  printLn (hasPathTraversal "../etc/passwd")  -- True
  printLn (hasPathTraversal "safe/path")      -- False
