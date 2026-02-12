-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafePathUnit

import Proven.Core
import Proven.SafePath

%default total

assertOk : Show e => String -> Result e a -> IO ()
assertOk name (Ok _) = putStrLn $ "  ✓ " ++ name
assertOk name (Err e) = putStrLn $ "  ✗ " ++ name ++ " (got error: " ++ show e ++ ")"

assertErr : Show a => String -> Result e a -> IO ()
assertErr name (Err _) = putStrLn $ "  ✓ " ++ name
assertErr name (Ok v) = putStrLn $ "  ✗ " ++ name ++ " (expected error, got: " ++ show v ++ ")"

assertEq : (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name expected actual =
  if expected == actual
    then putStrLn $ "  ✓ " ++ name
    else putStrLn $ "  ✗ " ++ name ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"

assertTrue : String -> Bool -> IO ()
assertTrue name True = putStrLn $ "  ✓ " ++ name
assertTrue name False = putStrLn $ "  ✗ " ++ name ++ " (expected True)"

export
runPathUnitTests : IO ()
runPathUnitTests = do
  putStrLn "SafePath Unit Tests"
  putStrLn "==================="

  -- Valid path tests
  putStrLn "\n[Valid Paths]"
  assertOk "parsePath \"/home/user/file.txt\" parses" (parsePath "/home/user/file.txt")
  assertOk "parsePath \"./relative/path\" parses" (parsePath "./relative/path")
  assertOk "parsePath \"file.txt\" parses" (parsePath "file.txt")

  -- Traversal prevention tests
  putStrLn "\n[Traversal Prevention]"
  assertErr "parsePath \"../../../etc/passwd\" blocked" (parsePath "../../../etc/passwd")
  assertErr "parsePath \"/home/../../../etc/shadow\" blocked" (parsePath "/home/../../../etc/shadow")
  assertErr "parsePath \"..\" blocked" (parsePath "..")
  assertTrue "containsTraversal \"../secret\" = True" (containsTraversal "../secret")
  assertTrue "containsTraversal \"safe/path\" = False" (not $ containsTraversal "safe/path")

  -- Component extraction tests
  putStrLn "\n[Component Extraction]"
  case parsePath "/home/user/documents/report.pdf" of
    Ok path => do
      assertEq "getDirectory = \"/home/user/documents\"" "/home/user/documents" (getDirectory path)
      assertEq "getFilename = \"report.pdf\"" "report.pdf" (getFilename path)
      assertEq "getExtension = \"pdf\"" "pdf" (getExtension path)
      assertEq "getBasename = \"report\"" "report" (getBasename path)
    Err _ => putStrLn "  ✗ Failed to parse test path"

  -- Path joining tests
  putStrLn "\n[Path Joining]"
  assertOk "joinPath \"/home/user\" \"file.txt\" works" (joinPath "/home/user" "file.txt")
  assertErr "joinPath base \"../escape\" blocked" (joinPath "/home/user" "../escape")
  assertOk "joinPath handles trailing slash" (joinPath "/home/user/" "file.txt")

  -- Normalization tests
  putStrLn "\n[Normalization]"
  assertEq "normalizePath removes double slashes"
           "/home/user/file" (normalizePath "/home//user///file")
  assertEq "normalizePath resolves . components"
           "/home/user" (normalizePath "/home/./user/.")

  -- Glob pattern tests
  putStrLn "\n[Glob Patterns]"
  assertTrue "matchGlob \"*.txt\" \"file.txt\" = True" (matchGlob "*.txt" "file.txt")
  assertTrue "matchGlob \"*.txt\" \"file.pdf\" = False" (not $ matchGlob "*.txt" "file.pdf")
  assertTrue "matchGlob \"file?.txt\" \"file1.txt\" = True" (matchGlob "file?.txt" "file1.txt")
  assertTrue "matchGlob \"**/*.rs\" \"src/main.rs\" = True" (matchGlob "**/*.rs" "src/main.rs")

  -- Security tests
  putStrLn "\n[Security]"
  assertTrue "isSafePath blocks null bytes" (not $ isSafePath "/path\x00/file")
  assertTrue "isSafePath allows normal paths" (isSafePath "/home/user/safe.txt")

  putStrLn "\n✓ SafePath unit tests complete"
