-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafePathProps

import Proven.Core
import Proven.SafePath
import Data.Maybe
import System

%default total

-- The real string API (Proven.SafePath.Operations):
--   validatePath : String -> Either String ValidatedPath  (normalises; rejects
--                  empty, NUL bytes, over-long segments/paths)
--   normalizePath, safeJoinPaths : String -> String -> Maybe String,
--   getExtension : String -> Maybe String
-- and the structured `Path` API (containsTraversal, safeJoin, ...).
-- There is no `joinPath`; the safe join is `safeJoinPaths`. `validatePath`
-- does not reject `..`: it resolves it. Traversal refusal lives in
-- `safeJoinPaths`/`safeJoin` and `containsTraversal`.

||| Property: the empty path is rejected. (The original orphan expected it to
||| be valid as "current directory"; the library deliberately rejects it, in
||| line with POSIX, where an empty pathname is an error.)
prop_emptyPathRejected : validatePath "" = Left "Empty path"
prop_emptyPathRejected = Refl

||| Property: any structured path whose first segment is ".." is flagged as
||| containing traversal, for every remaining segment list.
prop_leadingDotDotIsTraversal : (abs : Bool) -> (rest : List Segment) ->
                                containsTraversal (MkPath abs (".." :: rest)) = True
prop_leadingDotDotIsTraversal abs rest = foldlStaysTrue _ (\e => Refl) rest
  where
    -- A left fold whose step keeps True fixed returns True from True.
    foldlStaysTrue : (f : Bool -> Segment -> Bool) ->
                     ((e : Segment) -> f True e = True) ->
                     (xs : List Segment) -> foldl f True xs = True
    foldlStaysTrue f keep [] = Refl
    foldlStaysTrue f keep (x :: xs) = rewrite keep x in foldlStaysTrue f keep xs

||| OWED: `safeJoinPaths base rel`, when it succeeds, yields a path whose
||| segments extend those of `normalizePath base` (no escape from base).
||| Blocked by: `splitPath`/`normalizePath` go through `unpack`/`split` on
||| String, which do not reduce on abstract input. This names the
||| proposition; it is not a witness.
public export
SafeJoinContainedObligation : Type
SafeJoinContainedObligation =
  (base, rel, full : String) -> safeJoinPaths base rel = Just full ->
  isPrefixOf (splitPath (normalizePath base)) (splitPath full) = True

||| OWED: `normalizePath` is idempotent. Blocked by: same String
||| non-reduction. This names the proposition; it is not a witness.
public export
NormalizeIdempotentObligation : Type
NormalizeIdempotentObligation =
  (s : String) -> normalizePath (normalizePath s) = normalizePath s

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
validatesTo : String -> String -> Bool
validatesTo s expected = case validatePath s of
  Right v => getPath v == expected
  Left _ => False

private
rejected : String -> Bool
rejected s = case validatePath s of
  Right _ => False
  Left _ => True

||| Test runner for path properties
export
runPathProperties : IO ()
runPathProperties = do
  putStrLn "SafePath Property Tests"
  putStrLn "======================="
  putStrLn "prop_emptyPathRejected: PASS (proven by type)"
  putStrLn "prop_leadingDotDotIsTraversal: PASS (proven by type, all paths)"
  check "simple absolute path validates unchanged"
    (validatesTo "/home/user" "/home/user")
  check "relative path validates unchanged"
    (validatesTo "relative/path" "relative/path")
  check "null byte in path rejected"
    (rejected "/home/user\0malicious")
  check "traversal escaping the base is refused by safeJoinPaths"
    (isNothing (safeJoinPaths "/home" "../etc/passwd") &&
     isNothing (safeJoinPaths "/home/user" "a/../../../etc"))
  check "validatePath resolves traversal rather than passing it through"
    (validatesTo "/home/../etc/passwd" "/etc/passwd")
  check "structured traversal detection (containsTraversal/safeJoin)"
    (containsTraversal (parsePath "/home/../etc/passwd") &&
     not (containsTraversal (parsePath "/home/user")) &&
     isNothing (safeJoin (parsePath "/home") (parsePath "../etc/passwd")))
  check "normalisation removes double slashes"
    (normalizePath "/home//user" == "/home/user")
  check "normalisation idempotent on corpus (instances of NormalizeIdempotentObligation)"
    (all (\s => normalizePath (normalizePath s) == normalizePath s)
       ["/home//user", "a/./b/../c", "/../x", "../../y", "", "/"])
  check "joining paths"
    (safeJoinPaths "/home" "user" == Just "/home/user")
  check "extension extracted"
    (getExtension "file.txt" == Just "txt" &&
     getExtension "/a/b/archive.tar.gz" == Just "gz")
  check "no extension gives Nothing"
    (isNothing (getExtension "file") && isNothing (getExtension "/a/b/file"))
  putStrLn ""
