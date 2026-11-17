-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeFileProps

import Proven.Core
import Proven.SafeFile
import Data.List
import Data.Maybe
import Data.String
import Decidable.Equality
import System

%default total

-- Real API: Proven.SafeFile validates *paths*, not bare filenames:
-- `validate : String -> FileResult SafePath` (default options), the bounded
-- component constructor `pathComponent : String -> Maybe PathComponent`,
-- `getFilename`/`getExtension`, and the content sanitiser
-- `sanitize : String -> String` (removes NUL bytes only). The original
-- `validateFilename`/`sanitizeFilename`/`isHiddenFile` never existed.
-- `validatePath` is `export`, so its results are checked by execution.
--
-- Not expressible against this API (reported, no counterpart):
--   * Windows reserved names ("CON", ...): no reserved-name policy exists.
--   * Hidden-file detection (".gitignore"): no such predicate exists.

--------------------------------------------------------------------------------
-- Type-level proofs
--------------------------------------------------------------------------------

private
gtNotLte : (n, m : Nat) -> n > m = True -> n <= m = False
gtNotLte Z Z prf = absurd prf
gtNotLte Z (S j) prf = absurd prf
gtNotLte (S k) Z prf = Refl
gtNotLte (S k) (S j) prf = gtNotLte k j prf

||| Max filename length enforced: any component longer than
||| `maxFilenameLength` (255) is refused by `pathComponent`, for every string.
prop_maxLengthEnforced : (c : String) ->
                         length (unpack c) > Types.maxFilenameLength = True ->
                         pathComponent c = Nothing
prop_maxLengthEnforced c gt
  with (decEq (length (unpack c) <= Types.maxFilenameLength) True)
  prop_maxLengthEnforced c gt | Yes prf = absurd (trans (sym (gtNotLte _ _ gt)) prf)
  prop_maxLengthEnforced c gt | No _ = Refl

--------------------------------------------------------------------------------
-- Outstanding obligations
--------------------------------------------------------------------------------

||| OWED: `sanitize` output never contains a NUL byte.
||| Blocked by: `sanitize = sanitizeContent`, which is `export` (opaque here),
||| and even unfolded it needs `unpack (pack xs) = xs`, which does not hold
||| definitionally for the String primitives. Names the proposition only.
public export
SanitizeRemovesNulObligation : Type
SanitizeRemovesNulObligation = (s : String) -> elem '\0' (unpack (sanitize s)) = False

||| OWED: the filename part of any path contains no path separator.
||| Blocked by: `getFilename = filename`, which is `export`, and the
||| `pack`/`unpack` round-trip above. Names the proposition only.
public export
FilenameHasNoSeparatorObligation : Type
FilenameHasNoSeparatorObligation = (p : String) -> elem '/' (unpack (getFilename p)) = False

--------------------------------------------------------------------------------
-- Execution checks
--------------------------------------------------------------------------------

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
accepted : String -> Bool
accepted p = isOk (validate p)

||| Test runner for file properties
export
runFileProperties : IO ()
runFileProperties = do
  putStrLn "SafeFile Property Tests"
  putStrLn "======================="
  check "plain filename accepted" (accepted "document.txt")
  check "filename with spaces accepted" (accepted "my document.txt")
  check "NUL byte rejected" (not (accepted "file\0.txt"))
  check "parent traversal rejected" (not (accepted "../etc/passwd"))
  check "shell metacharacters rejected"
    (all (\p => not (accepted p)) ["a;b", "a|b", "$HOME/x", "a`b`", "a\nb"])
  check "blocked system paths rejected" (not (accepted "/etc/shadow"))
  check "path longer than maxPathLength (4096) rejected"
    (not (accepted (pack (replicate 4097 'a'))) && accepted (pack (replicate 4096 'a')))
  check "pathComponent bound: 255 accepted, 256 rejected"
    (isJust (pathComponent (pack (replicate 255 'a'))) &&
     isNothing (pathComponent (pack (replicate 256 'a'))))
  check "filename of a path drops the directory"
    (getFilename "path/file.txt" == "file.txt" && getDirectory "path/file.txt" == "path")
  check "filename never contains a separator (corpus)"
    (all (\p => not (elem '/' (unpack (getFilename p))))
         ["", "/", "a/b/c", "dir/", "/abs/x.txt", "x"])
  check "sanitize removes NUL bytes"
    (sanitize "file\0.txt" == "file.txt" &&
     all (\s => not (elem '\0' (unpack (sanitize s)))) ["\0", "a\0\0b", "clean"])
  check "sanitize preserves the extension"
    (getExtension (sanitize "bad\0file.txt") == Just "txt")
  check "extension extraction"
    (getExtension "archive.tar.gz" == Just "gz" && getExtension "README" == Nothing)
  -- Suspected library defect (reported, expectation kept): `validatePath`
  -- accepts the empty path and returns a `SafePath` for "".
  check "empty path rejected" (not (accepted ""))
  putStrLn ""
