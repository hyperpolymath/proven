-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafePathProps

import Proven.Core
import Proven.SafePath

%default total

||| Property: Simple path validates
prop_simplePathValid : isOk (validatePath "/home/user") = True
prop_simplePathValid = Refl

||| Property: Path with traversal fails
prop_traversalFails : isErr (validatePath "/home/../etc/passwd") = True
prop_traversalFails = Refl

||| OWED: Null byte in path fails
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_nullByteFails_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_nullByteFails : isErr (validatePath "/home/user\0malicious") = True

||| OWED: Empty path is valid (current directory)
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_emptyPathValid_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_emptyPathValid : isOk (validatePath "") = True

||| Property: Relative path validates
prop_relativePathValid : isOk (validatePath "relative/path") = True
prop_relativePathValid = Refl

||| OWED: Path normalization removes double slashes
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_normalizeDoubleSlash_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_normalizeDoubleSlash : normalizePath "/home//user" = "/home/user"

||| Property: Join paths correctly
prop_joinPaths : joinPath "/home" "user" = "/home/user"
prop_joinPaths = Refl

||| Property: Get extension extracts correctly
prop_getExtension : getExtension "file.txt" = Just "txt"
prop_getExtension = Refl

||| Property: No extension returns Nothing
prop_noExtension : getExtension "file" = Nothing
prop_noExtension = Refl

||| Test runner for path properties
export
runPathProperties : IO ()
runPathProperties = do
  putStrLn "SafePath Property Tests"
  putStrLn "======================="
  putStrLn "prop_simplePathValid: PASS (proven by type)"
  putStrLn "prop_traversalFails: PASS (proven by type)"
  putStrLn "prop_relativePathValid: PASS (proven by type)"
  putStrLn "prop_joinPaths: PASS (proven by type)"
  putStrLn "prop_getExtension: PASS (proven by type)"
  putStrLn "prop_noExtension: PASS (proven by type)"
  putStrLn ""
