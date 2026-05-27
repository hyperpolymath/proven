-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeFileProps

import Proven.Core
import Proven.SafeFile

%default total

||| Property: Valid filename passes
prop_validFilename : isOk (validateFilename "document.txt") = True
prop_validFilename = Refl

||| Property: Filename with spaces passes
prop_filenameWithSpaces : isOk (validateFilename "my document.txt") = True
prop_filenameWithSpaces = Refl

||| Property: Empty filename fails
prop_emptyFilenameFails : isErr (validateFilename "") = True
prop_emptyFilenameFails = Refl

||| Property: Path separator in filename fails
prop_pathSepFails : isErr (validateFilename "path/file.txt") = True
prop_pathSepFails = Refl

||| OWED: Null byte in filename fails
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_nullByteFails_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_nullByteFails : isErr (validateFilename "file\0.txt") = True

||| Property: Reserved names fail (Windows)
prop_reservedNamesFail : isErr (validateFilename "CON") = True
prop_reservedNamesFail = Refl

||| OWED: Sanitize removes dangerous chars
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_sanitizeRemovesDangerous_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_sanitizeRemovesDangerous : containsDangerous (sanitizeFilename "file<>:\"|?*.txt") = False

||| OWED: Sanitize preserves extension
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_sanitizePreservesExt_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_sanitizePreservesExt : getExtension (sanitizeFilename "bad<file>.txt") = Just "txt"

||| OWED: Max filename length enforced
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_maxLengthEnforced_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_maxLengthEnforced : (name : String) -> length name > maxFilenameLength ->
                           isErr (validateFilename name) = True

||| Property: Hidden file detection
prop_hiddenFileDetection : isHiddenFile ".gitignore" = True
prop_hiddenFileDetection = Refl

||| Property: Non-hidden file detection
prop_nonHiddenFileDetection : isHiddenFile "readme.txt" = False
prop_nonHiddenFileDetection = Refl

||| Test runner for file properties
export
runFileProperties : IO ()
runFileProperties = do
  putStrLn "SafeFile Property Tests"
  putStrLn "======================="
  putStrLn "prop_validFilename: PASS (proven by type)"
  putStrLn "prop_filenameWithSpaces: PASS (proven by type)"
  putStrLn "prop_emptyFilenameFails: PASS (proven by type)"
  putStrLn "prop_pathSepFails: PASS (proven by type)"
  putStrLn "prop_reservedNamesFail: PASS (proven by type)"
  putStrLn "prop_hiddenFileDetection: PASS (proven by type)"
  putStrLn "prop_nonHiddenFileDetection: PASS (proven by type)"
  putStrLn ""
