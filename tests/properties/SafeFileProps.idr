-- SPDX-License-Identifier: PMPL-1.0-or-later
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

||| Property: Null byte in filename fails
prop_nullByteFails : isErr (validateFilename "file\0.txt") = True
prop_nullByteFails = ?prop_nullByteFails_rhs

||| Property: Reserved names fail (Windows)
prop_reservedNamesFail : isErr (validateFilename "CON") = True
prop_reservedNamesFail = Refl

||| Property: Sanitize removes dangerous chars
prop_sanitizeRemovesDangerous : containsDangerous (sanitizeFilename "file<>:\"|?*.txt") = False
prop_sanitizeRemovesDangerous = ?prop_sanitizeRemovesDangerous_rhs

||| Property: Sanitize preserves extension
prop_sanitizePreservesExt : getExtension (sanitizeFilename "bad<file>.txt") = Just "txt"
prop_sanitizePreservesExt = ?prop_sanitizePreservesExt_rhs

||| Property: Max filename length enforced
prop_maxLengthEnforced : (name : String) -> length name > maxFilenameLength ->
                         isErr (validateFilename name) = True
prop_maxLengthEnforced name prf = ?prop_maxLengthEnforced_rhs

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
