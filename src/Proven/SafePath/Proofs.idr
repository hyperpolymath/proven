-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Proofs for SafePath operations
|||
||| This module contains proofs that verify path safety properties.
||| Non-trivial proofs are declared as postulates pending formal verification.
module Proven.SafePath.Proofs

import Proven.Core
import Proven.SafePath.Types
import Proven.SafePath.Operations
import Data.List

%default total

--------------------------------------------------------------------------------
-- Normalization Properties
--------------------------------------------------------------------------------

||| Normalization is idempotent: normalizing twice is the same as once
export postulate
normalizeIdempotent : (path : String) ->
                      normalizePath (normalizePath path) = normalizePath path

||| Normalization removes empty segments from path
export postulate
normalizeRemovesEmpty : (path : String) ->
                        not ("" `elem` splitPath (normalizePath path))

||| Normalization removes single dot segments
export postulate
normalizeRemovesDot : (path : String) ->
                      not ("." `elem` splitPath (normalizePath path))

||| Normalized absolute path has no leading .. segment
export postulate
normalizeAbsNoLeadingDotDot : (path : String) ->
                              isPrefixOf "/" path = True ->
                              case splitPath (normalizePath path) of
                                (".." :: _) => Void
                                _ => ()

--------------------------------------------------------------------------------
-- Safety Properties
--------------------------------------------------------------------------------

||| Safe join prevents escape beyond base directory
public export
data NoEscape : (base : String) -> (combined : String) -> Type where
  MkNoEscape : (base : String) -> (combined : String) ->
               (prf : isPrefixOf (splitPath (normalizePath base))
                                (splitPath (normalizePath combined)) = True) ->
               NoEscape base combined

||| Safe join guarantees no escape from base
export postulate
safeJoinNoEscape : (base, rel : String) ->
                   (result : String ** safeJoinPaths base rel = Just result) ->
                   NoEscape base result

||| Sanitized segment is always safe
export postulate
sanitizedIsSafe : (seg : String) ->
                  isSafeSegment (sanitizeSegment seg) = True

||| Contained path is always within base directory
export postulate
containedInBase : (base : String) -> (cp : ContainedPath base) ->
                  isAncestorOf base (getFullPath cp) = True

--------------------------------------------------------------------------------
-- Traversal Detection Properties
--------------------------------------------------------------------------------

||| Data type for path without traversal
public export
data NoTraversal : String -> Type where
  MkNoTraversal : (path : String) ->
                  (prf : not (".." `elem` splitPath (normalizePath path)) = True) ->
                  NoTraversal path

||| Sanitized path has no traversal segments
export postulate
sanitizedNoTraversal : (path : String) ->
                       NoTraversal (normalizePath (joinSegments (map sanitizeSegment (splitPath path))))

||| Detected traversal means .. is present in segments
export postulate
traversalHasDotDot : (path : String) ->
                     (".." `elem` splitPath path = True) ->
                     any (== "..") (splitPath path) = True

--------------------------------------------------------------------------------
-- Path Comparison Properties
--------------------------------------------------------------------------------

||| Path equality is reflexive
export postulate
pathEqRefl : (path : String) -> pathEqSensitive path path = True

||| Path equality is symmetric
export postulate
pathEqSym : (p1, p2 : String) ->
            pathEqSensitive p1 p2 = pathEqSensitive p2 p1

||| Parent is always an ancestor of its child
export postulate
parentIsAncestor : (parent, child : String) ->
                   isParentOf parent child = True ->
                   isAncestorOf parent child = True

||| Ancestor relation is transitive
export postulate
ancestorTransitive : (a, b, c : String) ->
                     isAncestorOf a b = True ->
                     isAncestorOf b c = True ->
                     isAncestorOf a c = True

--------------------------------------------------------------------------------
-- Extension Properties
--------------------------------------------------------------------------------

||| Extension after changeExtension is the new extension
export postulate
changeExtensionCorrect : (path, ext : String) ->
                         not (ext == "") = True ->
                         getExtension (changeExtension path ext) = Just ext

||| stripExtension removes the extension
export postulate
stripExtensionRemoves : (path : String) ->
                        isJust (getExtension path) = True ->
                        getExtension (stripExtension path) = Nothing

||| addExtension adds the given extension as suffix
export postulate
addExtensionAdds : (path, ext : String) ->
                   isSuffixOf ("." ++ ext) (addExtension path ext) = True

--------------------------------------------------------------------------------
-- Glob Matching Properties
--------------------------------------------------------------------------------

||| Empty pattern matches empty string
public export
emptyMatchesEmpty : matchGlob "" "" = True
emptyMatchesEmpty = Refl

||| Star wildcard matches any string
export postulate
starMatchesAll : (s : String) -> matchGlob "*" s = True

||| Question mark matches any single character
export postulate
questionMatchesSingle : (c : Char) -> matchGlob "?" (singleton c) = True

||| A literal pattern (no wildcards) matches itself
export postulate
literalMatchesSelf : (s : String) ->
                     all (\c => c /= '*' && c /= '?') (unpack s) = True ->
                     matchGlob s s = True

--------------------------------------------------------------------------------
-- Validation Properties
--------------------------------------------------------------------------------

||| Valid path length is bounded by 4096
export postulate
validPathBounded : (path : String) ->
                   (vp : ValidatedPath ** validatePath path = Right vp) ->
                   length path <= 4096

||| Valid path segments are bounded by 255 characters
export postulate
validSegmentsBounded : (path : String) ->
                       (vp : ValidatedPath ** validatePath path = Right vp) ->
                       all (\seg => length seg <= 255) (splitPath path)

||| Valid path contains no null bytes
export postulate
validPathNoNull : (path : String) ->
                  (vp : ValidatedPath ** validatePath path = Right vp) ->
                  not ('\0' `elem` unpack path)

--------------------------------------------------------------------------------
-- Contained Path Properties
--------------------------------------------------------------------------------

||| Contained path full path starts with base
export postulate
containedStartsWithBase : (base : String) -> (cp : ContainedPath base) ->
                          isPrefixOf (splitPath (normalizePath base))
                                    (splitPath (normalizePath (getFullPath cp))) = True

||| Making contained path is deterministic
public export
makeContainedDeterministic : (base, rel : String) ->
                             makeContainedPath base rel = makeContainedPath base rel
makeContainedDeterministic base rel = Refl
