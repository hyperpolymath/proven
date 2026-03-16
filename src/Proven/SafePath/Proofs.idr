-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafePath operations
|||
||| This module contains proofs that verify path safety properties.
||| Non-trivial proofs are declared as postulates pending formal verification.
module Proven.SafePath.Proofs

import Proven.Core
import Proven.SafePath.Types
import Proven.SafePath.Operations
import Data.List
import Data.Maybe

%default total

--------------------------------------------------------------------------------
-- Normalization Properties
--------------------------------------------------------------------------------

||| Normalization is idempotent: normalizing twice is the same as once
export
normalizeIdempotent : (path : String) ->
                      normalizePath (normalizePath path) = normalizePath path

||| Normalization removes empty segments from path
export
normalizeRemovesEmpty : (path : String) ->
                        not ("" `elem` splitPath (normalizePath path)) = True

||| Normalization removes single dot segments
export
normalizeRemovesDot : (path : String) ->
                      not ("." `elem` splitPath (normalizePath path)) = True

||| Normalized absolute path has no leading .. segment
export
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
export
safeJoinNoEscape : (base, rel : String) ->
                   (result : String ** safeJoinPaths base rel = Just result) ->
                   NoEscape base result

||| Sanitized segment is always safe
export
sanitizedIsSafe : (seg : String) ->
                  isSafeSegment (sanitizeSegment seg) = True

||| Contained path is always within base directory
export
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
export
sanitizedNoTraversal : (path : String) ->
                       NoTraversal (normalizePath (joinSegments (map sanitizeSegment (splitPath path))))

||| Detected traversal means .. is present in segments
export
traversalHasDotDot : (path : String) ->
                     (".." `elem` splitPath path = True) ->
                     any (== "..") (splitPath path) = True

--------------------------------------------------------------------------------
-- Path Comparison Properties
--------------------------------------------------------------------------------

||| Path equality is reflexive
export
pathEqRefl : (path : String) -> pathEqSensitive path path = True

||| Path equality is symmetric
export
pathEqSym : (p1, p2 : String) ->
            pathEqSensitive p1 p2 = pathEqSensitive p2 p1

||| Parent is always an ancestor of its child
export
parentIsAncestor : (parent, child : String) ->
                   isParentOf parent child = True ->
                   isAncestorOf parent child = True

||| Ancestor relation is transitive
export
ancestorTransitive : (a, b, c : String) ->
                     isAncestorOf a b = True ->
                     isAncestorOf b c = True ->
                     isAncestorOf a c = True

--------------------------------------------------------------------------------
-- Extension Properties
--------------------------------------------------------------------------------

||| Extension after changeExtension is the new extension
export
changeExtensionCorrect : (path, ext : String) ->
                         not (ext == "") = True ->
                         getExtension (changeExtension path ext) = Just ext

||| stripExtension removes the extension
export
stripExtensionRemoves : (path : String) ->
                        Data.Maybe.isJust (getExtension path) = True ->
                        getExtension (stripExtension path) = Nothing

||| addExtension adds the given extension as suffix
export
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
export
starMatchesAll : (s : String) -> matchGlob "*" s = True

||| Question mark matches any single character
export
questionMatchesSingle : (c : Char) -> matchGlob "?" (singleton c) = True

||| A literal pattern (no wildcards) matches itself
export
literalMatchesSelf : (s : String) ->
                     all (\c => c /= '*' && c /= '?') (unpack s) = True ->
                     matchGlob s s = True

--------------------------------------------------------------------------------
-- Validation Properties
--------------------------------------------------------------------------------

||| Valid path length is bounded by 4096
export
validPathBounded : (path : String) ->
                   (vp : ValidatedPath ** validatePath path = Right vp) ->
                   Prelude.String.length path <= 4096 = True

||| Valid path segments are bounded by 255 characters
export
validSegmentsBounded : (path : String) ->
                       (vp : ValidatedPath ** validatePath path = Right vp) ->
                       all (\seg => Prelude.String.length seg <= 255) (splitPath path) = True

||| Valid path contains no null bytes
export
validPathNoNull : (path : String) ->
                  (vp : ValidatedPath ** validatePath path = Right vp) ->
                  not ('\0' `elem` unpack path) = True

--------------------------------------------------------------------------------
-- Contained Path Properties
--------------------------------------------------------------------------------

||| Contained path full path starts with base
export
containedStartsWithBase : (base : String) -> (cp : ContainedPath base) ->
                          isPrefixOf (splitPath (normalizePath base))
                                    (splitPath (normalizePath (getFullPath cp))) = True

||| Making contained path is deterministic
public export
makeContainedDeterministic : (base, rel : String) ->
                             makeContainedPath base rel = makeContainedPath base rel
makeContainedDeterministic base rel = Refl
