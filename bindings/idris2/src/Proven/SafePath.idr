-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Safe filesystem path operations via libproven FFI.
|||
||| Provides directory traversal detection and filename sanitisation.
||| All logic is performed by the formally verified Idris 2 core
||| through the precompiled shared library.
module Proven.SafePath

import Proven.FFI

%default total

-- ============================================================================
-- Helpers
-- ============================================================================

||| Interpret a C boolean result tuple as a Maybe Bool.
boolResultToMaybe : (Int, Int) -> Maybe Bool
boolResultToMaybe (s, v) =
  if isOK s then Just (v /= 0)
  else Nothing

||| Read a C string result, copy to managed Idris String, free C buffer.
readAndFreeStringResult : HasIO io => (Int, AnyPtr, Int) -> io (Maybe String)
readAndFreeStringResult (status, ptr, _len) =
  if isOK status
    then do
      let str = prim__getString (prim__castPtr ptr)
      primIO $ prim__proven_free_string ptr
      pure (Just str)
    else pure Nothing

-- ============================================================================
-- Traversal detection
-- ============================================================================

||| Check whether a path contains directory traversal sequences ("..").
|||
||| Returns `Just True` if traversal sequences are detected, `Just False`
||| if the path is safe, or `Nothing` on error.
||| @ path The filesystem path to check
public export
hasTraversal : HasIO io => (path : String) -> io (Maybe Bool)
hasTraversal path = do
  let len = cast {to=Int} (length path)
  result <- primIO $ prim__proven_path_has_traversal path len
  pure (boolResultToMaybe result)

-- ============================================================================
-- Filename sanitisation
-- ============================================================================

||| Sanitise a filename by removing dangerous characters.
|||
||| Strips directory separators, null bytes, and other characters that
||| could be used for path injection. Returns `Just sanitised` on success,
||| `Nothing` on error.
||| @ filename The filename to sanitise
public export
sanitizeFilename : HasIO io => (filename : String) -> io (Maybe String)
sanitizeFilename filename = do
  let len = cast {to=Int} (length filename)
  result <- primIO $ prim__proven_path_sanitize_filename filename len
  readAndFreeStringResult result
