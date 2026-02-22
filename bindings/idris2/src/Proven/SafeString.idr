-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Safe string operations via libproven FFI.
|||
||| Provides UTF-8 validation and context-aware escaping for SQL, HTML,
||| and JavaScript. All logic is performed by the formally verified Idris 2
||| core through the precompiled shared library.
|||
||| String results allocated by libproven are copied into managed Idris
||| Strings and the C buffers are freed automatically by the wrappers.
module Proven.SafeString

import Proven.FFI

%default total

-- ============================================================================
-- Helpers
-- ============================================================================

||| Interpret a C boolean result tuple as a Maybe Bool.
||| Status 0 is success; the second element is the boolean value
||| (0 = False, nonzero = True).
boolResultToMaybe : (Int, Int) -> Maybe Bool
boolResultToMaybe (s, v) =
  if isOK s then Just (v /= 0)
  else Nothing

||| Read a C string result, copy it to a managed Idris String, and free
||| the C buffer. Returns Nothing if the status indicates an error.
|||
||| Uses prim__castPtr (from PrimIO) to cast AnyPtr to Ptr String, then
||| prim__getString (from Prelude.IO) to read the C string into a managed
||| Idris String. The C buffer is freed via prim__proven_free_string.
readAndFreeStringResult : HasIO io => (Int, AnyPtr, Int) -> io (Maybe String)
readAndFreeStringResult (status, ptr, _len) =
  if isOK status
    then do
      let str = prim__getString (prim__castPtr ptr)
      primIO $ prim__proven_free_string ptr
      pure (Just str)
    else pure Nothing

-- ============================================================================
-- UTF-8 validation
-- ============================================================================

||| Check whether a String is valid UTF-8.
|||
||| Returns `Just True` if the data is valid UTF-8, `Just False` if not,
||| or `Nothing` if the underlying library returned an error status.
||| @ str Input string to validate
public export
isValidUtf8 : HasIO io => (str : String) -> io (Maybe Bool)
isValidUtf8 str = do
  let len = cast {to=Int} (length str)
  result <- primIO $ prim__proven_string_is_valid_utf8 str len
  pure (boolResultToMaybe result)

-- ============================================================================
-- Escaping
-- ============================================================================

||| Escape a string for safe embedding in SQL (single-quote escaping).
|||
||| Returns `Just escaped` on success, `Nothing` on error.
||| Memory management is handled internally.
|||
||| Note: prefer parameterised queries over string escaping when possible.
||| @ str Input string to escape
public export
escapeSql : HasIO io => (str : String) -> io (Maybe String)
escapeSql str = do
  let len = cast {to=Int} (length str)
  result <- primIO $ prim__proven_string_escape_sql str len
  readAndFreeStringResult result

||| Escape a string for safe embedding in HTML (prevents XSS).
|||
||| Escapes `<`, `>`, `&`, `"`, and `'` to their HTML entity equivalents.
||| Returns `Just escaped` on success, `Nothing` on error.
||| @ str Input string to escape
public export
escapeHtml : HasIO io => (str : String) -> io (Maybe String)
escapeHtml str = do
  let len = cast {to=Int} (length str)
  result <- primIO $ prim__proven_string_escape_html str len
  readAndFreeStringResult result

||| Escape a string for safe embedding in JavaScript string literals.
|||
||| Returns `Just escaped` on success, `Nothing` on error.
||| @ str Input string to escape
public export
escapeJs : HasIO io => (str : String) -> io (Maybe String)
escapeJs str = do
  let len = cast {to=Int} (length str)
  result <- primIO $ prim__proven_string_escape_js str len
  readAndFreeStringResult result
