-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeShell operations
|||
||| Verifies properties of shell command injection prevention,
||| escape correctness, and variable name validation safety.
module Proven.SafeShell.Proofs

import Proven.Core
import Proven.SafeShell
import Data.String
import Data.List

%default total

--------------------------------------------------------------------------------
-- Escape Function Properties
--------------------------------------------------------------------------------

||| escapeForShell always succeeds (returns Ok, never Err).
||| The single-quote escaping strategy is total and cannot fail.
public export
escapeAlwaysSucceeds : (s : String) -> (escaped : String ** escapeForShell s = Ok escaped)
escapeAlwaysSucceeds s =
  let escaped = "'" ++ concatMap (\c => if c == '\'' then "'\\''" else singleton c) (unpack s) ++ "'"
  in (escaped ** Refl)

||| Empty string escapes to a quoted empty string ''.
public export
escapeEmptyIsQuoted : escapeForShell "" = Ok "''"
escapeEmptyIsQuoted = Refl

--------------------------------------------------------------------------------
-- Variable Name Validation Properties
--------------------------------------------------------------------------------

||| Empty string is not a valid variable name.
public export
emptyNotValidVar : isValidVarName "" = False
emptyNotValidVar = Refl

||| Validation of a variable rejects empty input.
public export
validateEmptyVarFails : validateVariable "" = Err (InvalidVariable "")
validateEmptyVarFails = Refl

--------------------------------------------------------------------------------
-- Injection Detection Properties
--------------------------------------------------------------------------------

||| If a command passes isSafeForExecution, it has no dangerous patterns.
||| This is the definition of isSafeForExecution, proved by reflexivity.
public export
safeCommandNoDangerousPatterns : (cmd : String) ->
                                 isSafeForExecution cmd = True ->
                                 detectDangerousPatterns cmd = []
safeCommandNoDangerousPatterns cmd prf =
  -- isSafeForExecution cmd = null (detectDangerousPatterns cmd)
  -- so if True, then detectDangerousPatterns cmd = []
  case detectDangerousPatterns cmd of
    [] => Refl
    (_ :: _) impossible

||| validateCommand succeeds if and only if isSafeForExecution is True.
public export
validateCommandOk : (cmd : String) -> isSafeForExecution cmd = True ->
                    validateCommand cmd = Ok cmd
validateCommandOk cmd prf = rewrite prf in Refl

||| validateCommand fails if isSafeForExecution is False.
public export
validateCommandFail : (cmd : String) -> isSafeForExecution cmd = False ->
                      validateCommand cmd = Err (InjectionDetected cmd)
validateCommandFail cmd prf = rewrite prf in Refl

--------------------------------------------------------------------------------
-- Command Building Properties
--------------------------------------------------------------------------------

||| Building args from empty list gives empty list.
public export
buildArgsEmpty : buildArgs [] = []
buildArgsEmpty = Refl

||| safeQuote on a string without metacharacters is the identity.
||| If containsMetachars s = False, safeQuote s = s.
public export
safeQuoteNoMeta : (s : String) -> containsMetachars s = False -> safeQuote s = s
safeQuoteNoMeta s prf = rewrite prf in Refl
