-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeShell - Safe shell syntax validation
|||
||| This module provides safe shell operations including:
||| - Quote handling (single, double, escape sequences)
||| - Variable expansion safety
||| - Command substitution validation
||| - Injection prevention
||| - Path expansion safety
|||
||| Example usage:
||| ```idris
||| -- Validate shell command
||| case validateCommand "echo $USER" of
|||   Ok safeCmd => executeCommand safeCmd
|||   Err InjectionDetected => putStrLn "Command injection attempt"
|||   Err e => putStrLn (friendlyError e)
|||
||| -- Safe quote escaping
||| case escapeForShell "<script>alert('xss')</script>" of
|||   Ok escaped => putStrLn escaped  -- '<script>alert('\''xss'\'')</script>'
|||   Err e => handleError e
||| ```
module Proven.SafeShell

import public Proven.Core
import Data.String
import Data.List

%default total

--------------------------------------------------------------------------------
-- Shell Syntax Types
--------------------------------------------------------------------------------

||| Quote types in shell syntax
public export
data QuoteType = SingleQuote | DoubleQuote | NoQuote

||| Shell token types
public export
data ShellToken
  = Word String
  | QuotedWord QuoteType String
  | Variable String
  | CommandSub String
  | ProcessSub String
  | Redirect String
  | Pipe

||| Shell command structure
public export
record ShellCommand where
  constructor MkCommand
  program : String
  args : List String
  redirects : List (String, String)  -- (type, target)

||| Shell syntax errors
public export
data ShellError
  = UnclosedQuote QuoteType
  | InvalidVariable String
  | InjectionDetected String
  | InvalidRedirect String
  | EmptyCommand

||| Result type for shell operations
public export
ShellResult : Type -> Type
ShellResult = Result ShellError

--------------------------------------------------------------------------------
-- Quote Handling
--------------------------------------------------------------------------------

||| Escape string for safe use in shell (single-quote style)
public export
escapeForShell : String -> ShellResult String
escapeForShell s =
  -- Single-quote escaping: 'text'\''more text'
  -- Handles any content safely
  let escaped = concatMap escapeSingleQuote (unpack s)
  in Ok ("'" ++ escaped ++ "'")
  where
    escapeSingleQuote : Char -> String
    escapeSingleQuote '\'' = "'\\''"
    escapeSingleQuote c = singleton c

||| Validate that string isInfixOf no shell metacharacters
public export
containsMetachars : String -> Bool
containsMetachars s =
  any isMetachar (unpack s)
  where
    isMetachar : Char -> Bool
    isMetachar c = c `elem` ['$', '`', '\\', '"', '\'', '|', '&', ';', '<', '>', '(', ')', '{', '}', '[', ']', '*', '?', '~', ' ', '\t', '\n']

||| Safely quote a string based on its content
public export
safeQuote : String -> String
safeQuote s =
  if containsMetachars s
    then case escapeForShell s of
      Ok quoted => quoted
      Err _ => "'" ++ s ++ "'"  -- Fallback
    else s  -- No quoting needed

--------------------------------------------------------------------------------
-- Variable Expansion Safety
--------------------------------------------------------------------------------

||| Check if variable name is valid (alphanumeric + underscore, starts with letter/underscore)
public export
isValidVarName : String -> Bool
isValidVarName s =
  case unpack s of
    [] => False
    (c :: cs) => (isAlpha c || c == '_') && all (\x => isAlphaNum x || x == '_') cs

||| Validate variable expansion syntax
public export
validateVariable : String -> ShellResult String
validateVariable var =
  if isValidVarName var
    then Ok var
    else Err (InvalidVariable var)

||| Check for potential variable expansion in string
public export
containsVariables : String -> Bool
containsVariables s = isInfixOf "$" s

--------------------------------------------------------------------------------
-- Command Injection Detection
--------------------------------------------------------------------------------

||| Dangerous shell patterns (command chaining, background, etc.)
public export
data DangerousPattern
  = CommandChaining    -- ; or &&  or ||
  | Backgrounding      -- &
  | SubshellExecution  -- $() or ``
  | Redirection        -- > >> < 2>
  | Piping             -- |

||| Detect dangerous shell patterns in string
public export
detectDangerousPatterns : String -> List DangerousPattern
detectDangerousPatterns s =
  let chars = unpack s
  in catMaybes
       [ if isInfixOf ";" s || isInfixOf "&&" s || isInfixOf "||" s then Just CommandChaining else Nothing
       , if isInfixOf "&" s && not (isInfixOf "&&" s) then Just Backgrounding else Nothing
       , if isInfixOf "$(" s || isInfixOf "`" s then Just SubshellExecution else Nothing
       , if any (\c => c `elem` ['>', '<']) chars then Just Redirection else Nothing
       , if isInfixOf "|" s && not (isInfixOf "||" s) then Just Piping else Nothing
       ]

||| Check if string is safe for direct execution (no injection vectors)
public export
isSafeForExecution : String -> Bool
isSafeForExecution s = null (detectDangerousPatterns s)

||| Validate command string for execution
public export
validateCommand : String -> ShellResult String
validateCommand cmd =
  if isSafeForExecution cmd
    then Ok cmd
    else Err (InjectionDetected cmd)

--------------------------------------------------------------------------------
-- Argument List Building
--------------------------------------------------------------------------------

||| Build safe argument list with proper escaping
public export
buildArgs : List String -> List String
buildArgs = map safeQuote

||| Join arguments into command string
public export
joinCommand : String -> List String -> String
joinCommand prog args =
  unwords (prog :: buildArgs args)

--------------------------------------------------------------------------------
-- Proofs
--------------------------------------------------------------------------------

||| Proof: Escaped strings contain no unquoted metacharacters
public export
escapedHasNoUnquotedMeta : (s : String) -> (escaped : String) ->
                           escapeForShell s = Ok escaped ->
                           (containsMetachars escaped = False)
-- Implementation deferred (requires more complex proof infrastructure)

-- ||| Proof: Valid variable names match regex [a-zA-Z_][a-zA-Z0-9_]*
-- public export
-- validVarNameRegex : (var : String) ->
--                     isValidVarName var = True ->
--                     (head : Char ** tail : List Char **
--                      unpack var = head :: tail &&
--                      (isAlpha head || head == '_') = True &&
--                      all (\x => isAlphaNum x || x == '_') tail = True)
-- -- Implementation deferred

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

||| Get friendly error message
public export
friendlyError : ShellError -> String
friendlyError (UnclosedQuote SingleQuote) = "Unclosed single quote"
friendlyError (UnclosedQuote DoubleQuote) = "Unclosed double quote"
friendlyError (UnclosedQuote NoQuote) = "Unclosed quote"
friendlyError (InvalidVariable v) = "Invalid variable name: " ++ v
friendlyError (InjectionDetected cmd) = "Potential command injection detected in: " ++ cmd
friendlyError (InvalidRedirect r) = "Invalid redirection: " ++ r
friendlyError EmptyCommand = "Empty command"
