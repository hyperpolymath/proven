-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Shell escaping functions for command injection prevention
|||
||| Provides escaping for shell arguments across different shell types
||| (POSIX sh, bash, Windows cmd, PowerShell)
module Proven.SafeCommand.Escape

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Shell Metacharacter Definitions
--------------------------------------------------------------------------------

||| Shell metacharacters that need escaping in POSIX shells
public export
shellMetaChars : List Char
shellMetaChars = ['|', '&', ';', '<', '>', '(', ')', '$', '`', '\\',
                  '"', '\'', ' ', '\t', '\n', '*', '?', '[', ']', '#',
                  '~', '=', '%', '!', '{', '}', '^']

||| Additional metacharacters for Windows cmd.exe
public export
cmdMetaChars : List Char
cmdMetaChars = ['|', '&', '<', '>', '^', '%', '!', '"', ' ', '\t',
                '(', ')', '@']

||| PowerShell-specific metacharacters
public export
powershellMetaChars : List Char
powershellMetaChars = ['$', '`', '"', '\'', '(', ')', '{', '}', '@',
                       '#', ';', '&', '|', '<', '>', ' ', '\t', '\n']

--------------------------------------------------------------------------------
-- POSIX Shell Escaping
--------------------------------------------------------------------------------

||| Escape string for safe use in POSIX shell arguments
||| Uses single quotes which prevent all interpretation
public export
escapeShellArg : String -> String
escapeShellArg s =
  if needsQuoting s
    then "'" ++ escapeInSingleQuotes s ++ "'"
    else s
  where
    needsQuoting : String -> Bool
    needsQuoting str = any (`elem` unpack str) shellMetaChars || null (unpack str)

    escapeInSingleQuotes : String -> String
    escapeInSingleQuotes str = pack (go (unpack str))
      where
        go : List Char -> List Char
        go [] = []
        go ('\'' :: cs) = unpack "'\\''" ++ go cs  -- End quote, escaped quote, start quote
        go (c :: cs) = c :: go cs

||| Escape for use in double-quoted context
||| Allows variable expansion but escapes dangerous chars
public export
escapeDoubleQuoted : String -> String
escapeDoubleQuoted s = "\"" ++ pack (go (unpack s)) ++ "\""
  where
    go : List Char -> List Char
    go [] = []
    go ('$' :: cs) = '\\' :: '$' :: go cs
    go ('`' :: cs) = '\\' :: '`' :: go cs
    go ('\\' :: cs) = '\\' :: '\\' :: go cs
    go ('"' :: cs) = '\\' :: '"' :: go cs
    go ('!' :: cs) = '\\' :: '!' :: go cs
    go (c :: cs) = c :: go cs

||| Escape using backslashes (for specific contexts)
public export
escapeBackslash : String -> String
escapeBackslash s = pack (go (unpack s))
  where
    go : List Char -> List Char
    go [] = []
    go (c :: cs) =
      if c `elem` shellMetaChars
        then '\\' :: c :: go cs
        else c :: go cs

--------------------------------------------------------------------------------
-- Windows cmd.exe Escaping
--------------------------------------------------------------------------------

||| Escape string for Windows cmd.exe
||| Uses ^ for escaping metacharacters
public export
escapeCmdArg : String -> String
escapeCmdArg s =
  if needsQuoting s
    then "\"" ++ escapeForCmd s ++ "\""
    else s
  where
    needsQuoting : String -> Bool
    needsQuoting str = any (`elem` unpack str) cmdMetaChars || null (unpack str)

    escapeForCmd : String -> String
    escapeForCmd str = pack (go (unpack str))
      where
        go : List Char -> List Char
        go [] = []
        go ('"' :: cs) = '\\' :: '"' :: go cs
        go ('\\' :: '"' :: cs) = '\\' :: '\\' :: '\\' :: '"' :: go cs
        go (c :: cs) = c :: go cs

||| Escape for cmd.exe outside quotes (using ^)
public export
escapeCmdBare : String -> String
escapeCmdBare s = pack (go (unpack s))
  where
    go : List Char -> List Char
    go [] = []
    go (c :: cs) =
      if c `elem` cmdMetaChars
        then '^' :: c :: go cs
        else c :: go cs

--------------------------------------------------------------------------------
-- PowerShell Escaping
--------------------------------------------------------------------------------

||| Escape string for PowerShell
||| Uses single quotes for literal strings
public export
escapePowerShell : String -> String
escapePowerShell s =
  if needsQuoting s
    then "'" ++ escapeInSingleQuotes s ++ "'"
    else s
  where
    needsQuoting : String -> Bool
    needsQuoting str = any (`elem` unpack str) powershellMetaChars || null (unpack str)

    escapeInSingleQuotes : String -> String
    escapeInSingleQuotes str = pack (go (unpack str))
      where
        go : List Char -> List Char
        go [] = []
        go ('\'' :: cs) = '\'' :: '\'' :: go cs  -- Double single quotes
        go (c :: cs) = c :: go cs

||| Escape for PowerShell double-quoted context
public export
escapePowerShellDouble : String -> String
escapePowerShellDouble s = "\"" ++ pack (go (unpack s)) ++ "\""
  where
    go : List Char -> List Char
    go [] = []
    go ('$' :: cs) = '`' :: '$' :: go cs
    go ('`' :: cs) = '`' :: '`' :: go cs
    go ('"' :: cs) = '`' :: '"' :: go cs
    go (c :: cs) = c :: go cs

--------------------------------------------------------------------------------
-- Environment Variable Escaping
--------------------------------------------------------------------------------

||| Escape environment variable name
||| Names can only contain alphanumeric and underscore, must start with letter/_
public export
escapeEnvName : String -> Maybe String
escapeEnvName s = case strM s of
  StrNil => Nothing
  StrCons c rest =>
    if (isAlpha c || c == '_') && all isValidEnvChar (unpack rest)
      then Just s
      else Nothing
  where
    isValidEnvChar : Char -> Bool
    isValidEnvChar x = isAlphaNum x || x == '_'

||| Escape environment variable value for shell
public export
escapeEnvValue : String -> String
escapeEnvValue = escapeShellArg

--------------------------------------------------------------------------------
-- Argument Array Escaping
--------------------------------------------------------------------------------

||| Escape list of arguments for shell command line
public export
escapeArgs : List String -> String
escapeArgs args = unwords (map escapeShellArg args)

||| Escape for Windows command line
public export
escapeCmdArgs : List String -> String
escapeCmdArgs args = unwords (map escapeCmdArg args)

||| Escape for PowerShell command line
public export
escapePowerShellArgs : List String -> String
escapePowerShellArgs args = unwords (map escapePowerShell args)

--------------------------------------------------------------------------------
-- Path Escaping
--------------------------------------------------------------------------------

||| Escape path for shell use
||| Handles spaces and special characters in paths
public export
escapePath : String -> String
escapePath = escapeShellArg

||| Escape path for Windows
public export
escapeWindowsPath : String -> String
escapeWindowsPath s =
  if any (== ' ') (unpack s) || any (`elem` unpack s) cmdMetaChars
    then "\"" ++ s ++ "\""
    else s

--------------------------------------------------------------------------------
-- URL/Argument Escaping for Commands
--------------------------------------------------------------------------------

||| Characters safe in URLs (don't need shell escaping beyond normal)
public export
urlSafeChars : List Char
urlSafeChars = unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:/?#[]@!$&'()*+,;=%"

||| Check if URL needs escaping for shell
public export
urlNeedsEscaping : String -> Bool
urlNeedsEscaping url = any (\c => not (c `elem` urlSafeChars)) (unpack url)

||| Escape URL for use as shell argument
public export
escapeUrlArg : String -> String
escapeUrlArg url =
  if urlNeedsEscaping url
    then escapeShellArg url
    else url

--------------------------------------------------------------------------------
-- Dangerous Pattern Detection
--------------------------------------------------------------------------------

||| Patterns that indicate potential command injection attempts
public export
dangerousPatterns : List String
dangerousPatterns = ["$(", "`", "&&", "||", ";", "|", "<(", ">(",
                     ">/", ">>/", "\n", "\r", "\x00"]

||| Check if string contains dangerous patterns
public export
containsDangerousPattern : String -> Bool
containsDangerousPattern s = any (\p => isInfixOf p s) dangerousPatterns

||| Check if string looks like command substitution
public export
looksLikeSubstitution : String -> Bool
looksLikeSubstitution s =
  isInfixOf "$(" s || isInfixOf "`" s || isInfixOf "${" s

||| Check if string looks like glob pattern
public export
looksLikeGlob : String -> Bool
looksLikeGlob s = any (`elem` unpack s) ['*', '?', '[', ']']

--------------------------------------------------------------------------------
-- Safe Unescaping (for display)
--------------------------------------------------------------------------------

||| Remove shell escaping for display (NOT for execution!)
||| Only for showing users what the command looks like
public export
unescapeForDisplay : String -> String
unescapeForDisplay s = case strM s of
  StrNil => ""
  StrCons '\'' rest =>
    -- Single quoted string
    pack (takeWhile (/= '\'') (unpack rest))
  StrCons '"' rest =>
    -- Double quoted string
    pack (takeWhile (/= '"') (unpack rest))
  _ => s
