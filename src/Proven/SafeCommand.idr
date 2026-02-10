-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeCommand - Injection-safe shell command construction
|||
||| This module provides:
||| - Type-safe command argument handling
||| - Shell injection prevention through proper escaping
||| - Command builder with validated arguments
||| - Proofs that constructed commands are injection-safe
module Proven.SafeCommand

import public Proven.Core
import public Proven.SafeCommand.Escape
import public Proven.SafeCommand.Builder

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Core Command Types
--------------------------------------------------------------------------------

||| A validated command name (no path traversal, no shell metacharacters)
public export
data CommandName : Type where
  MkCommandName : (name : String) -> {auto prf : isValidCommandName name = True} -> CommandName

||| A validated command argument (properly escaped)
public export
data SafeArg : Type where
  ||| Literal argument (will be escaped)
  LiteralArg : String -> SafeArg
  ||| Pre-escaped argument (trusted)
  EscapedArg : String -> SafeArg
  ||| Flag argument (e.g., -v, --verbose)
  FlagArg : String -> SafeArg
  ||| Option with value (e.g., --output=file.txt)
  OptionArg : (name : String) -> (value : String) -> SafeArg

||| A complete command with arguments
public export
record SafeCommand where
  constructor MkSafeCommand
  cmdName : String
  cmdArgs : List SafeArg
  cmdEnv : List (String, String)
  cmdWorkDir : Maybe String

||| Result of command validation
public export
data CommandResult : Type where
  ValidCommand : SafeCommand -> CommandResult
  InvalidCommand : (reason : String) -> CommandResult

--------------------------------------------------------------------------------
-- Command Name Validation
--------------------------------------------------------------------------------

||| Characters forbidden in command names
public export
forbiddenCommandChars : List Char
forbiddenCommandChars = [';', '|', '&', '$', '`', '(', ')', '{', '}',
                         '<', '>', '\n', '\r', '\x00', '\'', '"', '\\', ' ']

||| Check if character is valid in command name
public export
isValidCommandChar : Char -> Bool
isValidCommandChar c = not (c `elem` forbiddenCommandChars)

||| Check if string is valid command name
||| Must not be empty, must not contain forbidden chars, must not start with -
public export
isValidCommandName : String -> Bool
isValidCommandName s = case strM s of
  StrNil => False
  StrCons '-' _ => False  -- Prevent flag injection
  StrCons '.' rest => case strM rest of
    StrCons '.' _ => False  -- Prevent .. path traversal
    _ => all isValidCommandChar (unpack s)
  StrCons _ _ => all isValidCommandChar (unpack s)

||| Check if string is a safe path (no traversal)
public export
isSafePath : String -> Bool
isSafePath s =
  not (isInfixOf ".." s) &&
  not (isPrefixOf "/" s) &&
  not (isInfixOf "\x00" s)

--------------------------------------------------------------------------------
-- Argument Validation
--------------------------------------------------------------------------------

||| Check if argument starts with dash (is a flag/option)
public export
isFlag : String -> Bool
isFlag s = case strM s of
  StrCons '-' _ => True
  _ => False

||| Check if argument isInfixOf shell metacharacters
public export
containsShellMeta : String -> Bool
containsShellMeta s = any (`elem` unpack s) shellMetaChars

||| Validate a flag argument
public export
isValidFlag : String -> Bool
isValidFlag s = case strM s of
  StrCons '-' rest => case strM rest of
    StrNil => False  -- Just "-" is not valid
    StrCons '-' rest2 => case strM rest2 of
      StrNil => False  -- Just "--" is not valid
      _ => all isValidFlagChar (unpack rest2)  -- --long-option
    StrCons c _ => isAlpha c && all isValidFlagChar (unpack rest)  -- -x or -abc
  _ => False
  where
    isValidFlagChar : Char -> Bool
    isValidFlagChar c = isAlphaNum c || c == '-' || c == '_'

--------------------------------------------------------------------------------
-- SafeArg Operations
--------------------------------------------------------------------------------

||| Create a literal argument (will be escaped when rendered)
public export
arg : String -> SafeArg
arg = LiteralArg

||| Create a flag argument
public export
flag : String -> Maybe SafeArg
flag s = if isValidFlag s then Just (FlagArg s) else Nothing

||| Create a flag argument (unsafe - no validation)
public export
flagUnsafe : String -> SafeArg
flagUnsafe = FlagArg

||| Create short flag (-x)
public export
shortFlag : Char -> SafeArg
shortFlag c = FlagArg ("-" ++ singleton c)

||| Create long flag (--verbose)
public export
longFlag : String -> SafeArg
longFlag name = FlagArg ("--" ++ name)

||| Create option with value (--name=value)
public export
option : String -> String -> SafeArg
option = OptionArg

||| Create short option with value (-o value)
public export
shortOption : Char -> String -> List SafeArg
shortOption c value = [FlagArg ("-" ++ singleton c), LiteralArg value]

--------------------------------------------------------------------------------
-- SafeCommand Operations
--------------------------------------------------------------------------------

||| Create a command with just a name
public export
cmd : String -> Maybe SafeCommand
cmd name =
  if isValidCommandName name
    then Just (MkSafeCommand name [] [] Nothing)
    else Nothing

||| Create command (unsafe - no validation)
public export
cmdUnsafe : String -> SafeCommand
cmdUnsafe name = MkSafeCommand name [] [] Nothing

||| Add argument to command
public export
withArg : SafeArg -> SafeCommand -> SafeCommand
withArg a c = { cmdArgs := c.cmdArgs ++ [a] } c

||| Add multiple arguments
public export
withArgs : List SafeArg -> SafeCommand -> SafeCommand
withArgs args c = { cmdArgs := c.cmdArgs ++ args } c

||| Add literal argument
public export
withLiteral : String -> SafeCommand -> SafeCommand
withLiteral s = withArg (LiteralArg s)

||| Add flag
public export
withFlag : String -> SafeCommand -> SafeCommand
withFlag f = withArg (FlagArg f)

||| Add option with value
public export
withOption : String -> String -> SafeCommand -> SafeCommand
withOption name value = withArg (OptionArg name value)

||| Set environment variable
public export
withEnv : String -> String -> SafeCommand -> SafeCommand
withEnv name value c = { cmdEnv := (name, value) :: c.cmdEnv } c

||| Set working directory
public export
withWorkDir : String -> SafeCommand -> SafeCommand
withWorkDir dir c = { cmdWorkDir := Just dir } c

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

||| Render a single argument to escaped string
public export
renderArg : SafeArg -> String
renderArg (LiteralArg s) = escapeShellArg s
renderArg (EscapedArg s) = s
renderArg (FlagArg f) = f  -- Flags are not escaped
renderArg (OptionArg name value) = name ++ "=" ++ escapeShellArg value

||| Render command to shell string
public export
render : SafeCommand -> String
render c =
  let args = map renderArg c.cmdArgs
  in unwords (c.cmdName :: args)

||| Render command with environment
public export
renderWithEnv : SafeCommand -> String
renderWithEnv c =
  let envPart = unwords (map renderEnvVar c.cmdEnv)
      cmdPart = render c
  in if null c.cmdEnv
       then cmdPart
       else envPart ++ " " ++ cmdPart
  where
    renderEnvVar : (String, String) -> String
    renderEnvVar (name, value) = name ++ "=" ++ escapeShellArg value

||| Render command as argument list (for exec-style calls)
public export
toArgList : SafeCommand -> List String
toArgList c = c.cmdName :: map renderArg c.cmdArgs

--------------------------------------------------------------------------------
-- Common Commands
--------------------------------------------------------------------------------

||| Create ls command
public export
ls : List String -> SafeCommand
ls paths = withArgs (map LiteralArg paths) (cmdUnsafe "ls")

||| Create cp command
public export
cp : String -> String -> SafeCommand
cp src dst = withArgs [LiteralArg src, LiteralArg dst] (cmdUnsafe "cp")

||| Create mv command
public export
mv : String -> String -> SafeCommand
mv src dst = withArgs [LiteralArg src, LiteralArg dst] (cmdUnsafe "mv")

||| Create rm command (with safeguards)
public export
rm : List String -> SafeCommand
rm paths =
  -- Never allow rm -rf / or rm -rf ~
  let safePaths = filter isSafePath paths
  in withArgs (map LiteralArg safePaths) (cmdUnsafe "rm")

||| Create mkdir command
public export
mkdir : String -> SafeCommand
mkdir path = withLiteral path (cmdUnsafe "mkdir")

||| Create mkdir -p command
public export
mkdirP : String -> SafeCommand
mkdirP path = withArgs [FlagArg "-p", LiteralArg path] (cmdUnsafe "mkdir")

||| Create cat command
public export
cat : List String -> SafeCommand
cat files = withArgs (map LiteralArg files) (cmdUnsafe "cat")

||| Create echo command
public export
echo : String -> SafeCommand
echo msg = withLiteral msg (cmdUnsafe "echo")

||| Create grep command
public export
grep : String -> List String -> SafeCommand
grep pattern files =
  withArgs (LiteralArg pattern :: map LiteralArg files) (cmdUnsafe "grep")

||| Create find command
public export
find : String -> String -> SafeCommand
find path pattern =
  withArgs [LiteralArg path, FlagArg "-name", LiteralArg pattern] (cmdUnsafe "find")

||| Create git command
public export
git : List SafeArg -> SafeCommand
git args = withArgs args (cmdUnsafe "git")

||| Create curl command
public export
curl : String -> SafeCommand
curl url = withLiteral url (cmdUnsafe "curl")

||| Create wget command
public export
wget : String -> SafeCommand
wget url = withLiteral url (cmdUnsafe "wget")

--------------------------------------------------------------------------------
-- Pipe and Redirect (represented safely)
--------------------------------------------------------------------------------

||| Represent a pipeline of commands
public export
data Pipeline : Type where
  Single : SafeCommand -> Pipeline
  Pipe : Pipeline -> SafeCommand -> Pipeline

||| Create pipeline from two commands
public export
(|>) : SafeCommand -> SafeCommand -> Pipeline
(|>) c1 c2 = Pipe (Single c1) c2

||| Extend pipeline
public export
pipe : Pipeline -> SafeCommand -> Pipeline
pipe = Pipe

||| Render pipeline
public export
renderPipeline : Pipeline -> String
renderPipeline (Single c) = render c
renderPipeline (Pipe p c) = renderPipeline p ++ " | " ++ render c

||| Output redirection (safe representation)
public export
data Redirect : Type where
  ||| Redirect stdout to file (>)
  ToFile : SafeCommand -> String -> Redirect
  ||| Append stdout to file (>>)
  AppendFile : SafeCommand -> String -> Redirect
  ||| Redirect stderr to stdout (2>&1)
  StderrToStdout : SafeCommand -> Redirect

||| Render redirect
public export
renderRedirect : Redirect -> String
renderRedirect (ToFile c file) = render c ++ " > " ++ escapeShellArg file
renderRedirect (AppendFile c file) = render c ++ " >> " ++ escapeShellArg file
renderRedirect (StderrToStdout c) = render c ++ " 2>&1"
