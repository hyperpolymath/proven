// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeCommand - Safe command execution with input sanitization and injection prevention.
 *
 * Provides command execution utilities that sanitize inputs to prevent
 * shell injection attacks. All functions validate inputs before execution
 * and return explicit error types on failure.
 */

/** Command error types */
type commandError =
  | InvalidCommand
  | InvalidArgument
  | DangerousCharacter
  | CommandTooLong
  | EmptyCommand
  | PathTraversal
  | NullByteDetected

/** Maximum allowed command length */
let maxCommandLength = 8192

/** Maximum allowed argument length */
let maxArgumentLength = 4096

/** Dangerous shell characters */
let dangerousShellChars = [";", "&", "|", "$", "`", "(", ")", "{", "}", "<", ">", "\n", "\r", "!", "#", "~"]

/** Characters that require quoting but are not dangerous */
let requiresQuotingChars = [" ", "\t", "\"", "'", "*", "?", "[", "]", "\\"]

/** Check if a character is dangerous for shell execution */
let isDangerousChar = (char: string): bool => {
  Js.Array2.includes(dangerousShellChars, char)
}

/** Check if a character requires quoting */
let requiresQuoting = (char: string): bool => {
  isDangerousChar(char) || Js.Array2.includes(requiresQuotingChars, char)
}

/** Check if a string contains any dangerous shell characters */
let containsDangerousChars = (input: string): bool => {
  let chars = Js.String2.split(input, "")
  Js.Array2.some(chars, char => isDangerousChar(char) || char == "\x00")
}

/** Check if a string contains a null byte */
let containsNullByte = (input: string): bool => {
  Js.String2.includes(input, "\x00")
}

/** Check for path traversal sequences */
let hasPathTraversal = (input: string): bool => {
  Js.String2.includes(input, "..")
}

/** Validate a command name (no path traversal, no dangerous chars) */
let validateCommandName = (command: string): result<unit, commandError> => {
  let len = Js.String2.length(command)
  if len == 0 {
    Error(EmptyCommand)
  } else if len > maxCommandLength {
    Error(CommandTooLong)
  } else if containsNullByte(command) {
    Error(NullByteDetected)
  } else if containsDangerousChars(command) {
    Error(DangerousCharacter)
  } else if hasPathTraversal(command) {
    Error(PathTraversal)
  } else {
    Ok()
  }
}

/** Validate a command argument */
let validateArgument = (arg: string): result<unit, commandError> => {
  let len = Js.String2.length(arg)
  if len > maxArgumentLength {
    Error(InvalidArgument)
  } else if containsNullByte(arg) {
    Error(NullByteDetected)
  } else if containsDangerousChars(arg) {
    Error(DangerousCharacter)
  } else {
    Ok()
  }
}

/** Sanitize a string for safe shell usage by escaping special characters */
let sanitizeForShell = (input: string): result<string, commandError> => {
  if containsNullByte(input) {
    Error(NullByteDetected)
  } else {
    let chars = Js.String2.split(input, "")
    let sanitized = Js.Array2.map(chars, char => {
      if isDangerousChar(char) {
        "_"
      } else if requiresQuoting(char) {
        "\\" ++ char
      } else {
        char
      }
    })
    Ok(Js.Array2.joinWith(sanitized, ""))
  }
}

/** Quote a string for safe shell usage using single quotes */
let quoteForShell = (input: string): result<string, commandError> => {
  if containsNullByte(input) {
    Error(NullByteDetected)
  } else {
    let chars = Js.String2.split(input, "")
    let escaped = Js.Array2.map(chars, char => {
      if char == "'" {
        "'\\''"
      } else {
        char
      }
    })
    Ok("'" ++ Js.Array2.joinWith(escaped, "") ++ "'")
  }
}

/** Validate an array of arguments */
let validateArguments = (args: array<string>): result<unit, commandError> => {
  let results = Js.Array2.map(args, validateArgument)
  let firstError = Js.Array2.find(results, r =>
    switch r {
    | Error(_) => true
    | Ok(_) => false
    }
  )
  switch firstError {
  | Some(Error(e)) => Error(e)
  | _ => Ok()
  }
}

/** Build a safe command string from validated components */
let buildSafeCommand = (command: string, args: array<string>): result<string, commandError> => {
  switch validateCommandName(command) {
  | Error(e) => Error(e)
  | Ok(_) =>
    switch validateArguments(args) {
    | Error(e) => Error(e)
    | Ok(_) =>
      let quotedArgs = Js.Array2.map(args, arg => {
        switch quoteForShell(arg) {
        | Ok(quoted) => quoted
        | Error(_) => "" // Should not happen since we validated
        }
      })
      let argString = Js.Array2.joinWith(quotedArgs, " ")
      if Js.String2.length(argString) > 0 {
        Ok(command ++ " " ++ argString)
      } else {
        Ok(command)
      }
    }
  }
}

/** Check if a command path is absolute */
let isAbsolutePath = (path: string): bool => {
  Js.String2.length(path) > 0 && Js.String2.charAt(path, 0) == "/"
}

/** Validate that a command exists as an absolute path (basic check) */
let validateAbsoluteCommand = (path: string): result<unit, commandError> => {
  if !isAbsolutePath(path) {
    Error(InvalidCommand)
  } else {
    validateCommandName(path)
  }
}

/** Split a command string into command and arguments (simple split by spaces) */
let splitCommand = (commandLine: string): result<{command: string, args: array<string>}, commandError> => {
  let trimmed = Js.String2.trim(commandLine)
  if Js.String2.length(trimmed) == 0 {
    Error(EmptyCommand)
  } else if containsNullByte(trimmed) {
    Error(NullByteDetected)
  } else {
    let parts = Js.Array2.filter(Js.String2.split(trimmed, " "), part => Js.String2.length(part) > 0)
    if Js.Array2.length(parts) == 0 {
      Error(EmptyCommand)
    } else {
      let command = Belt.Array.getUnsafe(parts, 0)
      let args = Js.Array2.sliceFrom(parts, 1)
      Ok({command, args})
    }
  }
}

/** Allowed command configuration for whitelisting */
type allowedCommand = {
  name: string,
  allowedArgsPatterns: option<array<string>>,
  requireAbsolutePath: bool,
}

/** Check if a command is in the allowed list */
let isCommandAllowed = (command: string, allowedCommands: array<allowedCommand>): bool => {
  Js.Array2.some(allowedCommands, allowed => allowed.name == command)
}

/** Get allowed command config if command is in list */
let getAllowedCommandConfig = (command: string, allowedCommands: array<allowedCommand>): option<allowedCommand> => {
  Js.Array2.find(allowedCommands, allowed => allowed.name == command)
}

/** Create a command allowlist */
let createAllowlist = (commands: array<string>): array<allowedCommand> => {
  Js.Array2.map(commands, name => {
    name,
    allowedArgsPatterns: None,
    requireAbsolutePath: false,
  })
}

/** Common safe commands allowlist */
let commonSafeCommands = createAllowlist(["ls", "cat", "head", "tail", "wc", "grep", "find", "which", "echo", "date", "pwd", "whoami"])

/** Validate command against allowlist */
let validateAgainstAllowlist = (
  command: string,
  allowedCommands: array<allowedCommand>,
): result<unit, commandError> => {
  if isCommandAllowed(command, allowedCommands) {
    Ok()
  } else {
    Error(InvalidCommand)
  }
}

/** Escape a single argument for use in array-based exec (not shell) */
let escapeArgument = (arg: string): result<string, commandError> => {
  if containsNullByte(arg) {
    Error(NullByteDetected)
  } else {
    // For array-based exec, we just need to avoid null bytes
    Ok(arg)
  }
}

/** Build argument array for safe exec (not shell) */
let buildExecArgs = (args: array<string>): result<array<string>, commandError> => {
  if Js.Array2.some(args, containsNullByte) {
    Error(NullByteDetected)
  } else {
    Ok(args)
  }
}

/** Check if input looks like a shell metacharacter injection attempt */
let looksLikeInjection = (input: string): bool => {
  // Check for common injection patterns
  let injectionPatterns = [
    "$(", // Command substitution
    "`", // Backtick command substitution
    "&&", // Command chaining
    "||", // Command chaining
    ";", // Command separator
    "|", // Pipe
    ">", // Redirect
    "<", // Redirect
    "${", // Variable expansion
    "$()", // Command substitution empty
  ]
  Js.Array2.some(injectionPatterns, pattern => Js.String2.includes(input, pattern))
}

/** Sanitize filename component (remove path separators and dangerous chars) */
let sanitizeFilename = (filename: string): string => {
  filename
  ->Js.String2.replaceByRe(%re("/\\.\\./g"), "_")
  ->Js.String2.replaceByRe(%re("/[\\/\\\\]/g"), "_")
  ->Js.String2.replaceByRe(%re("/[\\x00-\\x1f]/g"), "")
  ->Js.String2.replaceByRe(%re("/[;|&$`<>(){}!#~]/g"), "_")
}

/** Create a safe environment variable name */
let sanitizeEnvVarName = (name: string): result<string, commandError> => {
  if Js.String2.length(name) == 0 {
    Error(InvalidArgument)
  } else if containsNullByte(name) {
    Error(NullByteDetected)
  } else {
    // Env var names should be alphanumeric + underscore, starting with letter or underscore
    let sanitized =
      name
      ->Js.String2.replaceByRe(%re("/[^a-zA-Z0-9_]/g"), "_")
      ->Js.String2.replaceByRe(%re("/^[0-9]/"), "_$&")
    Ok(sanitized)
  }
}
