// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeFile - Safe file operations with path validation and resource protection.
 *
 * Provides safe file I/O operations with:
 * - Path traversal attack prevention
 * - Bounded read operations to prevent memory exhaustion
 * - Proper error handling without crashes
 */

// ============================================================================
// External bindings for Node.js fs module
// ============================================================================

/** Node.js fs module binding */
@module("fs") external readFileSyncInternal: (string, string) => string = "readFileSync"
@module("fs") external writeFileSyncInternal: (string, string, string) => unit = "writeFileSync"
@module("fs") external appendFileSyncInternal: (string, string, string) => unit = "appendFileSync"
@module("fs") external existsSyncInternal: string => bool = "existsSync"
@module("fs") external unlinkSyncInternal: string => unit = "unlinkSync"
@module("fs") external renameSyncInternal: (string, string) => unit = "renameSync"
@module("fs") external copyFileSyncInternal: (string, string) => unit = "copyFileSync"
@module("fs") external mkdirSyncInternal: (string, 'options) => unit = "mkdirSync"
@module("fs") external rmdirSyncInternal: (string, 'options) => unit = "rmSync"

/** fs.statSync returns an object with size and other properties */
type statResult = {
  size: float,
  isFile: unit => bool,
  isDirectory: unit => bool,
}
@module("fs") external statSyncInternal: string => statResult = "statSync"

/** path module bindings */
@module("path") external pathJoin: (string, string) => string = "join"
@module("path") external pathBasename: string => string = "basename"
@module("path") external pathDirname: string => string = "dirname"
@module("path") external pathExtname: string => string = "extname"
@module("path") external pathNormalize: string => string = "normalize"
@module("path") external pathIsAbsolute: string => bool = "isAbsolute"

// ============================================================================
// Error types
// ============================================================================

/** Error types for file operations */
type fileError =
  | TraversalDetected
  | PathTooLong
  | FileTooLarge
  | ReadError
  | WriteError
  | OpenError
  | NotFound
  | PermissionDenied
  | InvalidPath
  | IsDirectory
  | NotAFile

// ============================================================================
// Constants
// ============================================================================

/** Maximum allowed path length */
let maxPathLength = 4096

/** Default maximum file size for bounded reads (16 MiB) */
let defaultMaxFileSize = 16 * 1024 * 1024

// ============================================================================
// Path validation
// ============================================================================

/** Check if a path contains directory traversal sequences */
let hasTraversal = (path: string): bool => {
  // Check for ".." sequence
  if Js.String2.includes(path, "..") {
    true
  } else if Js.String2.length(path) > 0 && Js.String2.charAt(path, 0) == "~" {
    // Check for home directory expansion
    true
  } else {
    false
  }
}

/** Check if a path is safe (no traversal attacks, within length limits) */
let isSafePath = (path: string): bool => {
  let len = Js.String2.length(path)
  if len == 0 || len > maxPathLength {
    false
  } else if hasTraversal(path) {
    false
  } else {
    // Check for null bytes
    !Js.String2.includes(path, "\x00")
  }
}

/** Validate a path and return it if safe, otherwise return error */
let validatePath = (path: string): result<string, fileError> => {
  let len = Js.String2.length(path)
  if len == 0 {
    Error(InvalidPath)
  } else if len > maxPathLength {
    Error(PathTooLong)
  } else if hasTraversal(path) {
    Error(TraversalDetected)
  } else if Js.String2.includes(path, "\x00") {
    Error(InvalidPath)
  } else {
    Ok(path)
  }
}

/** Sanitize a filename by removing dangerous characters */
let sanitizeFilename = (filename: string): string => {
  filename
  ->Js.String2.replaceByRe(%re("/\\.\\./g"), "_")
  ->Js.String2.replaceByRe(%re("/[\\/\\\\]/g"), "_")
  ->Js.String2.replaceByRe(%re("/[\\x00-\\x1f]/g"), "")
  ->Js.String2.replaceByRe(%re("/[<>:\"\\|\\?\\*]/g"), "_")
}

/** Safely join base path with filename, rejecting traversal attempts */
let safePath = (base: string, filename: string): result<string, fileError> => {
  if hasTraversal(filename) {
    Error(TraversalDetected)
  } else {
    let sanitized = sanitizeFilename(filename)
    let joined = pathJoin(base, sanitized)
    let len = Js.String2.length(joined)
    if len > maxPathLength {
      Error(PathTooLong)
    } else {
      Ok(joined)
    }
  }
}

/** Safely join multiple path components */
let safeJoin = (base: string, parts: array<string>): result<string, fileError> => {
  let hasUnsafe = parts->Belt.Array.some(part => hasTraversal(part))
  if hasUnsafe {
    Error(TraversalDetected)
  } else {
    let result = ref(base)
    parts->Belt.Array.forEach(part => {
      let sanitized = sanitizeFilename(part)
      result := pathJoin(result.contents, sanitized)
    })
    let len = Js.String2.length(result.contents)
    if len > maxPathLength {
      Error(PathTooLong)
    } else {
      Ok(result.contents)
    }
  }
}

// ============================================================================
// File reading operations
// ============================================================================

/** Read entire file contents with a maximum size limit */
let readFileBounded = (path: string, maxSize: int): result<string, fileError> => {
  switch validatePath(path) {
  | Error(e) => Error(e)
  | Ok(_) =>
    try {
      // Check file size first
      let stat = statSyncInternal(path)
      if stat.size > Belt.Int.toFloat(maxSize) {
        Error(FileTooLarge)
      } else if !stat.isFile() {
        Error(NotAFile)
      } else {
        let contents = readFileSyncInternal(path, "utf8")
        Ok(contents)
      }
    } catch {
    | Js.Exn.Error(e) =>
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("")
      if Js.String2.includes(message, "ENOENT") {
        Error(NotFound)
      } else if Js.String2.includes(message, "EACCES") || Js.String2.includes(message, "EPERM") {
        Error(PermissionDenied)
      } else if Js.String2.includes(message, "EISDIR") {
        Error(IsDirectory)
      } else {
        Error(ReadError)
      }
    | _ => Error(ReadError)
    }
  }
}

/** Read entire file with default size limit */
let readFile = (path: string): result<string, fileError> => {
  readFileBounded(path, defaultMaxFileSize)
}

/** Read file as lines with bounded total size */
let readLines = (path: string, maxSize: int): result<array<string>, fileError> => {
  switch readFileBounded(path, maxSize) {
  | Error(e) => Error(e)
  | Ok(contents) => Ok(Js.String2.split(contents, "\n"))
  }
}

/** Read file as lines with default size limit */
let readLinesDefault = (path: string): result<array<string>, fileError> => {
  readLines(path, defaultMaxFileSize)
}

// ============================================================================
// File writing operations
// ============================================================================

/** Write data to a file safely */
let writeFile = (path: string, data: string): result<unit, fileError> => {
  switch validatePath(path) {
  | Error(e) => Error(e)
  | Ok(_) =>
    try {
      writeFileSyncInternal(path, data, "utf8")
      Ok()
    } catch {
    | Js.Exn.Error(e) =>
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("")
      if Js.String2.includes(message, "EACCES") || Js.String2.includes(message, "EPERM") {
        Error(PermissionDenied)
      } else if Js.String2.includes(message, "EISDIR") {
        Error(IsDirectory)
      } else {
        Error(WriteError)
      }
    | _ => Error(WriteError)
    }
  }
}

/** Append data to a file safely */
let appendFile = (path: string, data: string): result<unit, fileError> => {
  switch validatePath(path) {
  | Error(e) => Error(e)
  | Ok(_) =>
    try {
      appendFileSyncInternal(path, data, "utf8")
      Ok()
    } catch {
    | Js.Exn.Error(e) =>
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("")
      if Js.String2.includes(message, "ENOENT") {
        Error(NotFound)
      } else if Js.String2.includes(message, "EACCES") || Js.String2.includes(message, "EPERM") {
        Error(PermissionDenied)
      } else if Js.String2.includes(message, "EISDIR") {
        Error(IsDirectory)
      } else {
        Error(WriteError)
      }
    | _ => Error(WriteError)
    }
  }
}

/** Write lines to a file */
let writeLines = (path: string, lines: array<string>): result<unit, fileError> => {
  let content = Js.Array2.joinWith(lines, "\n")
  writeFile(path, content)
}

// ============================================================================
// File existence and metadata
// ============================================================================

/** Check if a file exists and is a regular file (not a directory) */
let fileExists = (path: string): bool => {
  switch validatePath(path) {
  | Error(_) => false
  | Ok(_) =>
    try {
      let stat = statSyncInternal(path)
      stat.isFile()
    } catch {
    | _ => false
    }
  }
}

/** Check if a directory exists */
let directoryExists = (path: string): bool => {
  switch validatePath(path) {
  | Error(_) => false
  | Ok(_) =>
    try {
      let stat = statSyncInternal(path)
      stat.isDirectory()
    } catch {
    | _ => false
    }
  }
}

/** Check if a path exists (file or directory) */
let pathExists = (path: string): bool => {
  switch validatePath(path) {
  | Error(_) => false
  | Ok(_) => existsSyncInternal(path)
  }
}

/** Get file size safely, returning None if file doesn't exist or is inaccessible */
let getFileSize = (path: string): option<int> => {
  switch validatePath(path) {
  | Error(_) => None
  | Ok(_) =>
    try {
      let stat = statSyncInternal(path)
      if stat.isFile() {
        Some(Belt.Float.toInt(stat.size))
      } else {
        None
      }
    } catch {
    | _ => None
    }
  }
}

// ============================================================================
// File manipulation operations
// ============================================================================

/** Copy a file safely with path validation */
let copyFile = (srcPath: string, destPath: string): result<unit, fileError> => {
  switch (validatePath(srcPath), validatePath(destPath)) {
  | (Error(e), _) | (_, Error(e)) => Error(e)
  | (Ok(_), Ok(_)) =>
    try {
      copyFileSyncInternal(srcPath, destPath)
      Ok()
    } catch {
    | Js.Exn.Error(e) =>
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("")
      if Js.String2.includes(message, "ENOENT") {
        Error(NotFound)
      } else if Js.String2.includes(message, "EACCES") || Js.String2.includes(message, "EPERM") {
        Error(PermissionDenied)
      } else if Js.String2.includes(message, "EISDIR") {
        Error(IsDirectory)
      } else {
        Error(WriteError)
      }
    | _ => Error(WriteError)
    }
  }
}

/** Delete a file safely with path validation.
 * Returns success even if file doesn't exist (idempotent).
 */
let deleteFile = (path: string): result<unit, fileError> => {
  switch validatePath(path) {
  | Error(e) => Error(e)
  | Ok(_) =>
    try {
      unlinkSyncInternal(path)
      Ok()
    } catch {
    | Js.Exn.Error(e) =>
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("")
      if Js.String2.includes(message, "ENOENT") {
        // Idempotent delete - file already gone is success
        Ok()
      } else if Js.String2.includes(message, "EACCES") || Js.String2.includes(message, "EPERM") {
        Error(PermissionDenied)
      } else if Js.String2.includes(message, "EISDIR") {
        Error(IsDirectory)
      } else {
        Error(WriteError)
      }
    | _ => Error(WriteError)
    }
  }
}

/** Rename/move a file safely with path validation */
let renameFile = (oldPath: string, newPath: string): result<unit, fileError> => {
  switch (validatePath(oldPath), validatePath(newPath)) {
  | (Error(e), _) | (_, Error(e)) => Error(e)
  | (Ok(_), Ok(_)) =>
    try {
      renameSyncInternal(oldPath, newPath)
      Ok()
    } catch {
    | Js.Exn.Error(e) =>
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("")
      if Js.String2.includes(message, "ENOENT") {
        Error(NotFound)
      } else if Js.String2.includes(message, "EACCES") || Js.String2.includes(message, "EPERM") {
        Error(PermissionDenied)
      } else if Js.String2.includes(message, "EISDIR") {
        Error(IsDirectory)
      } else {
        Error(WriteError)
      }
    | _ => Error(WriteError)
    }
  }
}

/** Move a file (alias for renameFile) */
let moveFile = renameFile

// ============================================================================
// Directory operations
// ============================================================================

/** Create a directory with path validation */
let createDirectory = (path: string): result<unit, fileError> => {
  switch validatePath(path) {
  | Error(e) => Error(e)
  | Ok(_) =>
    try {
      mkdirSyncInternal(path, {"recursive": true})
      Ok()
    } catch {
    | Js.Exn.Error(e) =>
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("")
      if Js.String2.includes(message, "EACCES") || Js.String2.includes(message, "EPERM") {
        Error(PermissionDenied)
      } else {
        Error(WriteError)
      }
    | _ => Error(WriteError)
    }
  }
}

/** Remove a directory with path validation (must be empty unless recursive) */
let removeDirectory = (path: string, ~recursive: bool=false): result<unit, fileError> => {
  switch validatePath(path) {
  | Error(e) => Error(e)
  | Ok(_) =>
    try {
      rmdirSyncInternal(path, {"recursive": recursive, "force": recursive})
      Ok()
    } catch {
    | Js.Exn.Error(e) =>
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("")
      if Js.String2.includes(message, "ENOENT") {
        // Idempotent - directory already gone is success
        Ok()
      } else if Js.String2.includes(message, "EACCES") || Js.String2.includes(message, "EPERM") {
        Error(PermissionDenied)
      } else {
        Error(WriteError)
      }
    | _ => Error(WriteError)
    }
  }
}

// ============================================================================
// Path utilities
// ============================================================================

/** Get the base name of a path */
let basename = (path: string): option<string> => {
  switch validatePath(path) {
  | Error(_) => None
  | Ok(_) => Some(pathBasename(path))
  }
}

/** Get the directory name of a path */
let dirname = (path: string): option<string> => {
  switch validatePath(path) {
  | Error(_) => None
  | Ok(_) => Some(pathDirname(path))
  }
}

/** Get the extension of a path */
let extname = (path: string): option<string> => {
  switch validatePath(path) {
  | Error(_) => None
  | Ok(_) => Some(pathExtname(path))
  }
}

/** Normalize a path */
let normalize = (path: string): result<string, fileError> => {
  switch validatePath(path) {
  | Error(e) => Error(e)
  | Ok(_) =>
    let normalized = pathNormalize(path)
    // Re-validate after normalization
    if hasTraversal(normalized) {
      Error(TraversalDetected)
    } else {
      Ok(normalized)
    }
  }
}

/** Check if a path is absolute */
let isAbsolute = (path: string): bool => {
  switch validatePath(path) {
  | Error(_) => false
  | Ok(_) => pathIsAbsolute(path)
  }
}

// ============================================================================
// Safe file operations with rollback
// ============================================================================

/** Atomically write a file by writing to a temp file first, then renaming */
let writeFileAtomic = (path: string, data: string): result<unit, fileError> => {
  let tempPath = path ++ ".tmp." ++ Belt.Float.toString(Js.Date.now())
  switch writeFile(tempPath, data) {
  | Error(e) => Error(e)
  | Ok(_) =>
    switch renameFile(tempPath, path) {
    | Error(e) =>
      // Try to clean up temp file
      let _ = deleteFile(tempPath)
      Error(e)
    | Ok(_) => Ok()
    }
  }
}

/** Read a file, returning empty string if not found */
let readFileOrEmpty = (path: string): string => {
  switch readFile(path) {
  | Ok(contents) => contents
  | Error(_) => ""
  }
}

/** Read a file, returning default value if not found or on error */
let readFileOrDefault = (path: string, default: string): string => {
  switch readFile(path) {
  | Ok(contents) => contents
  | Error(_) => default
  }
}

/** Ensure a file exists, creating it with default content if not */
let ensureFile = (path: string, defaultContent: string): result<unit, fileError> => {
  if fileExists(path) {
    Ok()
  } else {
    writeFile(path, defaultContent)
  }
}

/** Ensure a directory exists, creating it if not */
let ensureDirectory = (path: string): result<unit, fileError> => {
  if directoryExists(path) {
    Ok()
  } else {
    createDirectory(path)
  }
}
