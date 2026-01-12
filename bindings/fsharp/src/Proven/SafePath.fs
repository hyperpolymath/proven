// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe filesystem path operations with traversal protection.
module SafePath =
    open System
    open System.IO

    /// Result of a path operation.
    type PathResult =
        | Ok of string
        | Error of string

    let private dangerousChars = [| '<'; '>'; ':'; '"'; '/'; '\\'; '|'; '?'; '*' |]

    /// Check if a path contains directory traversal sequences.
    let hasTraversal (path: string) : bool =
        path.Contains("..") || path.StartsWith("~")

    /// Check if a path is safe (no traversal).
    let isSafePath (path: string) : bool =
        not (hasTraversal path)

    /// Sanitize a filename by removing dangerous characters.
    let sanitizeFilename (filename: string) : string =
        let mutable safe = filename.Replace("..", "_")
        for c in dangerousChars do
            safe <- safe.Replace(string c, "_")
        safe <- safe.Replace("\u0000", "_")
        safe <- safe.TrimStart('.', ' ').TrimEnd('.', ' ')
        safe

    /// Safely join path components, checking for traversal.
    let join (base': string) (parts: string list) : PathResult =
        let sep = Path.DirectorySeparatorChar
        let mutable path = base'.TrimEnd(sep)
        for part in parts do
            if hasTraversal part then
                Error "traversal_detected" |> ignore
            else
                let safePart = sanitizeFilename part
                path <- sprintf "%s%c%s" path sep safePart
        // Check if any part had traversal
        if parts |> List.exists hasTraversal then
            Error "traversal_detected"
        else
            Ok path

    /// Resolve path and verify it's within a base directory.
    let resolveWithin (basePath: string) (userPath: string) : PathResult =
        try
            let realBase = Path.GetFullPath(basePath)
            let fullPath = Path.Combine(realBase, userPath)
            let resolved = Path.GetFullPath(fullPath)

            if resolved.StartsWith(realBase, StringComparison.OrdinalIgnoreCase) then
                Ok resolved
            else
                Error "path_escapes_base"
        with
        | _ -> Error "resolution_failed"

    /// Get safe basename (strip directory components).
    let safeBasename (path: string) : string =
        Path.GetFileName(path) |> sanitizeFilename

    /// Check if filename has an allowed extension.
    let hasAllowedExtension (filename: string) (allowedExtensions: string list) : bool =
        let ext = Path.GetExtension(filename)
        if String.IsNullOrEmpty(ext) then false
        else
            let ext = ext.TrimStart('.').ToLowerInvariant()
            allowedExtensions |> List.exists (fun allowed ->
                allowed.ToLowerInvariant() = ext)

    /// Get file extension (lowercase).
    let getExtension (filename: string) : string option =
        let ext = Path.GetExtension(filename)
        if String.IsNullOrEmpty(ext) || ext = "." then None
        else Some(ext.TrimStart('.').ToLowerInvariant())

    /// Check if path is absolute.
    let isAbsolutePath (path: string) : bool =
        Path.IsPathRooted(path)

    /// Check if path exists and is readable.
    let isReadable (path: string) : bool =
        try
            File.Exists(path) || Directory.Exists(path)
        with
        | _ -> false

    /// Check if path is a regular file (not symlink, device, etc).
    let isRegularFile (path: string) : bool =
        try
            File.Exists(path)
        with
        | _ -> false

    /// Check if path is a directory.
    let isDirectory (path: string) : bool =
        try
            Directory.Exists(path)
        with
        | _ -> false

    /// Create directory safely (checks for traversal first).
    let safeMkdir (base': string) (name: string) : bool =
        if hasTraversal name then false
        else
            try
                let safeName = sanitizeFilename name
                let fullPath = Path.Combine(base', safeName)
                Directory.CreateDirectory(fullPath) |> ignore
                true
            with
            | _ -> false

    /// Normalize a path.
    let normalizePath (path: string) : string =
        Path.GetFullPath(path)
