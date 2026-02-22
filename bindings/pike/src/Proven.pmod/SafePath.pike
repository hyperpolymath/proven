// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafePath.pike - Filesystem traversal prevention for Pike.
//
// All operations delegate to libproven via LibProven. Returns UNDEFINED
// on error.
//
// Usage:
//   import Proven;
//   LibProven.init();
//   if (SafePath.has_traversal("../../etc/passwd"))
//       write("Traversal attack detected!\n");
//   LibProven.deinit();

//! @class SafePath
//! Filesystem path safety operations.
//!
//! Detects directory traversal attacks and sanitizes filenames to prevent
//! path manipulation vulnerabilities.

protected LibProven lib = LibProven();

//! @decl int(0..1)|zero has_traversal(string path)
//! Check if a path contains directory traversal sequences ("..").
//! @returns
//!   @expr{1@} if traversal detected, @expr{0@} if safe,
//!   @expr{UNDEFINED@} on error.
int(0..1)|zero has_traversal(string path)
{
    return lib->call_bool("path_has_traversal", ({path}));
}

//! @decl string|zero sanitize_filename(string name)
//! Sanitize a filename by removing dangerous characters.
//! @returns
//!   Sanitized filename, or @expr{UNDEFINED@} on error.
string|zero sanitize_filename(string name)
{
    return lib->call_string("path_sanitize_filename", ({name}));
}
